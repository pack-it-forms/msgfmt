{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : MsgFmt
Description : Raw pack-it-forms message handling
Copyright   : (c) 2015 Peter Amidon <peter@picnicpark.org>
            : (c) 2015 Keith Amidon <amidon@picnicpark.org>

Generic handling of pack-it-forms encoded forms, which are also
compatible with PacFORMS encoded forms.  Forms consist of a set of key
value pairs representing the form fields.  All field names and values
are strings to allow this module to provide basic functionality for
any form.  It is expected that type-safe modules will be built on top
of this module for specific forms that require automated processing.
-}

module PackItForms.MsgFmt
( parse
, parseFile
, getValue
, getEnvVal
, getText
, getMap
, emptyRep
, fromList
, insertKV
, insertEnvKV
, insertAll
, insertEnvAll
, MsgFmt(..)
) where

import System.IO
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import Data.String.Utils

-- | Representation of a message
data MsgFmt = MsgFmt { envelope :: M.Map String String
                     , fields :: M.Map String String
                     , text ::  T.Text
                     } deriving (Show, Eq)

-- | Accessor for value for a field in the message
getValue :: MsgFmt -> String -> Maybe String
getValue (MsgFmt _ m _) k = M.lookup k m

-- | Accessor for the encoded form message text
getText :: MsgFmt -> T.Text
getText (MsgFmt _ _ s) = s

-- | Accessor to allow access to all of the fields as a map
getMap (MsgFmt _ m _) = m

-- | Accessor for the value of an envelope field
getEnvVal :: MsgFmt -> String -> Maybe String
getEnvVal (MsgFmt e _ _) k = M.lookup k e

-- | Create an empty message
emptyRep :: MsgFmt
emptyRep = MsgFmt M.empty M.empty ""

-- | Create a message from a list of field key/value pairs and a list of envelope key/value pairs
fromList :: [(String, String)] -> [(String, String)] -> MsgFmt
fromList e v = insertEnvAll (insertAll emptyRep v) e

-- | Add a field key/value pair to a message
insertKV :: MsgFmt -> String -> String ->  MsgFmt
insertKV (MsgFmt e m s) k v =
  let nis = encodeKV (quoteKey k) (quoteValue v)
   in MsgFmt e (M.insert k v m) (T.append s nis)

-- | Add a key/value pair to a message envelope
insertEnvKV :: MsgFmt -> String -> String -> MsgFmt
insertEnvKV m k v = insertEnvAll m [(k, v)]

-- Backtick escape invalid characters in encoded field names
quoteKey :: String -> String
quoteKey = backtickQuote "`:#!"

-- Backtick escape invalid characters in encoded field values
quoteValue :: String -> String
quoteValue = backtickQuote "`]#!"

-- Backtick quoting function
backtickQuote :: String -> String -> String
backtickQuote d = foldr step ""
  where step y acc | y `elem` d = '`':y:acc
                   | otherwise = y:acc

-- Encode a key/value pair in the text representation
encodeKV     :: String -> String -> T.Text
encodeKV k v = T.pack $ k ++ ": [" ++ v ++ "]\n"

-- | Add a list of field key/value pairs to message
insertAll :: MsgFmt -> [(String, String)] -> MsgFmt
insertAll = foldl $ uncurry . insertKV

-- | Add a key/value pair to the envelope of the message
insertEnvAll :: MsgFmt -> [(String, String)] -> MsgFmt
insertEnvAll (MsgFmt e m s) kv =
  let nEnv = M.union (M.fromList kv) e
      lines' = lines . T.unpack $ s
      ns = if any isEnvLine lines'
              then T.pack . unlines . map (regenEnv nEnv) $ lines'
              else T.pack . unlines . ((genEnv nEnv++"\n"):) $ lines'
  in MsgFmt nEnv m ns
  where regenEnv :: M.Map String String -> String -> String
        regenEnv e s | isEnvLine s = genEnv e
                     | otherwise = s
        isEnvLine = startswith "!OUTPOST! "
        genEnv :: M.Map String String -> String
        genEnv e = "!OUTPOST! " ++ serializeEnv e
        serializeEnv :: M.Map String String -> String
        serializeEnv e = drop 2 $ M.foldrWithKey strEntry "" e
        strEntry :: String -> String -> String -> String
        strEntry k v s = s ++ ", " ++ k ++ "=" ++ v

-- | Parse an encoded form message to a MsgFmt
--
-- Delimiters are considered :, [ and ].  : ends a key, [ starts a
-- value, and ] ends a value.  In all components, a backtick followed
-- by a character is an escape for that character, and can be used to
-- escape that character, so k`:ey is a valid key that would parse to
-- be named "k:ey".  Anything between delimiters is ignored; notably,
-- any characters between the colon that ends a key and the first
-- unescaped begin bracket will be ignored.
parse   :: String -> MsgFmt
parse p = MsgFmt (parseEnv p) (parseMap M.empty "" $ stripUnnecessary p) $ T.pack p

-- Skip comment and directive lines
stripUnnecessary :: String -> String
stripUnnecessary = unlines . filter pred . lines
  where pred = maybe True notComment . listToMaybe
        notComment = not . (`elem` ("#!" ::String))

-- Create map of key/value pairs for each field
parseMap :: M.Map String String -> String -> String -> M.Map String String
parseMap parsed _ ""     = parsed
parseMap parsed key string = if key == ""
                                then let (k,ks) = splitEF ':' string
                                     in parseMap parsed (stripKey k) ks
                                else let (_,v) = splitEF '[' string
                                         (nv,vs) = splitEF ']' v
                                         submap = parseMap parsed "" vs
                                     in M.insert key nv submap
  -- stripKey is used to get rid of the newline that might be at the
  -- beginning of some keys
  where stripKey ('\n':xs) = xs
        stripKey x = x

parseEnv :: String -> M.Map String String
parseEnv msg = maybe M.empty parseEnvLine line
  where line :: Maybe String
        line = listToMaybe . filter (startswith "!OUTPOST! ") . lines $ msg
        parseEnvLine :: String -> M.Map String String
        parseEnvLine l = M.fromList . mapMaybe mkKV . split "," $ drop 10 l
        mkKV :: String -> Maybe (String, String)
        mkKV s = let strs = split "=" s
                 in if length strs == 2
                       then Just (lstrip $ head strs, strs !! 1)
                       else Nothing

-- Split keys from values
splitEF :: Char -> String -> (String, String)
splitEF = splitEF' False

-- Worker for splitting keys and values dequoting backticks
splitEF' :: Bool -> Char -> String -> (String,String)
splitEF' _ _ []           = ("","")
splitEF' False d ('`':xs) = let rest = splitEF' True  d xs
                            in rest
splitEF' b d (x:xs)
  | not b && d == x       = ("",xs)
  | otherwise             = let rest = splitEF' False d xs
                            in (x : fst rest, snd rest)


-- | Parse the encoded form message in a file into a MsgFmt
parseFile   :: String -> IO MsgFmt
parseFile f = do
    s <- readFile f
    return $ parse s
