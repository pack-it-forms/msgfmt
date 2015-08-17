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
, getText
, getMap
, emptyRep
, fromList
, insertKV
, insertAll
, MsgFmt(..)
) where

import System.IO
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import Data.String.Utils

-- | Representation of a message
data MsgFmt = MsgFmt (M.Map String String) T.Text
  deriving (Show, Eq)

-- | Accessor for value for a field in the message
getValue :: MsgFmt -> String -> Maybe String
getValue (MsgFmt m _) k = M.lookup k m

-- | Accessor for the encoded form message text
getText :: MsgFmt -> T.Text
getText (MsgFmt _ s) = s

-- | Accessor to allow access to all of the fields as a map
getMap (MsgFmt m _) = m

-- | Create an empty message
emptyRep :: MsgFmt
emptyRep = MsgFmt M.empty ""

-- | Create a message from a list of field key/value pairs
fromList :: [(String, String)] -> MsgFmt
fromList = insertAll emptyRep

-- | Add a field key/value pair to a message
insertKV :: MsgFmt -> String -> String ->  MsgFmt
insertKV (MsgFmt m s) k v =
  let nis = encodeKV (quoteKey k) (quoteValue v)
   in MsgFmt (M.insert k v m) (T.append s nis)

-- Backtick escape invalid characters in encoded field names
quoteKey   :: String -> String
quoteKey s = backtickQuote "`:#!" s

-- Backtick escape invalid characters in encoded field values
quoteValue   :: String -> String
quoteValue s = backtickQuote "`]#!" s

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
parse p = MsgFmt (parseMap M.empty "" $ stripUnnecessary p) $ T.pack p

-- Skip comment and directive lines
stripUnnecessary :: String -> String
stripUnnecessary = unlines . filter pred . lines
  where pred = fromMaybe True . fmap notComment . listToMaybe
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

-- Split keys from values
splitEF :: Char -> String -> (String, String)
splitEF d s = splitEF' False d s

-- Worker for splitting keys and values dequoting backticks
splitEF' :: Bool -> Char -> String -> (String,String)
splitEF' _ _ []           = ("","")
splitEF' False d ('`':xs) = let rest = splitEF' True  d xs
                            in rest
splitEF' b d (x:xs)
  | not b && d == x       = ("",xs)
  | otherwise             = let rest = splitEF' False d xs
                            in ((x : fst rest), snd rest)


-- | Parse the encoded form message in a file into a MsgFmt
parseFile   :: String -> IO MsgFmt
parseFile f = do
    s <- readFile f
    return $ parse s
