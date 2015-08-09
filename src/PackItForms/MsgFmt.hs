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
, emptyRep
, insertKV
, insertAll
, MsgFmt(..)
) where

import System.IO
import qualified Data.Map as M
import qualified Data.Text as T
import Data.String.Utils

-- #####################
-- # Parsing functions #
-- #####################

-- These functions split an backtick-escaped string at the first instance of the delimiter that they see.
-- Delimiter -> value -> result
splitEF' :: Bool -> Char -> String -> (String,String)
splitEF' _ _ []           = ("","")
splitEF' False d ('`':xs) = let rest = splitEF' True  d xs
                            in rest
splitEF' b d (x:xs)
  | not b && d == x       = ("",xs)
  | otherwise             = let rest = splitEF' False d xs
                            in ((x : fst rest), snd rest)

splitEF     :: Char -> String -> (String, String)
splitEF d s = splitEF' False d s

-- Behavioral documentation:
--
-- Delimiters are considered :, [ and ].  : ends a key, [ starts a
-- value, and ] ends a value.  In all components, a backtick followed
-- by a character is an escape for that character, and can be used to
-- escape that character, so k`:ey is a valid key that would parse to
-- be named "k:ey".  Anything between delimiters is ignored; notably,
-- any characters between the colon that ends a key and the first
-- unescaped begin bracket will be ignored.

parseMap                   :: M.Map String String -> String -> String -> M.Map String String
parseMap parsed _ ""     = parsed
parseMap parsed key string = if key == ""
                                then let (k,ks) = splitEF ':' string
                                     in parseMap parsed (strip k) ks
                                else let (_,v) = splitEF '[' string
                                         (nv,vs) = splitEF ']' v
                                         submap = parseMap parsed "" vs
                                     in M.insert key (strip nv) submap

headEqual :: Eq a => a -> [a] -> Bool
headEqual _ [] = False
headEqual a (x:_) = a == x

stripUnnecessary :: String -> String
stripUnnecessary = unlines . filter (\x -> not ((headEqual '#' x) || (headEqual '!' x))) . lines

parse   :: String -> MsgFmt
parse p = MsgFmt (parseMap M.empty "" $ stripUnnecessary p) $ T.pack p

parseFile   :: String -> IO MsgFmt
parseFile f = do
    s <- readFile f
    return $ parse s

getValue :: MsgFmt -> String -> Maybe String
getValue (MsgFmt m _) k = M.lookup k m

getText :: MsgFmt -> T.Text
getText (MsgFmt _ s) = s

-- ########################
-- # Generation functions #
-- ########################

backtickQuote :: String -> String -> String
backtickQuote _ "" = ""
backtickQuote d (x:xs)
  | x `elem` d = '`' : x : backtickQuote d xs
  | otherwise = x : backtickQuote d xs

quoteKey   :: String -> String
quoteKey s = backtickQuote "`:#!" s

quoteValue   :: String -> String
quoteValue s = backtickQuote "`]#!" s

encodeKV     :: String -> String -> T.Text
encodeKV k v = T.pack $ k ++ ": [" ++ v ++ "]\n"

emptyRep :: MsgFmt
emptyRep = MsgFmt M.empty ""

insertKV :: String -> String -> MsgFmt ->  MsgFmt
insertKV k v (MsgFmt m s) = let nis = encodeKV (quoteKey k) (quoteValue v)
                                    in MsgFmt
                                         (M.insert (strip k) (strip v) m)
                                         (T.append s nis)

insertAll :: [(String,String)] -> MsgFmt ->  MsgFmt
insertAll [] p = p
insertAll ((k,v):kvs) p = insertAll kvs (insertKV k v p)

-- #####################
-- # Shared data types #
-- #####################

data MsgFmt = MsgFmt (M.Map String String) T.Text
