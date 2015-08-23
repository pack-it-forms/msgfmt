{-|
Module      : ParseUtils
Description : Utilities for parsing messages
Copyright   : (c) 2015 Keith Amidon <camalot@picnicpark.org>
            : (c) 2015 Peter Amidon <peter@picnicpark.org>
License     : Apache-2

Utilities for doing a number of operations that are useful when
attempting to construct type-safe form representations from 'MsgFmt' values
-}

module PackItForms.ParseUtils
       (FormatError(..), withFldFns, genFlds, eitherToMaybe) where

import qualified PackItForms.MsgFmt as MF
import Data.Maybe (fromMaybe)
import Control.Monad (liftM2)

-- | Errors encountered while trying to create Msg
data FormatError = MissingField String | FieldParseError String deriving (Show, Eq)

-- | When parsing a form, it is often useful to be able to easily get
-- the value of field after conversion to a number of types.  However,
-- these functions are usually most useful if they can close over the
-- 'PackItForms.MsgFmt.MsgFmt' being parsed, so that they only require
-- one argument: the name of the field to extract.  This function
-- therefore takes a function of three parameters and passes in three
-- functions:
--
--   [@fld@] This function returns @'Just' value@ if the field exists,
--   or @'Nothing'@ if it doesn't
--
--   [@fldE@] This function returns the value of field as a 'String',
--   or @""@ if it doesn't exist; this function is intended to be used
--   in cases where a missing field is equal to an "empty" field.
--
--   [@fldR@] This function returns @'Right' value@ if the field
--   exists, and @'Left' $ 'MissingField' fldname@ if it doesn't; this
--   function is intended to be used when the field is "required".
withFldFns :: MF.MsgFmt
           -> ((String -> Maybe String)
            -> (String -> String)
            -> (String -> Either FormatError String)
            -> a)
           -> a
withFldFns m f = f fld fldE fldR
  where fld = MF.getValue m
        fldE = fromMaybe "" . fld
        fldR x = maybe (Left $ MissingField x) Right $ fld x

-- | When generating a message format, it is often useful to generate
-- a list of keys and values from a number of form fields, as this
-- representation can be converted into a 'MF.MsgFmt' using
-- 'MF.fromList'.  This function helps make it easy to do so; it takes
-- a function, and produces a new function that takes a field name and
-- a list of fields and values, calls the function to get the correct
-- value, and appends the result to the list.  It is intended to be
-- used as @'foldr' ('genFlds' fldVal) [] ["field 1", "field 2", ...]@,
-- where @fldVal@ is a function that takes a field name and returns
-- the @Just value@ if that field has a valid value, or @Nothing@
-- otherwise.
genFlds :: (String -> Maybe String) -> String -> [(String, String)] -> [(String, String)]
genFlds f i acc = maybe acc (\x -> (i, x):acc) $ f i

-- | When implementing functions to be used with 'genFlds', it is
-- often useful to be able to convert a value of type @'Either'
-- 'FormatError' 'String'@ to a value of type @'Maybe' 'String'@,
-- since required fields often have a type of @'Either' 'FormatError'
-- 'String'@.  This function executes that transformation; if the
-- value is a @'Left' ('MissingField' _)@ it returns 'Nothing', since
-- a missing field should, presumably, not be output; otherwise, it
-- returns @'Just' ""@ in the case of any other 'Left' value, or
-- @'Just' y@ in the case of a @'Right' y@ value.
eitherToMaybe :: Either FormatError String -> Maybe String
eitherToMaybe x = case x of
                    Left (MissingField _) -> Nothing
                    Left _ -> Just ""
                    Right y -> Just y
