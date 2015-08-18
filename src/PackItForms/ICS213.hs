{-# LANGUAGE OverloadedStrings, GADTs, StandaloneDeriving #-}

{-|
Module      : ICS213
Description : ICS 213 Message Handling
Copyright   : (c) 2015 Keith Amidon <camalot@picnicpark.org>,
            : (c) 2015 Peter Amidon <peter@picnicpark.org>
License     : Apache-2

Type-safe handling of information in the header and footer used on
standard ICS 213 message forms.

TODO: * Add function to find validity errors
TODO: * Add function isValid
TODO: * Add function to fill in defaults to make a message valid
TODO: * Add completely empty form creation function
TODO: * Add functions to derive LocalTimes from separate Date & Time fields
-}

module PackItForms.ICS213
       (Msg(..)
       ,Header(..)
       ,Footer(..)
       ,StationRole(..)
       ,MsgNo
       ,Severity(..)
       ,HandlingOrder(..)
       ,ICS213Body(..)
       ,CatchAllMapBody(..)
       ,CopyDest(..)
       ,CommMethod(..)
       ,FormatError(..)
       ,isICS213Field
       ,received
       ,toMsgFmt
       ,fromMsgFmt) where

import qualified Data.Map as M
import Data.Maybe (isJust, fromJust, fromMaybe)
import Control.Monad (liftM)
import qualified Data.Set as S
import Data.Time.Calendar (Day)
import Data.Time.LocalTime (TimeOfDay)
import Data.Time.Format (defaultTimeLocale, parseTimeM, formatTime)
import qualified PackItForms.MsgFmt as MF

-- This has show instances for Either
import GHC.Show

-- | Full ICS213 message contents
data Msg a where
  Msg :: ICS213Body a => Header -> a -> Footer -> Msg a

deriving instance Eq a => Eq (Msg a)
deriving instance Show a => Show (Msg a)

-- | Header contents of an ICS213 message
data Header = Header { stationRole :: Maybe StationRole
                     , myMsgNo :: Either FormatError MsgNo
                     , otherMsgNo :: Maybe MsgNo
                     , formDate :: Either FormatError Day
                     , formTime :: Either FormatError TimeOfDay
                     , severity :: Either FormatError Severity
                     , handlingOrder :: Either FormatError HandlingOrder
                     , requestTakeAction :: Maybe Bool
                     , requestReply :: Maybe Bool
                     , replyBy :: Maybe String
                     , isFyi :: Maybe Bool
                     , toPosition :: Either FormatError String
                     , toName :: Maybe String
                     , toLocation :: Either FormatError String
                     , toTelephone :: Maybe String
                     , fromPosition :: Either FormatError String
                     , fromLocation :: Either FormatError String
                     , fromName :: Maybe String
                     , fromTelephone :: Maybe String
                     , subject :: Either FormatError String
                     , reference :: Maybe MsgNo } deriving (Show, Eq)

-- | Role of the station in handling the message
data StationRole = Sender | Receiver
  deriving (Show, Eq)

-- | Identifying number given to the message
type MsgNo = String

-- | Severity values for the message
data Severity = Emergency | Urgent | OtherSeverity
  deriving (Show, Eq, Ord)

-- | Requested handling order for the message
data HandlingOrder = Immediate | Priority | Routine
  deriving (Show, Eq, Ord)

-- | Body contents of the message
--
-- More specific message forms can be built around the basic structure
-- of the ICS213 form.  This class represents types which can
-- serialize/deserialize themselves into MsgFmts, and can therefore be
-- used as bodies for ICS 213 forms.
class ICS213Body a where
  -- | Convert an MF.MsgFmt into a body
  bodyFromMsgFmt :: MF.MsgFmt -> a
  -- | Convert a body into an MF.MsgFmt
  bodyToMsgFmt :: a -> MF.MsgFmt

-- | Default catch-all instance for the ICS213Body class.  This simply
-- puts all of the fields that aren't header or footer fields into a
-- map in 'bodyFromMsgFmt' and extracts them back into a MsgFmt in
-- 'bodyToMsgFmt'
data CatchAllMapBody = CatchAllMapBody (M.Map String String) deriving (Eq, Show)

instance ICS213Body CatchAllMapBody where
  bodyFromMsgFmt m = CatchAllMapBody $
    M.filterWithKey (\k _ -> not $ isICS213Field k) $ MF.getMap m
  bodyToMsgFmt (CatchAllMapBody b) = MF.fromList (M.toList b)


-- | Footer contents of an ICS213 message
data Footer = Footer { actionTaken :: Maybe String
                     , ccDest :: S.Set CopyDest
                     , commMethod :: Either FormatError CommMethod
                     , opCall :: Either FormatError CallSign
                     , opName :: Either FormatError String
                     , opDate :: Either FormatError Day
                     , opTime :: Either FormatError TimeOfDay }
            deriving (Show, Eq)

-- | Additional ICS destinations for copies of the message
data CopyDest = Management | Operations | Planning | Logistics | Finance
  deriving (Show, Eq, Ord)

-- | Method by which the communication of the form was accomplished
--
-- Arbitraty methods can be documented using the Other constructor
data CommMethod = Telephone | DispatchCenter | EOCRadio | FAX
     | Courier | AmateurRadio | Other (Maybe String)
  deriving (Show, Eq, Ord)

headerFields = ["1a."    -- Date
               ,"1b."    -- Time
               ,"2."     -- Sender Msg #
               ,"MsgNo"
               ,"3."     -- Receiver Msg #
               ,"4."     -- Severity
               ,"5."     -- Message Handling Order
               ,"6a."    -- Request Action Y/N
               ,"6b."    -- Request Reply Y/N
               ,"6c."    -- FYI Y/N
               ,"6d."    -- Reply by
               ,"7."     -- To ICS Position
               ,"8."     -- From ICS Position
               ,"9a."    -- To ICS Location
               ,"9b."    -- From ICS Location
               ,"ToName"
               ,"ToTel"
               ,"FmName"
               ,"FmTel"
               ,"10."    -- Subject
               ,"11."]   -- Reference

footerFields = ["13."    -- Action Taken
               ,"CCMgt"
               ,"CCOps"
               ,"CCPlan"
               ,"CCLog"
               ,"CCFin"
               ,"Method"
               ,"Other"
               ,"OpCall"
               ,"OpName"
               ,"OpDate"
               ,"OpTime"]

isICS213Field :: String -> Bool
isICS213Field k = k `elem` headerFields || k `elem` footerFields

-- | Errors encountered while trying to create Msg
data FormatError = MissingField String | FieldParseError String deriving (Show, Eq)

-- | Separate type for callsigns since they are important
type CallSign = String

getErrors :: Msg a -> [FormatError]
getErrors (Msg h b f) =
  foldl addError [] $ map ($ h) funcs
  where addError l (Left e) = e:l
        addErorr l (Right _) = l
        rr (Left e) = Left e
        rr (Right _) = Right ()
        funcs = [ rr . myMsgNo, rr . formDate, rr . formTime, rr. severity
                , rr . handlingOrder, rr . toPosition, rr . toLocation
                , rr . fromPosition, rr . fromLocation, rr . subject]

fromMsgFmt :: ICS213Body a => MF.MsgFmt -> Msg a
fromMsgFmt m = Msg header body footer
  where header = Header { stationRole = role
                        , myMsgNo = fldR "MsgNo"
                        , otherMsgNo = other
                        , formDate = formD
                        , formTime = formT
                        , severity = sev
                        , handlingOrder = order
                        , requestTakeAction = case fld "6a." of
                                                Just "Yes" -> Just True
                                                Just _ -> Just False
                                                otherwise -> Nothing
                        , requestReply = case fld "6b." of
                                           Just "Yes" -> Just True
                                           Just _ -> Just False
                                           otherwise -> Nothing
                        , replyBy = fld "6d."
                        , isFyi = case fld "6c." of
                                    Just "checked" -> Just True
                                    Just _ -> Just False
                                    otherwise -> Nothing
                        , toPosition = fldR "7."
                        , toLocation = fldR "9a."
                        , toName = fld "ToName"
                        , toTelephone = fld "ToTel"
                        , fromPosition = fldR "8."
                        , fromLocation = fldR "9b."
                        , fromName = fld "FmName"
                        , fromTelephone = fld "FmTel"
                        , subject = fldR "10."
                        , reference = fld "11." }
        body = bodyFromMsgFmt m
        footer = Footer { actionTaken = fld "13."
                        , ccDest = copies
                        , commMethod = method
                        , opCall = fldR "OpCall"
                        , opName = fldR "OpName"
                        , opDate = opD
                        , opTime = opT}
        fld = MF.getValue m
        fldE = fromMaybe "" . fld
        fldR x = case fld x of
                   Nothing -> Left $ MissingField x
                   Just y -> Right y
        role | isJust n && r == n = Just Receiver
             | isJust n && s == n = Just Sender
             | isJust s = Just Receiver
             | isJust r = Just Sender
             | otherwise = Nothing
           where s = fld "2."
                 r = fld "3."
                 n = fld "MsgNo"
        other | role == Just Sender = fld "3."
              | role == Just Receiver = fld "2."
              | otherwise = Nothing
        formD = fldR "1a." >>= maybe (Left $ FieldParseError "1a.") Right
                                . parseTimeM True defaultTimeLocale "%m/%d/%Y"
        formT = fldR "1b." >>= \x -> case parseTimeM True defaultTimeLocale "%T" x of
                Just t -> Right t
                Nothing -> case parseTimeM True defaultTimeLocale "%H:%M" x of
                             Just t -> Right t
                             Nothing -> Left $ FieldParseError "1b."
        sev = fldR "4." >>= \x -> case x of
                                    "EMERGENCY" -> Right Emergency
                                    "URGENT" -> Right Urgent
                                    "OTHER" -> Right OtherSeverity
                                    otherwise -> Left $ FieldParseError "4."
        order = fldR "5." >>= \x -> case x of
                                      "IMMEDIATE" -> Right Immediate
                                      "PRIORITY" -> Right Priority
                                      "ROUTINE" -> Right Routine
                                      otherwise -> Left $ FieldParseError "5."
        method | r == Just "Telephone" = Right Telephone
               | r == Just "Dispatch Center" = Right DispatchCenter
               | r == Just "EOC Radio" = Right EOCRadio
               | r == Just "FAX" = Right FAX
               | r == Just "Courier" = Right Courier
               | r == Just "AmateurRadio" = Right AmateurRadio
               | r == Just "Other" = Right $ Other $ fld "Other"
               | otherwise = Left $ MissingField "Method"
           where r = fld "Method"
        copies = foldl step S.empty destOptions
          where step acc x | fldE (fst x) == "checked" = S.insert (snd x) acc
                           | otherwise = acc
                destOptions = [("CCMgt", Management)
                              ,("CCOps", Operations)
                              ,("CCPlan", Planning)
                              ,("CCLog", Logistics)
                              ,("CCFin", Finance)]
        opD = fldR "OpDate" >>= maybe (Left $ FieldParseError "OpDate") Right
                                . parseTimeM True defaultTimeLocale "%m/%d/%Y"
        opT = fldR "OpTime" >>= \x -> case parseTimeM True defaultTimeLocale "%T" x of
                Just t -> Right t
                Nothing -> case parseTimeM True defaultTimeLocale "%H:%M" x of
                             Just t -> Right t
                             Nothing -> Left $ FieldParseError "OpTime"

-- | Test whether a message was recieved
received :: Msg a -> Bool
received m | stationRole h == Just Receiver = True
           | isJust (otherMsgNo h) = True
           | otherwise = False
       where (Msg h _ _) = m

toMsgFmt :: Msg a -> MF.MsgFmt
toMsgFmt m@(Msg h b f) =  MF.insertAll body g2
  where locale = defaultTimeLocale
        genFlds f i acc = maybe acc (\x -> (i, x):acc) $ f i
        g1 = foldr (genFlds ftrVal) [] footerFields
        ftrVal "OpTime" = eitherToMaybe $ liftM (formatTime locale "%T") (opTime f)
        ftrVal "OpDate" = eitherToMaybe $ liftM (formatTime locale "%m/%d/%Y") (opDate f)
        ftrVal "OpName" = eitherToMaybe $ opName f
        ftrVal "OpCall" = eitherToMaybe $ opCall f
        ftrVal "Other" = otherMethod $ commMethod f
          where otherMethod (Right (Other v)) = v
                otherMethod _ = Nothing
        ftrVal "Method" = case commMethod f of
                            Right Telephone -> Just "Telephone"
                            Right DispatchCenter -> Just "Dispatch Center"
                            Right EOCRadio -> Just "EOC Radio"
                            Right FAX -> Just "FAX"
                            Right Courier -> Just "Courier"
                            Right AmateurRadio -> Just "AmateurRadio"
                            Right (Other _) -> Just "Other"
                            otherwise -> Nothing
        ftrVal "CCFin" = if Finance `S.member` ccDest f
                            then Just "checked" else Nothing
        ftrVal "CCLog" = if Logistics `S.member` ccDest f
                            then Just "checked" else Nothing
        ftrVal "CCPlan" = if Planning `S.member` ccDest f
                            then Just "checked" else Nothing
        ftrVal "CCOps" = if Operations `S.member` ccDest f
                            then Just "checked" else Nothing
        ftrVal "CCMgt" = if Management `S.member` ccDest f
                            then Just "checked" else Nothing
        ftrVal "13." = actionTaken f
        body = bodyToMsgFmt b
        g2 = foldr (genFlds hdrVal) g1 headerFields
        hdrVal "11." = reference h
        hdrVal "10." = eitherToMaybe $ subject h
        hdrVal "FmTel" = fromTelephone h
        hdrVal "FmName" = fromName h
        hdrVal "ToTel" = toTelephone h
        hdrVal "ToName" = toName h
        hdrVal "9b." = eitherToMaybe $ fromLocation h
        hdrVal "9a." = eitherToMaybe $ toLocation h
        hdrVal "8." = eitherToMaybe $ fromPosition h
        hdrVal "7." = eitherToMaybe $ toPosition h
        hdrVal "6d." = replyBy h
        hdrVal "6c." = case isFyi h of
                         Just True -> Just "checked"
                         Just False -> Just ""
                         _ -> Nothing
        hdrVal "6b." = case requestReply h of
                         Just True -> Just "Yes"
                         Just False -> Just "No"
                         _ -> Nothing
        hdrVal "6a." = case requestTakeAction h of
                         Just True -> Just "Yes"
                         Just False -> Just "No"
                         _ -> Nothing
        hdrVal "5." = case handlingOrder h of
                        Right Immediate -> Just "IMMEDIATE"
                        Right Priority -> Just "PRIORITY"
                        Right Routine -> Just "ROUTINE"
                        Left (FieldParseError _) -> Just ""
                        otherwise -> Nothing
        hdrVal "4." = case severity h of
                        Right Emergency -> Just "EMERGENCY"
                        Right Urgent -> Just "URGENT"
                        Right OtherSeverity -> Just "OTHER"
                        Left (FieldParseError _) -> Just ""
                        otherwise -> Nothing
        hdrVal "3." = case stationRole h of
                        Just Sender -> otherMsgNo h
                        otherwise -> Nothing
        hdrVal "MsgNo" = eitherToMaybe $ myMsgNo h
        hdrVal "2." = case stationRole h of
                        Just Receiver -> otherMsgNo h
                        otherwise -> Nothing
        hdrVal "1b." = eitherToMaybe $ liftM (formatTime locale "%T") (formTime h)
        hdrVal "1a." = eitherToMaybe $ liftM (formatTime locale "%m/%d/%Y") (formDate h)
        eitherToMaybe x = case x of
                            Left (MissingField _) -> Nothing
                            Left _ -> Just ""
                            Right y -> Just y
