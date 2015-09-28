{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}

module PackItForms.ICS213Tests (ics213Tests) where

import Control.Lens.Traversal (sequenceOf)
import Control.Lens.Each (each)
import Control.Lens.Operators ((%~))
import Control.Monad (liftM)
import Data.Maybe (isNothing, isJust)
import qualified Data.Fixed as F
import qualified Data.Map as Map
import qualified Data.Set as S
import Data.Time.Calendar (Day(ModifiedJulianDay))
import Data.Time.LocalTime (TimeOfDay(TimeOfDay))
import PackItForms.ICS213
import qualified PackItForms.MsgFmt as MF
import PackItForms.ParseUtils
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck as QC

ics213Tests :: TestTree
ics213Tests = testGroup "Tests for ICS 213 messages"
  [localOption (QuickCheckTests 10000)
   (QC.testProperty "ICS213 messages roundtrip through MsgFmt" prop_roundtrips)]

prop_roundtrips :: Msg CatchAllMapBody -> Property
prop_roundtrips m = m === fromMsgFmt (toMsgFmt m)

instance (Arbitrary a, ICS213Body a) => Arbitrary (Msg a) where
  arbitrary = do
    h <- arbitrary
    b <- arbitrary
    f <- arbitraryFtr (stationRole h)
    return $ Msg h b f

instance Arbitrary Header where
  arbitrary = do
    let isValidMsgNum :: String -> Bool
        isValidMsgNum x = not (null x)
                       && all (`notElem` ("=,\n\r"::String)) x
        msgnum :: Gen MF.MsgNo
        msgnum = suchThat (resize 8 arbitrary) isValidMsgNum
    m <- msgnum
    -- Ensure that otherMsgNo is != to myMsgNo
    let othermsgnum :: Gen MF.MsgNo
        othermsgnum = suchThat msgnum (/= m)
    let myMsgNo = Right m
    let sq2 (a, b) = a >>= (\a -> b >>= (\b -> return (a, b)))
    (stationRole, otherMsgNo) <- oneof $ each %~ sq2 $
      [(return $ Right Receiver, fmap Just othermsgnum)
      ,(return $ Right Sender, fmap Just othermsgnum)
      ,(return $ Right Sender, return Nothing)]
    formDate <- oneof [return . Left $ MissingField "1a."
                      ,return . Left $ FieldParseError "1a."
                      ,liftM Right arbitrary]
    formTime <- oneof [return . Left $ MissingField "1b."
                      ,return . Left $ FieldParseError "1b."
                      ,liftM Right arbitrary]
    severity <- oneof [return . Left $ MissingField "4."
                      ,return . Left $ FieldParseError "4."
                      ,liftM Right arbitrary]
    handlingOrder <- oneof [return . Left $ MissingField "5."
                           ,return . Left $ FieldParseError "5."
                           ,liftM Right arbitrary]
    requestTakeAction <- arbitrary
    requestReply <- arbitrary
    replyBy <- if requestReply == Just True
                  then resize 20 arbitrary
                  else return Nothing
    isFyi <- arbitrary
    toPosition <- oneof [return . Left $ MissingField "7."
                        ,liftM Right $ resize 30 arbitrary]
    toName <- resize 40 arbitrary
    toLocation <- oneof [return . Left $ MissingField "9a."
                        ,liftM Right $ resize 30 arbitrary]
    toTelephone <- resize 12 arbitrary
    fromPosition <- oneof [return . Left $ MissingField "8."
                          ,liftM Right $ resize 30 arbitrary]
    fromName <- resize 40 arbitrary
    fromLocation <- oneof [return . Left $ MissingField "9b."
                          ,liftM Right $ resize 30 arbitrary]
    fromTelephone <- resize 12 arbitrary
    subject <- oneof [return . Left $ MissingField "10."
                     ,liftM Right $ resize 100 arbitrary]
    reference <- resize 30 arbitrary
    return Header{..}

instance Arbitrary Day where
  arbitrary = ModifiedJulianDay <$> choose (50000, 100000)

instance Arbitrary TimeOfDay where
  arbitrary = TimeOfDay <$> choose (0,23)
                          <*> choose (0, 59) <*> arbitrary

instance Arbitrary F.Pico where
  arbitrary = liftM ((*10^12) . F.MkFixed) $ choose (0,59)

instance Arbitrary Severity where
  arbitrary = elements [Emergency, Urgent, OtherSeverity]

instance Arbitrary HandlingOrder where
  arbitrary = elements [Immediate, Priority, Routine]

instance Arbitrary CatchAllMapBody where
  arbitrary = CatchAllMapBody <$> Map.fromList <$>
    listOf
      (sequenceOf each
         (suchThat (resize 20 arbitrary) (not . isICS213Field),
          resize 100 arbitrary))

arbitraryFtr role = do
  let err et f1 f2 = case role of
                       Right Sender -> Left $ et f1
                       Right Receiver -> Left $ et f2
                       Left x -> Left x
  actionTaken <- resize 100 arbitrary
  ccDest <- arbitrary
  commMethod <- oneof [return . Left $ MissingField "Method"
                      ,liftM Right arbitrary]
  opCall <- oneof [return $ err MissingField "OpCall" "ocall"
                  ,liftM Right $ suchThat (resize 8 arbitrary) $ not . null]
  opName <- oneof [return $ err MissingField "OpName" "oname"
                  ,liftM Right $ suchThat (resize 30 arbitrary) $ not . null]
  otherOpCall <- oneof [return Nothing
                       ,liftM Just $ suchThat (resize 8 arbitrary) $ not . null]
  otherOpName <- oneof [return Nothing
                       ,liftM Just $ suchThat (resize 30 arbitrary) $ not . null]
  opDate <- oneof [return $ err MissingField "OpDate" "ordate"
                  ,return $ err FieldParseError "OpDate" "ordate"
                  ,liftM Right arbitrary]
  opTime <- oneof [return $ err MissingField "OpTime" "ortime"
                  ,return $ err FieldParseError "OpTime" "ortime"
                  ,liftM Right arbitrary]
  otherOpDate <- oneof [return Nothing
                       ,liftM Just arbitrary]
  otherOpTime <- oneof [return Nothing
                       ,liftM Just arbitrary]
  bbsDate <- oneof [return Nothing
                   ,liftM Just arbitrary]
  bbsTime <- oneof [return Nothing
                   ,liftM Just arbitrary]
  return Footer{..}

instance Arbitrary (S.Set CopyDest) where
  arbitrary = liftM S.fromList $
                sublistOf [Management, Operations, Planning, Logistics, Finance]

instance Arbitrary CommMethod where
  arbitrary = oneof [return Telephone
                    ,return DispatchCenter
                    ,return EOCRadio
                    ,return FAX
                    ,return Courier
                    ,return AmateurRadio
                    ,liftM Other arbitrary]

