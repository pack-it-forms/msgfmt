{-|
Module      : LADamage
Description : Los Altos Damage Form Message Handling
Copyright   : (c) 2015 Keith Amidon <camalot@picnicpark.org>
              (c) 2015 Peter Amidon <peter@picnicpark.org>
License     : Apache-2

Type-safe handling of information in the body of Los Altos damage
forms, based on the ICS 213 message form handling support in
'PackItForms.ICS213'

TODO: * Add support for "rolling up" multiple status reports into one.
-}

module PackItForms.LADamage (LADamageBody(..), BATStatus(..), BATNum(..)) where

import Data.Char (isAlpha)
import qualified Data.Map as M
import qualified PackItForms.ICS213 as ICS213
import qualified PackItForms.MsgFmt as MF
import PackItForms.ParseUtils
import qualified Data.Text as T
import qualified Data.Either as E

-- | ICS213 form body for the Los Altos Damage Assessment form
data LADamageBody = LADamageBody { statuses :: [BATStatus]
                                 , notes :: T.Text } deriving (Eq, Show)

-- | BATStatus represents the status information for a BAT
data BATStatus = BATStatus { number :: Either FormatError BATNum
                            , okay :: Maybe Bool
                            , minorInjuryCnt :: Maybe Integer
                            , delayedInjuryCnt :: Maybe Integer
                            , immediateInjuryCnt :: Maybe Integer
                            , fatalityCnt :: Maybe Integer
                            , missingCnt :: Maybe Integer
                            , trappedCnt :: Maybe Integer
                            , lightDamageCnt :: Maybe Integer
                            , moderateDamageCnt :: Maybe Integer
                            , heavyDamageCnt :: Maybe Integer
                            , fireCnt :: Maybe Integer
                            , electricHazardCnt :: Maybe Integer
                            , waterHazardCnt :: Maybe Integer
                            , gasHazardCnt :: Maybe Integer
                            , chemicalHazardCnt :: Maybe Integer
                            , roadBlocked :: Maybe Bool
     } deriving (Eq, Show)

-- | A type alias for BAT numbers
type BATNum = Integer

batStatusFields = ["a.batnum"
                  ,"b.ok"
                  ,"c.people-minor"
                  ,"d.people-delayed"
                  ,"e.people-immediate"
                  ,"f.people-V"
                  ,"g.people-missing"
                  ,"h.people-trapped"
                  ,"i.damage-light"
                  ,"j.damage-moderate"
                  ,"k.damage-heavy"
                  ,"l.hazards-fires-burning"
                  ,"m.hazards-electric"
                  ,"n.hazards-water"
                  ,"o.hazards-gas"
                  ,"p.hazards-chemical"
                  ,"q.roads-no-access"]


instance ICS213.ICS213Body LADamageBody where
  bodyFromMsgFmt m = withFldFns m bodyFromMsgFmtWithFields
    where bodyFromMsgFmtWithFields fld fldE fldR
            = LADamageBody (filter nonEmpty $ map mkBatStatus [0..9]) (T.pack $ fldE "12.10.general-notes")
            where mkBatStatus n
                    = BATStatus { number = batFldR "a.batnum"
                                , okay = batFldB "b.ok"
                                , minorInjuryCnt = batFld "c.people-minor"
                                , delayedInjuryCnt = batFld "d.people-delayed"
                                , immediateInjuryCnt = batFld "e.people-immediate"
                                , fatalityCnt = batFld "f.people-V"
                                , missingCnt = batFld "g.people-missing"
                                , trappedCnt = batFld "h.people-trapped"
                                , lightDamageCnt = batFld "i.damage-light"
                                , moderateDamageCnt = batFld "j.damage-moderate"
                                , heavyDamageCnt = batFld "k.damage-heavy"
                                , fireCnt = batFld "l.hazards-fires-burning"
                                , electricHazardCnt = batFld "m.hazards-electric"
                                , waterHazardCnt = batFld "n.hazards-water"
                                , gasHazardCnt = batFld "o.hazards-gas"
                                , chemicalHazardCnt = batFld "p.hazards-chemical"
                                , roadBlocked = batFldB "q.roads-no-access" }
                    where batFld :: (Read a) => String -> Maybe a
                          batFld = fmap read . fld . (("12."++show n)++)
                          batFldR :: (Read a) => String -> Either FormatError a
                          batFldR = fmap read . fldR . (("12."++show n)++)
                          batFldB x = case fld $ "12." ++ show n ++ x of
                                        Just "checked" -> Just True
                                        Just _ -> Just False
                                        otherwise -> Nothing
                  nonEmpty m = E.isRight $ number m
  bodyToMsgFmt (LADamageBody s t) = MF.fromList list
    where notes = ("12.10.general-notes", T.unpack t)
          list = notes:concatMap statusToKV (zip s [0..])
          statusToKV (s, i) = foldr (genFlds fldVal) [] mkBatStatusFields
            where fldVal = fldVal' . truncateInput
                  mkBatStatusFields = map (("12."++show i)++) batStatusFields
                  truncateInput = dropWhile (not . isAlpha)
                  fldVal' "a.batnum" = eitherToMaybe . fmap show $ number s
                  fldVal' "b.ok" = boolFldToStr okay
                  fldVal' "c.people-minor" = intFldToStr minorInjuryCnt
                  fldVal' "d.people-delayed" = intFldToStr delayedInjuryCnt
                  fldVal' "e.people-immediate" = intFldToStr immediateInjuryCnt
                  fldVal' "f.people-V" = intFldToStr fatalityCnt
                  fldVal' "g.people-missing" = intFldToStr missingCnt
                  fldVal' "h.people-trapped" = intFldToStr trappedCnt
                  fldVal' "i.damage-light" = intFldToStr lightDamageCnt
                  fldVal' "j.damage-moderate" = intFldToStr moderateDamageCnt
                  fldVal' "k.damage-heavy" = intFldToStr heavyDamageCnt
                  fldVal' "l.hazards-fires-burning" = intFldToStr fireCnt
                  fldVal' "m.hazards-electric" = intFldToStr electricHazardCnt
                  fldVal' "n.hazards-water" = intFldToStr waterHazardCnt
                  fldVal' "o.hazards-gas" = intFldToStr gasHazardCnt
                  fldVal' "p.hazards-chemical" = intFldToStr chemicalHazardCnt
                  fldVal' "q.roads-no-access" = boolFldToStr roadBlocked
                  fldval' x =  Nothing
                  intFldToStr f = show <$> f s
                  boolFldToStr f = case f s of
                                     Just True -> Just "checked"
                                     Just False -> Just ""
                                     _ -> Nothing
