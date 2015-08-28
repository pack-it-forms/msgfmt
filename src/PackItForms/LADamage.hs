{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards #-}
{-|
Module      : LADamage
Description : Los Altos Damage Form Message Handling
Copyright   : (c) 2015 Keith Amidon <camalot@picnicpark.org>
              (c) 2015 Peter Amidon <peter@picnicpark.org>
License     : Apache-2

Type-safe handling of information in the body of Los Altos damage
forms, based on the ICS 213 message form handling support in
'PackItForms.ICS213'
-}

module PackItForms.LADamage
       (LADamageBody(..)
       ,Update
       ,Accumulated
       ,BATStatus(..)
       ,number
       ,okay
       ,minorInjuryCnt
       ,delayedInjuryCnt
       ,immediateInjuryCnt
       ,fatalityCnt
       ,missingCnt
       ,trappedCnt
       ,lightDamageCnt
       ,moderateDamageCnt
       ,heavyDamageCnt
       ,fireCnt
       ,electricHazardCnt
       ,waterHazardCnt
       ,gasHazardCnt
       ,chemicalHazardCnt
       ,roadBlocked
       ,BATNum(..)
       ,rollupBATStatuses
       ,addBATStatuses) where

import Data.Char (isAlpha)
import qualified Data.Map as M
import qualified PackItForms.ICS213 as ICS213
import qualified PackItForms.MsgFmt as MF
import PackItForms.ParseUtils
import qualified Data.Text as T
import qualified Data.Either as E
import Control.Lens
import Control.Lens.TH
import Control.Arrow

-- | ICS213 form body for the Los Altos Damage Assessment form
data LADamageBody = LADamageBody { statuses :: [BATStatus Update]
                                 , notes :: T.Text } deriving (Eq, Show)

-- | A type to be used as the phantom type parameter for a 'BATStatus'
-- representing that this status is an update.
data Update
-- | A type to be used as the phantom type parameter for a 'BATStatus'
-- representing that this status is the accumulted,
-- best-information-at-this-time, information for a BAT.
data Accumulated

-- | BATStatus represents the status information for a BAT; it is a
-- phantom type, where the phantom type parameter represents whether a
-- given 'BATStatus' is an incremental update on the state of a BAT or
-- the accumulated, best-information-at-this-time status of a BAT.
data BATStatus a = BATStatus { _number :: Either FormatError BATNum
                             , _okay :: Maybe Bool
                             , _minorInjuryCnt :: Maybe Integer
                             , _delayedInjuryCnt :: Maybe Integer
                             , _immediateInjuryCnt :: Maybe Integer
                             , _fatalityCnt :: Maybe Integer
                             , _missingCnt :: Maybe Integer
                             , _trappedCnt :: Maybe Integer
                             , _lightDamageCnt :: Maybe Integer
                             , _moderateDamageCnt :: Maybe Integer
                             , _heavyDamageCnt :: Maybe Integer
                             , _fireCnt :: Maybe Integer
                             , _electricHazardCnt :: Maybe Integer
                             ,  _waterHazardCnt :: Maybe Integer
                             , _gasHazardCnt :: Maybe Integer
                             , _chemicalHazardCnt :: Maybe Integer
                             , _roadBlocked :: Maybe Bool
     } deriving (Eq, Show)

-- | A type alias for BAT numbers
type BATNum = Integer

makeLensesWith lensRules ''BATStatus

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
                    = BATStatus { _number = batFldR "a.batnum"
                                , _okay = batFldB "b.ok"
                                , _minorInjuryCnt = batFld "c.people-minor"
                                , _delayedInjuryCnt = batFld "d.people-delayed"
                                , _immediateInjuryCnt = batFld "e.people-immediate"
                                , _fatalityCnt = batFld "f.people-V"
                                , _missingCnt = batFld "g.people-missing"
                                , _trappedCnt = batFld "h.people-trapped"
                                , _lightDamageCnt = batFld "i.damage-light"
                                , _moderateDamageCnt = batFld "j.damage-moderate"
                                , _heavyDamageCnt = batFld "k.damage-heavy"
                                , _fireCnt = batFld "l.hazards-fires-burning"
                                , _electricHazardCnt = batFld "m.hazards-electric"
                                , _waterHazardCnt = batFld "n.hazards-water"
                                , _gasHazardCnt = batFld "o.hazards-gas"
                                , _chemicalHazardCnt = batFld "p.hazards-chemical"
                                , _roadBlocked = batFldB "q.roads-no-access" }
                    where batFld :: (Read a) => String -> Maybe a
                          batFld = fmap read . fld . (("12."++show n)++)
                          batFldR :: (Read a) => String -> Either FormatError a
                          batFldR = fmap read . fldR . (("12."++show n)++)
                          batFldB x = case fld $ "12." ++ show n ++ x of
                                        Just "checked" -> Just True
                                        Just _ -> Just False
                                        otherwise -> Nothing
                  nonEmpty m = E.isRight $ m ^. number
  bodyToMsgFmt (LADamageBody s t) = MF.fromList list
    where notes = ("12.10.general-notes", T.unpack t)
          list = notes:concatMap statusToKV (zip s [0..])
          statusToKV (s, i) = foldr (genFlds fldVal) [] mkBatStatusFields
            where fldVal = fldVal' . truncateInput
                  mkBatStatusFields = map (("12."++show i)++) batStatusFields
                  truncateInput = dropWhile (not . isAlpha)
                  fldVal' "a.batnum" = eitherToMaybe . fmap show $ s ^. number
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
                  intFldToStr f = show <$> s ^. f
                  boolFldToStr f = case s ^. f of
                                     Just True -> Just "checked"
                                     Just False -> Just ""
                                     _ -> Nothing

-- | "Roll up" a list of BAT statuses into an aggregate status; values
-- from later messages supresede values from earlier messages.  The
-- number of the first BATStatus in the list is assumed to be the BAT
-- for which statuses are being rolled up; if any other messages have
-- a conflicting BAT number, they will be ignored.
rollupBATStatuses :: [BATStatus Update] -> BATStatus Accumulated
rollupBATStatuses = toAcc . head &&& tail >>> uncurry (foldl addBATStatuses)
  where toAcc :: BATStatus Update -> BATStatus Accumulated
        toAcc BATStatus{..} = BATStatus{..}

-- | Add one BAT status record into another; this is intended to be
-- used for incremental updating of aggregate statuses.  The semantics
-- are the same as for 'rollupBATStatuses' above.
addBATStatuses :: BATStatus Accumulated -> BATStatus Update -> BATStatus Accumulated

-- The implementation of 'addBATStatuses' folds over a list of fields
-- in order to allow (nearly) boilerplate-free implementation, and the
-- easy addition of new fields.  Unfortunately, this list must,
-- therefore, be a list of lenses; since lenses are polymorphic types,
-- it is therefore necessary to use a newtype wrapper (such as the
-- ones provided in "Control.Lens.Reified"); this is a similar type.
-- Using this data type instead of the newtype wrappers in
-- "Control.Lens.Reified", however, provides one significant
-- advantage: by using existential types, it is possible to treat
-- integer and boolean fields the same way, which makes the
-- implementation of 'addBATStatuses' significantly simpler.
data BATStatusLens = forall b. BSL (forall a. Lens' (BATStatus a) (Maybe b))

addBATStatuses old new = if old ^. number /= new ^. number
                            then old
                            else foldl (\x (BSL y) -> addFld x y) old flds
  where addFld :: BATStatus Accumulated -> (forall a. Lens' (BATStatus a) (Maybe b)) -> BATStatus Accumulated
        addFld s f = maybe s (\x -> s & f .~ Just x) $ new ^. f
        flds :: [BATStatusLens]
        flds = [ BSL okay, BSL minorInjuryCnt, BSL delayedInjuryCnt
               , BSL immediateInjuryCnt, BSL fatalityCnt, BSL missingCnt
               , BSL trappedCnt, BSL lightDamageCnt, BSL moderateDamageCnt
               , BSL heavyDamageCnt, BSL fireCnt, BSL electricHazardCnt
               , BSL waterHazardCnt, BSL gasHazardCnt, BSL chemicalHazardCnt
               , BSL roadBlocked]
