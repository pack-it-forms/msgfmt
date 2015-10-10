{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

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
       ,addBATStatuses
       ,ZoneStatus
       ,zoneNum
       ,numBATs
       ,okayCnt
       ,minorInjurySum
       ,delayedInjurySum
       ,immediateInjurySum
       ,fatalitySum
       ,missingSum
       ,trappedSum
       ,lightDamageSum
       ,moderateDamageSum
       ,heavyDamageSum
       ,fireSum
       ,electricHazardSum
       ,waterHazardSum
       ,gasHazardSum
       ,chemicalHazardSum
       ,roadBlockedCnt
       ,ZoneNum(..)
       ,CoveredField(..)
       ,ZoneMapping(..)
       ,empty
       ,insert
       ,zones
       ,batsOfZone
       ,zoneOfBAT
       ,rollupZoneStatuses) where

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
import qualified Data.List as L

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

-- Use explicit signatures to appease haddock
makeLensesWith (lensRules & generateSignatures .~ False) ''BATStatus
-- | Lens for the '_number' field of BATStatus
number :: Lens' (BATStatus a) (Either FormatError BATNum)
-- | Lens for the '_okay' field of BATStatus
okay :: Lens' (BATStatus a) (Maybe Bool)
-- | Lens for the '_minorInjuryCnt' field of BATStatus
minorInjuryCnt :: Lens' (BATStatus a) (Maybe Integer)
-- | Lens for the '_delayedInjuryCnt' field of BATStatus
delayedInjuryCnt :: Lens' (BATStatus a) (Maybe Integer)
-- | Lens for the '_immediateInjuryCnt' field of BATStatus
immediateInjuryCnt :: Lens' (BATStatus a) (Maybe Integer)
-- | Lens for the '_fatalityCnt' field of BATStatus
fatalityCnt :: Lens' (BATStatus a) (Maybe Integer)
-- | Lens for the '_missingCnt' field of BATStatus
missingCnt :: Lens' (BATStatus a) (Maybe Integer)
-- | Lens for the '_trappedCnt' field of BATStatus
trappedCnt :: Lens' (BATStatus a) (Maybe Integer)
-- | Lens for the '_lightDamageCnt' field of BATStatus
lightDamageCnt :: Lens' (BATStatus a) (Maybe Integer)
-- | Lens for the '_moderateDamageCnt' field of BATStatus
moderateDamageCnt :: Lens' (BATStatus a) (Maybe Integer)
-- | Lens for the '_heavyDamageCnt' field of BATStatus
heavyDamageCnt :: Lens' (BATStatus a) (Maybe Integer)
-- | Lens for the '_fireCnt' field of BATStatus
fireCnt :: Lens' (BATStatus a) (Maybe Integer)
-- | Lens for the '_electricHazardCnt' field of BATStatus
electricHazardCnt :: Lens' (BATStatus a) (Maybe Integer)
-- | Lens for the '_waterHazardCnt' field of BATStatus
waterHazardCnt :: Lens' (BATStatus a) (Maybe Integer)
-- | Lens for the '_gasHazardCnt' field of BATStatus
gasHazardCnt :: Lens' (BATStatus a) (Maybe Integer)
-- | Lens for the '_chemicalHazardCnt' field of BATStatus
chemicalHazardCnt :: Lens' (BATStatus a) (Maybe Integer)
-- | Lens for the '_roadBlocked' field of BATStatus
roadBlocked :: Lens' (BATStatus a) (Maybe Bool)

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
    where bodyFromMsgFmtWithFields fld fldE fldR _ _ _
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
  bodyToMsgFmt (LADamageBody s t) = MF.fromList [] list
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

-- | A type alias for zone numbers
type ZoneNum = Integer

-- | A way of representing a field sum, along with an account of how
-- many BATs have reported it.
data CoveredField a = CoveredField Int a deriving (Eq, Show)

data ZoneStatus = ZoneStatus { _zoneNum :: ZoneNum
                             , _numBATs :: Int
                             , _okayCnt :: CoveredField Integer
                             , _minorInjurySum :: CoveredField Integer
                             , _delayedInjurySum :: CoveredField Integer
                             , _immediateInjurySum :: CoveredField Integer
                             , _fatalitySum :: CoveredField Integer
                             , _missingSum :: CoveredField Integer
                             , _trappedSum :: CoveredField Integer
                             , _lightDamageSum :: CoveredField Integer
                             , _moderateDamageSum :: CoveredField Integer
                             , _heavyDamageSum :: CoveredField Integer
                             , _fireSum :: CoveredField Integer
                             , _electricHazardSum :: CoveredField Integer
                             , _waterHazardSum :: CoveredField Integer
                             , _gasHazardSum :: CoveredField Integer
                             , _chemicalHazardSum :: CoveredField Integer
                             , _roadBlockedCnt :: CoveredField Integer
                  } deriving (Eq, Show)

makeLensesWith lensRules ''ZoneStatus

-- | A mapping formalizing which BATs belong to which zones
data ZoneMapping = ZM { zonesToBats :: M.Map ZoneNum [BATNum]
                      , batsToZones :: M.Map BATNum ZoneNum
                   } deriving (Eq, Show)

-- | Create an empty ZoneMapping
empty :: ZoneMapping
empty = ZM M.empty M.empty

-- | Insert a zone/bat pair into a ZoneMapping
insert :: ZoneMapping -> ZoneNum -> BATNum -> ZoneMapping
insert (ZM z2b b2z) zn bn = ZM (M.insertWith (++) zn [bn] z2b) (M.insert bn zn b2z)

-- | Get a list of zones
zones :: ZoneMapping -> [ZoneNum]
zones (ZM z2b _) = M.keys z2b

-- | Get the zone associated with a BAT
zoneOfBAT :: ZoneMapping -> BATNum  -> Maybe ZoneNum
zoneOfBAT (ZM _ b2z) n = M.lookup n b2z

-- | Get the BATs associated with a zone
batsOfZone :: ZoneMapping -> ZoneNum -> Maybe [BATNum]
batsOfZone (ZM z2b _) n = M.lookup n z2b

-- Something like the BATStatusLens above, but which contains a lens
-- for a BAT and one for a zone --- to be used when implementing
-- rolling up BAT status reports into one zone status report.
data ZoneStatusLens = forall b c. Summable b => ZSL (forall a. Lens' (BATStatus a) (Maybe b))
                                                    (Lens' ZoneStatus (CoveredField Integer))

emptyZoneStatus :: ZoneNum -> Int -> ZoneStatus
emptyZoneStatus n b = ZoneStatus { _zoneNum = n
                                 , _numBATs = b
                                 , _okayCnt = CoveredField 0 0
                                 , _minorInjurySum = CoveredField 0 0
                                 , _delayedInjurySum = CoveredField 0 0
                                 , _immediateInjurySum = CoveredField 0 0
                                 , _fatalitySum = CoveredField 0 0
                                 , _missingSum = CoveredField 0 0
                                 , _trappedSum = CoveredField 0 0
                                 , _lightDamageSum = CoveredField 0 0
                                 , _moderateDamageSum = CoveredField 0 0
                                 , _heavyDamageSum = CoveredField 0 0
                                 , _fireSum = CoveredField 0 0
                                 , _electricHazardSum = CoveredField 0 0
                                 , _waterHazardSum = CoveredField 0 0
                                 , _gasHazardSum = CoveredField 0 0
                                 , _chemicalHazardSum = CoveredField 0 0
                                 , _roadBlockedCnt = CoveredField 0 0
                      }

class Summable a where
  sumIt :: a -> Integer -> Integer

instance Summable Integer where
  sumIt = (+)

instance Summable Bool where
  sumIt True x = x + 1
  sumIt False x = x

rollupZoneStatuses :: ZoneMapping -> [BATStatus Accumulated] -> M.Map ZoneNum ZoneStatus
rollupZoneStatuses zm bsl = M.fromList groupedBATs
  where groupedBATs = map (rollupZoneStatuses' . withKey) $ L.groupBy groupBats sortedBSL
        -- The various partial functions below cannot fail, as failure
        -- would require a missing BAT number or a missing entry in
        -- the ZoneMapping, and BATs that satisfy either of those
        -- conditions are filtered out of the list.
        groupBats ((^.number) -> Right n1) ((^.number) -> Right n2) = z1 == z2
          where z1 = M.lookup n1 (batsToZones zm)
                z2 = M.lookup n2 (batsToZones zm)
        withKey l@(((^.number) -> Right n1):_) = (batsToZones zm M.! n1,l)
        sortedBSL = L.sortBy cmpZones (filter hasZone bsl)
        cmpZones ((^.number) -> Right n1) ((^.number) -> Right n2) = compare n1 n2
        hasZone x = either (const False) (`M.member` batsToZones zm) $ x ^. number
        rollupZoneStatuses' :: (ZoneNum, [BATStatus Accumulated])
                            -> (ZoneNum, ZoneStatus)
        rollupZoneStatuses' (n,bs) = (n, rollupZoneStatuses'' n numBATs bs)
          where numBATs = length $ zonesToBats zm M.! (batsToZones zm M.! n)
        -- A private function for rolling up the zone statuses for a
        -- single zone; the first Integer it the number of BATs in
        -- this zone.  It is assumed that all of the BATs in the
        -- provided list belong to the same zone, and that no two BAT
        -- status reports are for the same BAT.  rollupZoneStatuses'
        -- :: Integer -> [BATStatus Accumulated] -> ZoneStatus
        rollupZoneStatuses'' :: ZoneNum -> Int -> [BATStatus Accumulated] -> ZoneStatus
        rollupZoneStatuses'' n b = L.foldl' addZoneStatuses (emptyZoneStatus n b)
        addZoneStatuses :: ZoneStatus -> BATStatus Accumulated -> ZoneStatus
        addZoneStatuses z b = L.foldl' (addFld b) z flds
        addFld :: BATStatus Accumulated -> ZoneStatus -> ZoneStatusLens -> ZoneStatus
        addFld b z (ZSL bl zl) = maybe z (\x -> z & zl %~ addZ x) $ b ^. bl
        addZ :: Summable b => b -> CoveredField Integer -> CoveredField Integer
        addZ x (CoveredField n v) = CoveredField (n + 1) (sumIt x v)
        flds = [ ZSL okay okayCnt, ZSL minorInjuryCnt minorInjurySum
               , ZSL delayedInjuryCnt delayedInjurySum
               , ZSL immediateInjuryCnt immediateInjurySum
               , ZSL fatalityCnt fatalitySum, ZSL missingCnt missingSum
               , ZSL trappedCnt trappedSum, ZSL lightDamageCnt lightDamageSum
               , ZSL moderateDamageCnt moderateDamageSum
               , ZSL heavyDamageCnt heavyDamageSum, ZSL fireCnt fireSum
               , ZSL electricHazardCnt electricHazardSum
               , ZSL waterHazardCnt waterHazardSum
               , ZSL gasHazardCnt gasHazardSum
               , ZSL chemicalHazardCnt chemicalHazardSum
               , ZSL roadBlocked roadBlockedCnt ]
