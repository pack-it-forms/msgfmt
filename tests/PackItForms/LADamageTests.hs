module PackItForms.LADamageTests (laDamageTests) where

import PackItForms.LADamage
import PackItForms.ParseUtils (FormatError(..))
import qualified PackItForms.ICS213 as ICS213
import PackItForms.ICS213Tests -- For instances

import Test.QuickCheck
import qualified Data.Text as T
import Control.Monad (liftM)
import Test.Tasty
import qualified Test.Tasty.QuickCheck as QC

laDamageTests :: TestTree
laDamageTests = testGroup "Tests for Los Altos Damage form messages"
  [localOption (QC.QuickCheckTests 10000)
   (QC.testProperty "LA Damage messages roundtrip through MsgFmt" prop_roundtrips)]

prop_roundtrips :: ICS213.Msg LADamageBody -> Property
prop_roundtrips m = m === ICS213.fromMsgFmt (ICS213.toMsgFmt m)

instance Arbitrary LADamageBody where
  arbitrary = LADamageBody <$> resize 10 arbitrary <*> (T.pack <$> arbitrary)

instance Arbitrary BATStatus where
  arbitrary = BATStatus <$> batno <*> arbitrary <*> arbitrary <*> arbitrary
                        <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                        <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                        <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                        <*> arbitrary
    where batno = oneof [liftM Right arbitrary]
