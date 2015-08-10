import Test.Tasty

import PackItForms.MsgFmtTests
import PackItForms.ICS213Tests

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [ packItFormsMsgFmtTests
                          , ics213Tests ]
