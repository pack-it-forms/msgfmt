import Test.Tasty

import PackItForms.MsgFmtTests

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [ packItFormsMsgFmtTests ]
