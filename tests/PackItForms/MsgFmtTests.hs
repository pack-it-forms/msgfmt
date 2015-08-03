{-# LANGUAGE OverloadedStrings #-}

module PackItForms.MsgFmtTests (packItFormsMsgFmtTests) where

import System.IO

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.QuickCheck.Property

import PackItForms.MsgFmt

import Data.String.Utils
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Text as T

-- 1: Filename
-- 2: Expected map
-- return: Assertion checking that they are true
packItFormsMsgFmtTest       :: String -> M.Map String String -> Assertion
packItFormsMsgFmtTest fn em = do
  let filename = ("tests/PackItForms/data/msgs/" ++ fn)
  c <- readFile filename
  let es = T.pack c
  let pfr = parse c
  es @=? getText pfr
  let MsgFmt m s = pfr
  es @=? s
  em @=? m
  pfpfr <- parseFile filename
  es @=? getText pfpfr
  let MsgFmt pfm pfs = pfpfr
  es @=? pfs
  em @=? pfm

da01test :: TestTree
da01test = testCase "Read form DA-01" $ packItFormsMsgFmtTest "DA-01" $
  M.fromList
    [ ("MsgNo","TST-42")
    , ("1a.","03/08/2014")
    , ("4.","OTHER")
    , ("5.","ROUTINE")
    , ("6a.","No")
    , ("6b.","No")
    , ("6c.","checked")
    , ("1b.","08:36:30")
    , ("7.","Logistics")
    , ("8.","Logistics")
    , ("9a.","ToLoc")
    , ("9b.","FromLoc")
    , ("ToName","ToName")
    , ("FmName","FromName")
    , ("ToTel","ToTelephone")
    , ("FmTel","FromTelephone")
    , ("10.","Damage Summary for Testing Town 0")
    , ("11.","RefNum6")
    , ("12.1a.","0")
    , ("12.1b.","1")
    , ("12.1c.","2")
    , ("12.1d.","3")
    , ("12.1e.","4")
    , ("12.1f.","5")
    , ("12.1g.","6")
    , ("12.1h.","7")
    , ("12.1i.","8")
    , ("12.1j.","9")
    , ("12.1k.","10")
    , ("12.1l.","11")
    , ("12.1m.","12")
    , ("12.1n.","13")
    , ("12.1o.","14")
    , ("12.1p.","15")
    , ("12.2r.","Loc1")
    , ("12.2a.","16")
    , ("12.2b.","17")
    , ("12.2c.","18")
    , ("12.2d.","19")
    , ("12.2e.","20")
    , ("12.2f.","21")
    , ("12.2g.","22")
    , ("12.2h.","23")
    , ("12.2i.","24")
    , ("12.2j.","25")
    , ("12.2k.","26")
    , ("12.2l.","27")
    , ("12.2m.","28")
    , ("12.2n.","29")
    , ("12.2o.","30")
    , ("12.2p.","31")
    , ("12.2q.","Notes 1")
    , ("12.3r.","Loc2")
    , ("12.3a.","32")
    , ("12.3b.","33")
    , ("12.3c.","34")
    , ("12.3d.","35")
    , ("12.3e.","36")
    , ("12.3f.","37")
    , ("12.3g.","38")
    , ("12.3h.","39")
    , ("12.3i.","40")
    , ("12.3j.","41")
    , ("12.3k.","42")
    , ("12.3l.","43")
    , ("12.3m.","44")
    , ("12.3n.","45")
    , ("12.3o.","46")
    , ("12.3p.","47")
    , ("12.3q.","Notes 3")
    , ("12.4r.","Loc3")
    , ("12.4a.","48")
    , ("12.4b.","49")
    , ("12.4c.","50")
    , ("12.4d.","51")
    , ("12.4e.","52")
    , ("12.4f.","53")
    , ("12.4g.","54")
    , ("12.4h.","55")
    , ("12.4i.","56")
    , ("12.4j.","57")
    , ("12.4k.","58")
    , ("12.4l.","59")
    , ("12.4m.","60")
    , ("12.4n.","61")
    , ("12.4o.","62")
    , ("12.4p.","63")
    , ("12.3q.","Notes 3")
    , ("12.4.","\\n\\nGenNotes\\nnewline\\\\n\\\\\\\\n")
    , ("13.","\\n\\nNOAT")
    , ("CCMgt","checked")
    , ("CCOps","checked")
    , ("Rec-Sent","Sent")
    , ("OpCall","KJ6PUN")
    , ("OpName","Peter A.")
    , ("OpDate","date")
    , ("OpTime","time")
    , ("Method","Other")
    , ("Other","Packet") ]

msg001test :: TestTree
msg001test = testCase "Read form MSG001" $ packItFormsMsgFmtTest "MSG001" $
  M.fromList
    [ ("MsgNo", "001")
    , ("1a.", "02/27/2014")
    , ("1b.", "1552")
    , ("4.", "OTHER")
    , ("5.", "IMMEDIATE")
    , ("6a.", "No")
    , ("6b.", "No")
    , ("7.", "Logistics")
    , ("9a.", "HOME")
    , ("8.", "Logistics")
    , ("9b.", "HOME")
    , ("10.", "TEST")
    , ("12.", "\\nTHIS IS A TEST MESSAGE FORM.  THIS ILLUSTRATES THE COMPLEXITY\n OF PACFORMS.")
    , ("Rec-Sent", "Sent")
    , ("Method", "Other")
    , ("Other", "Packet")
    , ("OpDate", "02/27/2014")
    , ("OpTime", "1555")
    ]

ts01test :: TestTree
ts01test = testCase "Read form TS01" $ packItFormsMsgFmtTest "TS01" $
  M.fromList
    [ ("2.", "TIF2")
    , ("MsgNo", "SCC001")
    , ("3.", "TIF3")
    , ("1a.", "03/03/2014")
    , ("1b.", "1741")
    , ("4.", "OTHER")
    , ("5.", "ROUTINE")
    , ("6a.", "No")
    , ("6b.", "Yes")
    , ("6c.", "checked")
    , ("6d.", "test]")
    , ("7.", "Logistics")
    , ("9a.", "Home (TF1)")
    , ("ToName", "Peter")
    , ("ToTel", "(000)-000-0000")
    , ("8.", "Logistics")
    , ("9b.", "TF1 (Home)")
    , ("FmName", "Peter")
    , ("FmTel", "(000)-000-0000")
    , ("10.", "Testing")
    , ("11.", "002")
    , ("12.", "\\nThis is a test message class 3.")
    , ("13.", "\\nNO ACTIONS WERE TAKEN")
    , ("CCMgt", "checked")
    , ("CCOps", "checked")
    , ("CCPlan", "checked")
    , ("CCLog", "checked")
    , ("CCFin", "checked")
    , ("Rec-Sent", "Received")
    , ("Method", "Other")
    , ("Other", "Packet")
    , ("OpCall", "KJ6PUN")
    , ("OpName", "Peter A.")
    , ("OpDate", "03/03/2014")
    , ("OpTime", "2032") ]

ts02test :: TestTree
ts02test = testCase "Read form TS02" $ packItFormsMsgFmtTest "TS02" $
  M.fromList
    [ ("2.", "TIF2")
    , ("MsgNo", "SCC001")
    , ("3.", "TIF3")
    , ("1a.", "03/03/2014")
    , ("1b.", "1741")
    , ("4.", "OTHER")
    , ("5.", "ROUTINE")
    , ("6a.", "No")
    , ("6b.", "Yes")
    , ("6c.", "checked")
    , ("6d.", "test")
    , ("7.", "Logistics")
    , ("9a.", "Home (TF1)")
    , ("ToName", "Peter")
    , ("ToTel", "(000)-000-0000")
    , ("8.", "Logistics")
    , ("9b.", "TF1 (Home)")
    , ("FmName", "Peter")
    , ("FmTel", "(000)-000-0000")
    , ("10.", "Testing")
    , ("11.", "002")
    , ("12.", "\\nThis is a testtest message class 3. this is a long\n mmessage.  stuff.  how long can messages get? \n testtestlong long long...\\n\\nThis is a second paragraph.\n  Two line breaks have been generated. \n Literal \\n:")
    , ("13.", "\\nNO ACTIONS WERE TAKEN")
    , ("CCMgt", "checked")
    , ("CCOps", "checked")
    , ("CCPlan", "checked")
    , ("CCLog", "checked")
    , ("CCFin", "checked")
    , ("Rec-Sent", "Sent")
    , ("Method", "Other")
    , ("Other", "Packet")
    , ("OpCall", "KJ6PUN")
    , ("OpName", "Peter A.")
    , ("OpDate", "03/04/2014")
    , ("OpTime", "0912") ]

emptytest :: TestTree
emptytest = testCase "Parse empty form" $ do
    let MsgFmt m s = parse ""
    "" @=? s
    M.empty @=? m

-- The second part of a MsgFmt should be the Text version of what was passed in; this is an attempt to ensure that.
secondPartIsInput :: TestTree
secondPartIsInput = localOption (QuickCheckTests 5000) $
  testProperty "Second part of MsgFmt is input" $
    \s -> let pfr              = parse s
              MsgFmt _ ns = pfr
          in (getText pfr == ns) && (ns == T.pack s)

verifyNonEmptyList :: [(String, String)] -> Bool
verifyNonEmptyList v = let ns = null . strip
                           vt = map (\(x,y) -> not (ns x || ns y)) v
                       in and vt

generateAndParseAreInversesProperty :: [(String,String)] -> Property
generateAndParseAreInversesProperty v = verifyNonEmptyList v ==>
  let MsgFmt gm gs = insertAll v emptyRep
      MsgFmt em es = parse $ T.unpack $ gs
      strippedV         = map (\(x,y) -> (strip x, strip y)) v
      reversedStrippedV = reverse strippedV
  in (em == M.fromList reversedStrippedV)
     && (gm == M.fromList strippedV)
     && (es == gs)

generateAndParseAreInverses :: TestTree
generateAndParseAreInverses = localOption (QuickCheckTests 5000) $
  testProperty "Generate and parse act as inverses" $
    generateAndParseAreInversesProperty

getValueIsValueProperty :: [(String,String)] -> Property
getValueIsValueProperty v = verifyNonEmptyList v ==>
   let p = insertAll v emptyRep
       stripKeys = map (\(x,y) -> (strip x, y))
   in and $ map (\(x,y) -> (fromMaybe "" . getValue p) x == (strip y)) $
                (M.toList . M.fromList . stripKeys) v

getValueIsValue :: TestTree
getValueIsValue = localOption (QuickCheckTests 5000) $
  testProperty "getValue returns the correct value" $
    getValueIsValueProperty

packItFormsMsgFmtTests :: TestTree
packItFormsMsgFmtTests = testGroup "Tests for PacFORMS representation parser"
  [ da01test, msg001test, ts01test, ts02test, emptytest,
  secondPartIsInput, generateAndParseAreInverses, getValueIsValue ]
