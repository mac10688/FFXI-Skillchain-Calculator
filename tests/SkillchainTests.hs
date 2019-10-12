module Main (main) where

import Test.HUnit
import Skillchain
import Data.Map.Lazy as Map
import Data.List as List
import Data.Set as Set
import Data.Maybe as Maybe

combo = Weaponskill "Combo" HandToHand [Impaction]
shoulderTackle = Weaponskill "Shoulder Tackle" HandToHand [Reverberation, Impaction]
ragingFist = Weaponskill "Raging Fists" HandToHand [Impaction]
spinningAttack = Weaponskill "Spinning Attack" HandToHand [Liquefaction, Impaction]
howlingFist = Weaponskill "Howling Fist" HandToHand [Impaction, Transfixion]
shijinSpiral = Weaponskill "Shijin Spiral" HandToHand [Fusion, Reverberation]

impactionH2hKeyTest = TestCase $ assertBool 
  "Does (Impaction, HandToHande) key exist" $
  (Impaction, HandToHand) `Map.member` createScMap

impactionH2hValueTest = TestCase $ assertBool
  "Impaction HandToHand has Combo, Shoulder Tackle, Raging Fists, Spinning Attack, Howling Fist" $ 
  isSubsequenceOf [combo, shoulderTackle, ragingFist, spinningAttack, howlingFist] 
  $ createScMap ! (Impaction, HandToHand) 

impactionH2hValueTestNot = TestCase $ assertBool
  "Impaction HandToHand does not have Shijin Spiral" $ 
  not $ elem shijinSpiral $ createScMap ! (Impaction, HandToHand) 


scMapTests = TestLabel "ScMapTests" $
  TestList  [ impactionH2hKeyTest
            , impactionH2hValueTest
            , impactionH2hValueTestNot]

everyWsCombo :: [(Weaponskill, Weaponskill)]
everyWsCombo = List.map (\(ws1, ws2, _) -> (ws1, ws2))
  $ concat 
  $ [findAllScForWeapons w1 w2 | w1 <- [(minBound :: WeaponType)..], w2 <- [(minBound :: WeaponType)..]]

noDuplicateSkillchains =
  let
    weaponSkills = everyWsCombo
    setToCompare = Set.fromList weaponSkills
  in 
  TestCase $ assertBool
  "No duplicate skillchains in all the weaponskills"
  (length weaponSkills == length setToCompare)


wsTests = TestLabel "Test Sc Results" $
  TestList [ noDuplicateSkillchains ]

main = runTestTT $ TestList [scMapTests, wsTests]
