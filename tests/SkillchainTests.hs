module Main (main) where

import Test.HUnit
import Skillchain
import Data.Map.Lazy as Map
import Data.List as List

combo = Weaponskill "Combo" HandToHand [Impaction]
shoulderTackle = Weaponskill "Shoulder Tackle" HandToHand [Reverberation, Impaction]
ragingFist = Weaponskill "Raging Fists" HandToHand [Impaction]
spinningAttack = Weaponskill "Spinning Attack" HandToHand [Liquefaction, Impaction]
howlingFist = Weaponskill "Howling Fist" HandToHand [Impaction, Transfixion]
shijinSpiral = Weaponskill "Shijin Spiral" HandToHand [Fusion, Reverberation]

impactionH2hKeyTest = TestCase $ assertBool 
  "Does (Impaction, HandToHande) key exist" $
  (Impaction, HandToHand) `member` createScMap

impactionH2hValueTest = TestCase $ assertBool
  "Impaction HandToHand has Combo, Shoulder Tackle, Raging Fists, Spinning Attack, Howling Fist" $ 
  isSubsequenceOf [combo, shoulderTackle, ragingFist, spinningAttack, howlingFist] 
  $ createScMap ! (Impaction, HandToHand) 

impactionH2hValueTestNot = TestCase $ assertBool
  "Impaction HandToHand does not have Shijin Spiral" $ 
  not $ elem shijinSpiral $ createScMap ! (Impaction, HandToHand) 

scMapTests = TestList  [ TestLabel "test1" impactionH2hKeyTest
                       , TestLabel "test2" impactionH2hValueTest
                       , TestLabel "test3" impactionH2hValueTestNot]

main = runTestTT $ scMapTests
