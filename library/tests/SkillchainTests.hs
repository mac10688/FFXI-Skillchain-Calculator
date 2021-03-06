module Main (main) where

import Test.HUnit
import Skillchain
import Data.Map.Lazy as Map
import Data.List as List
import Data.Set as Set
import Data.Maybe as Maybe
import SkillchainData as SkillchainData

main = runTestTT $ TestList [
  TestLabel "mayScFromScAttrToWs" $
    TestList [
               TestCase $ assertBool
               "Scission -> Spinning Axe -> Liquefaction"
               (mayScFromScAttrToWs Scission spinningAxe == Just Liquefaction),

               TestCase $ assertBool
               "Transfixion -> Combo -> Nothing"
               (mayScFromScAttrToWs Transfixion combo == Nothing)
             ], 
  TestLabel "mayScContFromScAttrWs" $
    TestList [
              TestCase $ assertBool
              "Scission -> Spinning Axe -> Liquefaction -> Spinning Axe -> Scission" $
              mayScContFromScAttrWs Scission [spinningAxe, spinningAxe] ==
              (Just 
              $ ScContinuation spinningAxe Liquefaction
              $ Just 
              $ ScContinuation spinningAxe Scission Nothing)
             ],
  TestLabel "mayScFromTwoWs" $
    TestList [
              TestCase $ assertBool
              "Spinning Axe -> Spinning Axe -> Scission" $
              mayScFromTwoWs spinningAxe spinningAxe == Just Scission
             ],
  TestLabel "allCombinations" $
    TestList [
                TestCase $ assertBool
                "[[1,2,3], [4,5], [6,7,8,9]]" $
                allCombinations [[1,2,3], [4,5], [6,7,8,9]] == 
                [ [1,4,6],
                  [1,4,7],
                  [1,4,8],
                  [1,4,9],
                  [1,5,6],
                  [1,5,7],
                  [1,5,8],
                  [1,5,9],
                  [2,4,6],
                  [2,4,7],
                  [2,4,8],
                  [2,4,9],
                  [2,5,6],
                  [2,5,7],
                  [2,5,8],
                  [2,5,9],
                  [3,4,6],
                  [3,4,7],
                  [3,4,8],
                  [3,4,9],
                  [3,5,6],
                  [3,5,7],
                  [3,5,8],
                  [3,5,9]
                ]
             ],
  TestLabel "getAllWs" $
    TestList [
                TestCase $ assertBool
                "getAllWs [Axe, Sword]" $
                getAllWs [Axe, Sword] ==
                  [ 
                    [ragingAxe, smashAxe, galeAxe, avalanchingAxe, spinningAxe, rampage, calamity, mistralAxe, decimation, boraAxe, ruinator, onslaught, cloudsplitter, primalRend]
                  , [fastBlade, burningBlade, redLotusBlade, flatBlade, shiningBlade, seraphBlade, circleBlade, spiritsWithin, vorpalBlade, swiftBlade, savageBlade, sanguineBlade, requiescat, knightsOfRound, chantDuCygne, deathBlossom, atonement, expiacion]
                  ]
             ]
  ]

