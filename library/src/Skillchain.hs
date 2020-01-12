{-# LANGUAGE FlexibleInstances #-}

module Skillchain (
  SkillchainAttr(..), 
  Skillchain(..),
  WeaponType(..), 
  Weaponskill(..), 
  SkillchainCombination(..),
  ScContinuation(..),
  scCombinations,
  getAllWs,
  getWs,
  allCombinations,
  weaponskillsByWeaponType,
  mayScFromTwoWsCombo,
  mayScFromTwoWs,
  mayScContFromScAttrWs,
  mayScFromScAttrToWs,
  scLength
) where

import Data.Map.Lazy as Map
import Data.List as List
import Data.Maybe as Maybe
import Data.List.Unique as Unique
import Safe as Safe
import SkillchainData
import Control.Parallel.Strategies

scCombinations :: [WeaponType] -> [SkillchainCombination]
scCombinations [] = []
scCombinations weapons = 
  let
    weaponskills = allCombinations $ getAllWs weapons 
  in
    catMaybes $ parMap rseq mayScFromTwoWsCombo weaponskills

getAllWs :: [WeaponType] -> [[Weaponskill]]
getAllWs weapons = List.foldr (\w acc -> getWs w : acc) [] weapons
    
getWs :: WeaponType -> [Weaponskill]
getWs weapon = findWithDefault [] weapon weaponskillsByWeaponType

allCombinations :: [[a]] -> [[a]]
allCombinations []       = [[]]
allCombinations (xs:xss) = [ x:xs_ | x <- xs, xs_ <- allCombinations xss ]

weaponskillsByWeaponType :: Map WeaponType [Weaponskill]
weaponskillsByWeaponType =
  Map.fromList 
  $ List.map (\w -> (w, List.filter (\ws -> (weaponType ws) == w) weaponSkills)) [(minBound :: WeaponType)..]
 
mayScFromTwoWsCombo :: [Weaponskill] -> Maybe SkillchainCombination
mayScFromTwoWsCombo allWs@(w1:w2:ws) = 
  let
    maybeSc = mayScFromTwoWs w1 w2
  in
    case maybeSc of
      Just sc' -> 
        let 
          scCombo = ScStart w1 $ ScContinuation w2 sc' $ mayScContFromScAttrWs sc' ws
        in
          if (length allWs == scLength scCombo)
          then Just $ scCombo
          else Nothing
      Nothing -> Nothing
mayScFromTwoWsCombo [] = Nothing

mayScFromTwoWs :: Weaponskill -> Weaponskill -> Maybe Skillchain
mayScFromTwoWs ws1 ws2 =
  let
    wsCombos = List.foldr (\key accum -> 
      case Map.lookup key scByComposition of
        Just sc -> sc : accum
        Nothing -> accum
      ) [] [(sc1, sc2) | sc1 <- skillchainAttributes ws1, sc2 <- skillchainAttributes ws2]
  in
    headMay wsCombos

mayScContFromScAttrWs :: SkillchainAttr -> [Weaponskill] -> Maybe ScContinuation
mayScContFromScAttrWs _ [] = Nothing
mayScContFromScAttrWs sc (w:ws) = let
    maybeSc = mayScFromScAttrToWs sc w 
  in
    case maybeSc of
      Just sc' -> Just $ ScContinuation w sc' $ mayScContFromScAttrWs sc' ws
      Nothing -> Nothing

mayScFromScAttrToWs :: SkillchainAttr -> Weaponskill -> Maybe Skillchain
mayScFromScAttrToWs sc ws = 
  headMay 
  $ catMaybes 
  $ List.map (\x -> Map.lookup (sc, x) scByComposition ) (skillchainAttributes ws)
