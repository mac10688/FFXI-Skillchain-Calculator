module Skillchain (
  SkillchainType(..), 
  scMap, 
  WeaponType(..), 
  Weaponskill(..), 
  createScMap
) where

import Data.Map.Lazy as Map

data SkillchainType =
    Transfixion
  | Compression
  | Liquefaction
  | Scission
  | Reverberation
  | Detonation
  | Induration
  | Impaction
  | Gravitation
  | Distortion
  | Fusion
  | Fragmentation
  | Light
  | Light2
  | Darkness
  | Darkness2
  | Radiance
  | Umbra
  deriving (Eq, Show, Enum, Ord, Bounded)

scMap :: Map SkillchainType [(SkillchainType, SkillchainType)]
scMap = fromList
  [
    (Liquefaction, [(Impaction, Liquefaction), (Scission, Liquefaction)])
  , (Impaction, [(Reverberation, Impaction), (Induration, Impaction)])
  , (Detonation, [(Impaction, Detonation), (Compression, Detonation), (Scission, Detonation)])
  , (Scission, [(Liquefaction, Scission), (Detonation, Scission)])
  , (Reverberation, [(Transfixion, Reverberation), (Scission, Reverberation)])
  , (Induration, [(Reverberation, Induration)])
  , (Compression, [(Induration, Compression)])
  , (Transfixion, [(Compression, Transfixion)])
  , (Fusion, [(Liquefaction, Impaction), (Distortion, Fusion)])
  , (Fragmentation, [(Induration, Reverberation), (Gravitation, Fragmentation)])
  , (Gravitation, [(Detonation, Compression), (Fusion, Gravitation)])
  , (Distortion, [(Transfixion, Scission), (Fragmentation, Distortion)])
  , (Light, [(Fusion, Fragmentation)])
  , (Light2, [(Light, Light)])
  , (Darkness, [(Gravitation, Distortion)])
  , (Darkness2, [(Darkness, Darkness)])
  ]

data WeaponType =
      HandToHand
    | Dagger
    | Sword
    | GreatSword
    | Axe
    | GreatAxe
    | Scythe
    | Polearm
    | Katana
    | GreatKatana
    | Club
    | Staff
    | Archery
    | Marksmanship
    deriving (Eq, Show, Enum, Ord)

data Weaponskill = Weaponskill { weaponskillName :: String
                               , weaponType :: WeaponType
                               , skillChainProperties :: [SkillchainType]
                               } deriving (Eq, Show, Ord)

weaponSkills :: [Weaponskill]
weaponSkills = [ Weaponskill "Combo" HandToHand [Impaction]
               , Weaponskill "Shoulder Tackle" HandToHand [Reverberation, Impaction]
               , Weaponskill "One Inch Punch" HandToHand [Compression]
               , Weaponskill "Backhand Blow" HandToHand [Detonation]
               , Weaponskill "Raging Fists" HandToHand [Impaction]
               , Weaponskill "Spinning Attack" HandToHand [Liquefaction, Impaction]
               , Weaponskill "Howling Fist" HandToHand [Impaction, Transfixion]
               , Weaponskill "Dragon Kick" HandToHand [Fragmentation]
               , Weaponskill "Asuran Fists" HandToHand [Gravitation, Liquefaction]
               , Weaponskill "Tornado Kick" HandToHand [Detonation, Induration, Impaction]
               , Weaponskill "Shijin Spiral" HandToHand [Fusion, Reverberation]
               , Weaponskill "Final Heaven" HandToHand [Light, Fusion]
               , Weaponskill "Victory Smite" HandToHand [Light, Fragmentation]
               , Weaponskill "Ascetic's Fury" HandToHand [Fusion, Transfixion]
               , Weaponskill "Stringing Pummel" HandToHand [Gravitation, Liquefaction]
               , Weaponskill "Wasp Sting" Dagger [Scission]
               , Weaponskill "Gust Slash" Dagger [Detonation]
               , Weaponskill "Shadowstitch" Dagger [Reverberation]
               , Weaponskill "Viper Bite" Dagger [Scission]
               , Weaponskill "Cyclone" Dagger [Detonation, Impaction]
               , Weaponskill "Energy Steal" Dagger []
               , Weaponskill "Energy Drain" Dagger []
               , Weaponskill "Dancing Edge" Dagger [Scission, Detonation]
               , Weaponskill "Shark Bite" Dagger [Fragmentation]
               , Weaponskill "Evisceration" Dagger [Gravitation, Transfixion]
               , Weaponskill "Aeolian Edge" Dagger [Scission, Detonation, Impaction]
               , Weaponskill "Exenterator" Dagger [Fragmentation, Scission]
               , Weaponskill "Mercy Stroke" Dagger [Darkness, Gravitation]
               , Weaponskill "Rudra's Storm" Dagger [Darkness, Distortion]
               , Weaponskill "Mandalic Stab" Dagger [Fusion, Compression]
               , Weaponskill "Mordant Rime" Dagger [Fragmentation, Distortion]
               , Weaponskill "Pyrrhic Kleos" Dagger [Distortion, Scission]
               , Weaponskill "Fast Blade" Sword [Scission]
               , Weaponskill "Burning Blade" Sword [Liquefaction]
               , Weaponskill "Red Lotus Blade" Sword [Liquefaction, Detonation]
               , Weaponskill "Flat Blade" Sword [Impaction]
               , Weaponskill "Shining Blade" Sword [Scission]
               , Weaponskill "Seraph Blade" Sword [Scission]
               , Weaponskill "Circle Blade" Sword [Reverberation, Impaction]
               , Weaponskill "Spirits Within" Sword []
               , Weaponskill "Vorpal Blade" Sword [Scission, Impaction]
               , Weaponskill "Swift Blade" Sword [Scission, Impaction]
               , Weaponskill "Savage Blade" Sword [Fragmentation, Scission]
               , Weaponskill "Sanguine Blade" Sword []
               , Weaponskill "Requiescat" Sword [Gravitation, Scission]
               , Weaponskill "Knights of Round" Sword [Light, Fusion]
               , Weaponskill "Chant du Cygne" Sword [Light, Distortion]
               , Weaponskill "Death Blossom" Sword [Fragmentation, Distortion]
               , Weaponskill "Atonement" Sword [Fusion, Reverberation]
               , Weaponskill "Expiacion" Sword [Distortion, Scission]
               ]

createScMap :: Map (SkillchainType, WeaponType) [Weaponskill]
createScMap = Prelude.foldr (\ ws@(Weaponskill n w scs) returnMap -> 
      let
        localMap = fromList $ Prelude.map (\sc -> ((sc, w), [ws])) scs
      in 
        unionWith (\lv rv -> head lv : rv ) localMap returnMap) Map.empty weaponSkills

helper :: [Maybe [(Weaponskill, Weaponskill, SkillchainType)]] -> Maybe [(Weaponskill, Weaponskill, SkillchainType)]
helper info = Prelude.foldr(\item accum -> case (item, accum) of
                                     (Just i, Just xs) -> Just $ i ++ xs
                                     (Nothing, Just xs) -> Just xs
                                     (Just i, Nothing) -> Just i
                                     otherwise -> Nothing
                      ) Nothing info

findAllScForWeapons :: WeaponType -> WeaponType -> Maybe [(Weaponskill, Weaponskill, SkillchainType)]
findAllScForWeapons w1 w2 = helper $ Prelude.map (\sc -> findSkillchain sc w1 w2) [(minBound :: SkillchainType)..]

findSkillchain :: SkillchainType -> WeaponType -> WeaponType -> Maybe [(Weaponskill, Weaponskill, SkillchainType)]
findSkillchain sc w1 w2 =
  case Map.lookup sc scMap of
    Nothing -> Nothing
    Just scs -> helper $ Prelude.map (\ scT -> findWeaponskillsForSc sc scT w1 w2) scs

findWeaponskillsForSc :: SkillchainType -> (SkillchainType, SkillchainType) -> WeaponType -> WeaponType -> Maybe [(Weaponskill, Weaponskill, SkillchainType)]
findWeaponskillsForSc sc (sc1, sc2) w1 w2 =
  let
    mwsc1 = createScMap !? (sc1, w1) 
    mwsc2 = createScMap !? (sc2, w2)
  in
    case (mwsc1, mwsc2) of
      (Just wsc1, Just wsc2) -> Just [(ws1, ws2, sc) | ws1 <- wsc1, ws2 <- wsc2]
      otherwise -> Nothing

