module Skillchain (
  SkillchainType(..), 
  scMap, 
  WeaponType(..), 
  Weaponskill(..), 
  createScMap,
  everyWsCombo
) where

import Data.Map.Lazy as Map
import Data.List as List
import Data.Maybe as Maybe
import Data.List.Unique as Unique

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

--lvl1Sc :: Set SkillchainType
--lvl1Sc = fromList
  --[
    --Compression
  --, Detonation
  --, Liquefaction
  --, Impaction
  --, Induration
  --, Reverberation
  --, Scission
  --, Transfixion
  --]

--lvl2Sc :: Set SkillchainType
--lvl2Sc = fromList
  --[
    --Distortion
  --, Fragmentation
  --, Fusion
  --, Gravitation
  --]

--lvl3Sc :: Set SkillchainType
--lvl3Sc = fromList
  --[
    --Darkness
  --, Light
  --]

--ultimateSc :: Set SkillchainType
--ulimateSc = fromList
  --[
    --Darkness2
  --, Light2
  --]

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

scMap' :: Map (SkillchainType, SkillchainType) SkillchainType
scMap' = fromList
  [
    ((Impaction, Liquefaction), Liquefaction)
  , ((Scission, Liquefaction), Liquefaction)
  , ((Reverberation, Impaction), Impaction)
  , ((Induration, Impaction), Impaction)
  , ((Impaction, Detonation), Detonation)
  , ((Compression, Detonation), Detonation)
  , ((Scission, Detonation), Detonation)
  , ((Liquefaction, Scission), Scission)
  , ((Detonation, Scission), Scission)
  , ((Transfixion, Reverberation), Reverberation)
  , ((Scission, Reverberation), Reverberation)
  , ((Reverberation, Induration), Induration)
  , ((Induration, Compression), Compression)
  , ((Transfixion, Compression), Compression)
  , ((Compression, Transfixion), Transfixion)
  , ((Liquefaction, Impaction), Fusion)
  , ((Distortion, Fusion), Fusion)
  , ((Induration, Reverberation), Fragmentation)
  , ((Gravitation, Fragmentation), Fragmentation)
  , ((Detonation, Compression), Gravitation)
  , ((Fusion, Gravitation), Gravitation)
  , ((Transfixion, Scission), Distortion)
  , ((Fragmentation, Distortion), Distortion)
  , ((Fusion, Fragmentation), Light)
  , ((Gravitation, Distortion), Darkness)
  , ((Light, Light), Light2)
  , ((Darkness, Darkness), Darkness2)
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
    deriving (Eq, Show, Enum, Ord, Bounded)

data Weaponskill = Weaponskill { weaponskillName :: String
                               , weaponType :: WeaponType
                               , skillchainProperties :: [SkillchainType]
                               } deriving (Eq, Ord)

instance Show Weaponskill where
  show (Weaponskill name _ _) = show name

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
               , Weaponskill "Hard Slash" GreatSword [Scission]
               , Weaponskill "Power Slash" GreatSword [Transfixion]
               , Weaponskill "Frostbite" GreatSword [Induration]
               , Weaponskill "Freezebite" GreatSword [Detonation, Induration]
               , Weaponskill "Shockwave" GreatSword [Reverberation]
               , Weaponskill "Crescent Moon" GreatSword [Scission]
               , Weaponskill "Sickle Moon" GreatSword [Scission, Impaction]
               , Weaponskill "Spinning Slash" GreatSword [Fragmentation]
               , Weaponskill "Ground Strike" GreatSword [Fragmentation, Distortion]
               , Weaponskill "Herculean Slash" GreatSword [Detonation, Induration, Impaction]
               , Weaponskill "Resolution" GreatSword [Fragmentation, Scission]
               , Weaponskill "Scourge" GreatSword [Light, Fusion]
               , Weaponskill "Torcleaver" GreatSword [Light, Distortion]
               , Weaponskill "Dimidiation" GreatSword [Light, Fragmentation]
               , Weaponskill "Raging Axe" Axe [Detonation, Impaction]
               , Weaponskill "Smash Axe" Axe [Reverberation, Induration]
               , Weaponskill "Gale Axe" Axe [Detonation]
               , Weaponskill "Avalanching Axe" Axe [Scission, Impaction]
               , Weaponskill "Spinning Axe" Axe [Liquefaction, Scission, Impaction]
               , Weaponskill "Rampage" Axe [Scission]
               , Weaponskill "Calamity" Axe [Scission, Impaction]
               , Weaponskill "Mistral Axe" Axe [Fusion]
               , Weaponskill "Decimation" Axe [Fusion, Reverberation]
               , Weaponskill "Bora Axe" Axe [Scission, Detonation]
               , Weaponskill "Ruinator" Axe [Distortion, Detonation]
               , Weaponskill "Onslaught" Axe [Darkness, Gravitation]
               , Weaponskill "Cloudsplitter" Axe [Darkness, Fragmentation]
               , Weaponskill "Primal Rend" Axe [Gravitation, Reverberation]
               , Weaponskill "Shield Break" GreatAxe [Impaction]
               , Weaponskill "Iron Tempest" GreatAxe [Scission]
               , Weaponskill "Sturmwind" GreatAxe [Scission, Reverberation]
               , Weaponskill "Armor Break" GreatAxe [Impaction]
               , Weaponskill "Keen Edge" GreatAxe [Compression]
               , Weaponskill "Weapon Break" GreatAxe [Impaction]
               , Weaponskill "Raging Rush" GreatAxe [Reverberation, Induration]
               , Weaponskill "Full Break" GreatAxe [Distortion]
               , Weaponskill "Steel Cyclone" GreatAxe [Distortion, Detonation]
               , Weaponskill "Fell Cleave" GreatAxe [Scission, Detonation]
               , Weaponskill "Upheaval" GreatAxe [Fusion, Compression]
               , Weaponskill "Metatron Torment" GreatAxe [Light, Fusion]
               , Weaponskill "Ukko's Fury" GreatAxe [Light, Fusion]
               , Weaponskill "King's Justice" GreatAxe [Fragmentation, Scission]
               , Weaponskill "Slice" Scythe [Scission]
               , Weaponskill "Dark Harvest" Scythe [Reverberation]
               , Weaponskill "Shadow of Death" Scythe [Reverberation, Induration]
               , Weaponskill "Nightmare Scythe" Scythe [Scission, Compression]
               , Weaponskill "Spinning Scythe" Scythe [Scission, Reverberation]
               , Weaponskill "Vorpal Scythe" Scythe [Scission, Transfixion]
               , Weaponskill "Guillotine" Scythe [Induration]
               , Weaponskill "Cross Reaper" Scythe [Distortion]
               , Weaponskill "Spiral Hell" Scythe [Distortion, Scission]
               , Weaponskill "Infernal Scythe" Scythe [Reverberation, Compression]
               , Weaponskill "Entropy" Scythe [Gravitation, Reverberation]
               , Weaponskill "Catastrophe" Scythe [Darkness, Gravitation]
               , Weaponskill "Quietus" Scythe [Darkness, Distortion]
               , Weaponskill "Insurgency" Scythe [Fusion, Compression]
               , Weaponskill "Double Thrust" Polearm [Transfixion]
               , Weaponskill "Thunder Thrust" Polearm [Impaction, Transfixion]
               , Weaponskill "Raiden Thrust" Polearm [Impaction, Transfixion]
               , Weaponskill "Leg Sweep" Polearm [Impaction]
               , Weaponskill "Penta Thrust" Polearm [Compression]
               , Weaponskill "Vorpal Thrust" Polearm [Reverberation, Transfixion]
               , Weaponskill "Skewer" Polearm [Impaction, Transfixion]
               , Weaponskill "Wheeling Thrust" Polearm [Fusion]
               , Weaponskill "Impulse Drive" Polearm [Gravitation, Induration]
               , Weaponskill "Sonic Thrust" Polearm [Scission, Transfixion]
               , Weaponskill "Stardiver" Polearm [Gravitation, Transfixion]
               , Weaponskill "Geirskogul" Polearm [Light, Distortion]
               , Weaponskill "Camlann's Torment" Polearm [Light, Fragmentation]
               , Weaponskill "Drakesbane" Polearm [Fusion, Transfixion]
               , Weaponskill "Blade: Rin" Katana [Transfixion]
               , Weaponskill "Blade: Retsu" Katana [Scission]
               , Weaponskill "Blade: Teki" Katana [Reverberation]
               , Weaponskill "Blade: To" Katana [Detonation, Induration]
               , Weaponskill "Blade: Chi" Katana [Impaction, Transfixion]
               , Weaponskill "Blade: Ei" Katana [Compression]
               , Weaponskill "Blade: Jin" Katana [Detonation, Impaction]
               , Weaponskill "Blade: Ten" Katana [Gravitation]
               , Weaponskill "Blade: Ku" Katana [Gravitation, Transfixion]
               , Weaponskill "Blade: Yu" Katana [Scission, Reverberation]
               , Weaponskill "Blade: Shun" Katana [Fusion, Impaction]
               , Weaponskill "Blade: Metsu" Katana [Darkness, Fragmentation]
               , Weaponskill "Blade: Hi" Katana [Darkness, Gravitation]
               , Weaponskill "Blade: Kamu" Katana [Fragmentation, Compression]
               , Weaponskill "Tachi: Enpi" GreatKatana [Transfixion, Scission]
               , Weaponskill "Tachi: Hobaku" GreatKatana [Induration]
               , Weaponskill "Tachi: Goten" GreatKatana [Transfixion, Impaction]
               , Weaponskill "Tachi: Kagero" GreatKatana [Liquefaction]
               , Weaponskill "Tachi: Jinpu" GreatKatana [Scission, Detonation]
               , Weaponskill "Tachi: Koki" GreatKatana [Reverberation, Impaction]
               , Weaponskill "Tachi: Yukikaze" GreatKatana [Induration, Detonation]
               , Weaponskill "Tachi: Gekko" GreatKatana [Distortion, Reverberation]
               , Weaponskill "Tachi: Kasha" GreatKatana [Fusion, Compression]
               , Weaponskill "Tachi: Ageha" GreatKatana [Compression, Scission]
               , Weaponskill "Tachi: Shoha" GreatKatana [Fragmentation, Compression]
               , Weaponskill "Tachi: Kaiten" GreatKatana [Light, Fragmentation]
               , Weaponskill "Tachi: Fudo" GreatKatana [Light, Distortion]
               , Weaponskill "Tachi: Rana" GreatKatana [Gravitation, Induration]
               , Weaponskill "Shining Strike" Club [Impaction]
               , Weaponskill "Seraph Strike" Club [Impaction]
               , Weaponskill "Brainshaker" Club [Reverberation]
               , Weaponskill "Starlight" Club []
               , Weaponskill "Moonlight" Club []
               , Weaponskill "Skullbreaker" Club [Induration, Reverberation]
               , Weaponskill "True Strike" Club [Detonation, Impaction]
               , Weaponskill "Judgment" Club [Impaction]
               , Weaponskill "Hexa Strike" Club [Fusion]
               , Weaponskill "Black Halo" Club [Fragmentation, Compression]
               , Weaponskill "Flash Nova" Club [Reverberation, Induration]
               , Weaponskill "Realmrazer" Club [Fusion, Impaction]
               , Weaponskill "Randgrith" Club [Light, Fragmentation]
               , Weaponskill "Dagan" Club []
               , Weaponskill "Mystic Boon" Club []
               , Weaponskill "Exudation" Club [Darkness, Fragmentation]
               , Weaponskill "Heavy Swing" Staff [Impaction]
               , Weaponskill "Rock Crusher" Staff [Impaction]
               , Weaponskill "Earth Crusher" Staff [Detonation, Impaction]
               , Weaponskill "Starburst" Staff [Reverberation, Compression]
               , Weaponskill "Sunburst" Staff [Reverberation, Compression]
               , Weaponskill "Shell Crusher" Staff [Detonation]
               , Weaponskill "Full Swing" Staff [Liquefaction, Impaction]
               , Weaponskill "Spirit Taker" Staff []
               , Weaponskill "Retribution" Staff [Gravitation, Reverberation]
               , Weaponskill "Cataclysm" Staff [Reverberation, Compression]
               , Weaponskill "Shattersoul" Staff [Gravitation, Induration]
               , Weaponskill "Gate of Tartarus" Staff [Darkness, Distortion]
               , Weaponskill "Myrkr" Staff []
               , Weaponskill "Vidohunir" Staff [Fragmentation, Distortion]
               , Weaponskill "Garland of Bliss" Staff [Fusion, Reverberation]
               , Weaponskill "Omniscience" Staff [Gravitation, Transfixion]
               , Weaponskill "Flaming Arrow" Archery [Liquefaction, Transfixion]
               , Weaponskill "Piercing Arrow" Archery [Reverberation, Transfixion]
               , Weaponskill "Dulling Arrow" Archery [Liquefaction, Transfixion]
               , Weaponskill "Sidewinder" Archery [Reverberation, Detonation, Transfixion]
               , Weaponskill "Blast Arrow" Archery [Induration, Transfixion]
               , Weaponskill "Arching Arrow" Archery [Fusion]
               , Weaponskill "Empyreal Arrow" Archery [Fusion, Transfixion]
               , Weaponskill "Refulgent Arrow" Archery [Reverberation, Transfixion]
               , Weaponskill "Apex Arrow" Archery [Fragmentation, Transfixion]
               , Weaponskill "Namas Arrow" Archery [Light, Distortion]
               , Weaponskill "Jishnu's Radiance" Archery [Light, Fusion]
               , Weaponskill "Hot Shot" Marksmanship [Liquefaction, Transfixion]
               , Weaponskill "Split Shot" Marksmanship [Reverberation, Transfixion]
               , Weaponskill "Sniper Shot" Marksmanship [Liquefaction, Transfixion]
               , Weaponskill "Slug Shot" Marksmanship [Reverberation, Detonation, Transfixion]
               , Weaponskill "Blast Shot" Marksmanship [Induration, Transfixion]
               , Weaponskill "Heavy Shot" Marksmanship [Fusion]
               , Weaponskill "Detonator" Marksmanship [Fusion, Transfixion]
               , Weaponskill "Numbing Shot" Marksmanship [Induration, Detonation, Impaction]
               , Weaponskill "Last Stand" Marksmanship [Fusion, Reverberation]
               , Weaponskill "Coronach" Marksmanship [Darkness, Fragmentation]
               , Weaponskill "Wildfire" Marksmanship [Darkness, Gravitation]
               , Weaponskill "Trueflight" Marksmanship [Fragmentation, Scission]
               , Weaponskill "Leaden Salute" Marksmanship [Gravitation, Transfixion]
               ]

createScMap :: Map (SkillchainType, WeaponType) [Weaponskill]
createScMap = Prelude.foldr (\ ws@(Weaponskill n w scs) returnMap -> 
      let
        localMap = fromList $ Prelude.map (\sc -> ((sc, w), [ws])) scs
      in 
        unionWith (\lv rv -> head lv : rv ) localMap returnMap) Map.empty weaponSkills

findAllScForWeapons :: WeaponType -> WeaponType -> [(Weaponskill, Weaponskill, SkillchainType)]
findAllScForWeapons w1 w2 = concat $ Prelude.map (\sc -> findSkillchain sc w1 w2) [(minBound :: SkillchainType)..]

howManyWsBetweenWeapons :: [(WeaponType, WeaponType, Int)]
howManyWsBetweenWeapons = List.sortOn (\(_, _, x) -> x) $ [(w1, w2, length $ findAllScForWeapons w1 w2) 
  | w1 <- [(minBound :: WeaponType)..], w2 <- [(minBound :: WeaponType)..]]

everyWsCombo :: [(Weaponskill, Weaponskill, SkillchainType)]
everyWsCombo = concat $ [findAllScForWeapons w1 w2 | w1 <- [(minBound :: WeaponType)..], w2 <- [(minBound :: WeaponType)..]]

everyWsCombo' :: [(Weaponskill, Weaponskill)]
everyWsCombo' = fmap (\(ws1, ws2, _) -> (ws1, ws2)) everyWsCombo

duplicateWs :: [(Weaponskill, Weaponskill, SkillchainType)]
duplicateWs = repeated everyWsCombo

findSkillchain :: SkillchainType -> WeaponType -> WeaponType -> [(Weaponskill, Weaponskill, SkillchainType)]
findSkillchain sc w1 w2 =
  case Map.lookup sc scMap of
    Nothing -> []
    Just scs -> unique $ concat $ Prelude.map (\ scT -> findWeaponskillsForSc sc scT w1 w2) scs

findWeaponskillsForSc :: SkillchainType -> (SkillchainType, SkillchainType) -> WeaponType -> WeaponType -> [(Weaponskill, Weaponskill, SkillchainType)]
findWeaponskillsForSc sc (sc1, sc2) w1 w2 =
  let
    mwsc1 = createScMap !? (sc1, w1) 
    mwsc2 = createScMap !? (sc2, w2)
  in
    case (mwsc1, mwsc2) of
      (Just wsc1, Just wsc2) -> List.filter (\(ws1, ws2, sc') ->
                                              case findSkillchainForWs ws1 ws2 of
                                                Just sc' -> sc == sc'
                                                Nothing -> False
                                            )
                                $ [(ws1, ws2, sc) | ws1 <- wsc1, ws2 <- wsc2]
      otherwise -> []
    
findSkillchainForWs :: Weaponskill -> Weaponskill -> Maybe SkillchainType
findSkillchainForWs ws1 ws2 =
  let
    allPossibleSc = [(sc1, sc2) | sc1 <- skillchainProperties ws1, sc2 <- skillchainProperties ws2]
    x =  List.foldr (\key accum -> case Map.lookup key scMap' of
                                      Just sc -> sc : accum
                                      Nothing -> accum
                    ) [] allPossibleSc

  in
    case x of
      [] -> Nothing
      xs -> Just $ maximum x
  

