{-# LANGUAGE DeriveGeneric #-}

module SkillchainData where

import GHC.Generics as Generics
import Data.Aeson as Aeson
import Data.Map.Lazy as Map
import Data.Maybe as Maybe

data Skillchain =
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
  deriving (Eq, Show, Enum, Ord, Bounded, Generic, Read)

type SkillchainAttr = Skillchain

type SkillchainComposition = (SkillchainAttr, SkillchainAttr)

data SkillchainCombination = ScStart Weaponskill ScContinuation deriving (Eq)

scLength :: SkillchainCombination -> Int
scLength (ScStart _ cont) = length' 2 cont
  where
    length' accum (ScContinuation _ _ (Just cont)) = length' (accum +1) cont
    length' accum (ScContinuation _ _ Nothing) = accum

instance Show SkillchainCombination where
  show (ScStart ws continuation) = show ws ++ " -> " ++ show continuation

data ScContinuation = ScContinuation Weaponskill Skillchain (Maybe ScContinuation)
  deriving (Eq)

instance Show ScContinuation where
  show (ScContinuation ws sc Nothing) = show ws ++ " -> " ++ show sc ++ "\n"
  show (ScContinuation ws sc (Just continue)) = show ws ++ " -> " ++ show sc ++ " -> " ++ show continue

instance ToJSON Weaponskill
instance ToJSON Skillchain
instance ToJSON WeaponType

compositionsBySc :: Map Skillchain [SkillchainComposition]
compositionsBySc = fromList
  [
    (Liquefaction,  [ (Impaction, Liquefaction), (Scission, Liquefaction)])
  , (Impaction,     [ (Reverberation, Impaction), (Induration, Impaction)])
  , (Detonation,    [ (Impaction, Detonation), (Compression, Detonation), (Scission, Detonation)])
  , (Scission,      [ (Liquefaction, Scission), (Detonation, Scission)])
  , (Reverberation, [ (Transfixion, Reverberation), (Scission, Reverberation)])
  , (Induration,    [ (Reverberation, Induration)])
  , (Compression,   [ (Induration, Compression)])
  , (Transfixion,   [ (Compression, Transfixion)])
  , (Fusion,        [ (Liquefaction, Impaction), (Distortion, Fusion)])
  , (Fragmentation, [ (Induration, Reverberation), (Gravitation, Fragmentation)])
  , (Gravitation,   [ (Detonation, Compression), (Fusion, Gravitation)])
  , (Distortion,    [ (Transfixion, Scission), (Fragmentation, Distortion)])
  , (Light,         [ (Fusion, Fragmentation)])
  , (Light2,        [ (Light, Light)])
  , (Darkness,      [ (Gravitation, Distortion)])
  , (Darkness2,     [ (Darkness, Darkness)])
  ]

scByComposition :: Map SkillchainComposition Skillchain
scByComposition = fromList
  [
    ((Impaction, Liquefaction    ) , Liquefaction  )
  , ((Scission, Liquefaction     ) , Liquefaction  )
  , ((Reverberation, Impaction   ) , Impaction     )
  , ((Induration, Impaction      ) , Impaction     )
  , ((Impaction, Detonation      ) , Detonation    )
  , ((Compression, Detonation    ) , Detonation    )
  , ((Scission, Detonation       ) , Detonation    )
  , ((Liquefaction, Scission     ) , Scission      )
  , ((Detonation, Scission       ) , Scission      )
  , ((Transfixion, Reverberation ) , Reverberation )
  , ((Scission, Reverberation    ) , Reverberation )
  , ((Reverberation, Induration  ) , Induration    )
  , ((Induration, Compression    ) , Compression   )
  , ((Transfixion, Compression   ) , Compression   )
  , ((Compression, Transfixion   ) , Transfixion   )
  , ((Liquefaction, Impaction    ) , Fusion        )
  , ((Distortion, Fusion         ) , Fusion        )
  , ((Induration, Reverberation  ) , Fragmentation )
  , ((Gravitation, Fragmentation ) , Fragmentation )
  , ((Detonation, Compression    ) , Gravitation   )
  , ((Fusion, Gravitation        ) , Gravitation   )
  , ((Transfixion, Scission      ) , Distortion    )
  , ((Fragmentation, Distortion  ) , Distortion    )
  , ((Fusion, Fragmentation      ) , Light         )
  , ((Gravitation, Distortion    ) , Darkness      )
  , ((Light, Light               ) , Light2        )
  , ((Darkness, Darkness         ) , Darkness2     )
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
    deriving (Eq, Show, Enum, Ord, Bounded, Generic, Read)

allWeaponTypes :: [WeaponType]
allWeaponTypes = [(minBound :: WeaponType)..]

data Weaponskill = Weaponskill { weaponskillName :: String
                               , weaponType :: WeaponType
                               , skillchainAttributes :: [SkillchainAttr]
                               } deriving (Eq, Ord, Generic)

instance Show Weaponskill where
  show (Weaponskill name _ _) = name

combo           = Weaponskill "Combo" HandToHand [Impaction]
shoulderTackle  = Weaponskill "Shoulder Tackle" HandToHand [Reverberation, Impaction]
oneInchPunch    = Weaponskill "One Inch Punch" HandToHand [Compression]
backhandBlow    = Weaponskill "Backhand Blow" HandToHand [Detonation]
ragingFists     = Weaponskill "Raging Fists" HandToHand [Impaction]
spinningAttack  = Weaponskill "Spinning Attack" HandToHand [Liquefaction, Impaction]
howlingFist     = Weaponskill "Howling Fist" HandToHand [Impaction, Transfixion]
dragonKick      = Weaponskill "Dragon Kick" HandToHand [Fragmentation]
asuranFists     = Weaponskill "Asuran Fists" HandToHand [Gravitation, Liquefaction]
tornadoKick     = Weaponskill "Tornado Kick" HandToHand [Detonation, Induration, Impaction]
shijinSpiral    = Weaponskill "Shijin Spiral" HandToHand [Fusion, Reverberation]
finalHeaven     = Weaponskill "Final Heaven" HandToHand [Light, Fusion]
victorySmite    = Weaponskill "Victory Smite" HandToHand [Light, Fragmentation]
asceticsFury    = Weaponskill "Ascetic's Fury" HandToHand [Fusion, Transfixion]
stringingPummel = Weaponskill "Stringing Pummel" HandToHand [Gravitation, Liquefaction]
waspSting       = Weaponskill "Wasp Sting" Dagger [Scission]
gustSlash       = Weaponskill "Gust Slash" Dagger [Detonation]
shadowstitch    = Weaponskill "Shadowstitch" Dagger [Reverberation]
viperBite       = Weaponskill "Viper Bite" Dagger [Scission]
cyclone         = Weaponskill "Cyclone" Dagger [Detonation, Impaction]
energySteal     = Weaponskill "Energy Steal" Dagger []
energyDrain     = Weaponskill "Energy Drain" Dagger []
dancingEdge     = Weaponskill "Dancing Edge" Dagger [Scission, Detonation]
sharkBite       = Weaponskill "Shark Bite" Dagger [Fragmentation]
evisceration    = Weaponskill "Evisceration" Dagger [Gravitation, Transfixion]
aeolianEdge     = Weaponskill "Aeolian Edge" Dagger [Scission, Detonation, Impaction]
exenterator     = Weaponskill "Exenterator" Dagger [Fragmentation, Scission]
mercyStroke     = Weaponskill "Mercy Stroke" Dagger [Darkness, Gravitation]
rudraStorm      = Weaponskill "Rudra's Storm" Dagger [Darkness, Distortion]
mandalicStab    = Weaponskill "Mandalic Stab" Dagger [Fusion, Compression]
mordantRime     = Weaponskill "Mordant Rime" Dagger [Fragmentation, Distortion]
pyrrhicKleos    = Weaponskill "Pyrrhic Kleos" Dagger [Distortion, Scission]
fastBlade       = Weaponskill "Fast Blade" Sword [Scission]
burningBlade    = Weaponskill "Burning Blade" Sword [Liquefaction]
redLotusBlade   = Weaponskill "Red Lotus Blade" Sword [Liquefaction, Detonation]
flatBlade       = Weaponskill "Flat Blade" Sword [Impaction]
shiningBlade    = Weaponskill "Shining Blade" Sword [Scission]
seraphBlade     = Weaponskill "Seraph Blade" Sword [Scission]
circleBlade     = Weaponskill "Circle Blade" Sword [Reverberation, Impaction]
spiritsWithin   = Weaponskill "Spirits Within" Sword []
vorpalBlade     = Weaponskill "Vorpal Blade" Sword [Scission, Impaction]
swiftBlade      = Weaponskill "Swift Blade" Sword [Scission, Impaction]
savageBlade     = Weaponskill "Savage Blade" Sword [Fragmentation, Scission]
sanguineBlade   = Weaponskill "Sanguine Blade" Sword []
requiescat      = Weaponskill "Requiescat" Sword [Gravitation, Scission]
knightsOfRound  = Weaponskill "Knights of Round" Sword [Light, Fusion]
chantDuCygne    = Weaponskill "Chant du Cygne" Sword [Light, Distortion]
deathBlossom    = Weaponskill "Death Blossom" Sword [Fragmentation, Distortion]
atonement       = Weaponskill "Atonement" Sword [Fusion, Reverberation]
expiacion       = Weaponskill "Expiacion" Sword [Distortion, Scission]
hardSlash       = Weaponskill "Hard Slash" GreatSword [Scission]
powerSlash      = Weaponskill "Power Slash" GreatSword [Transfixion]
frostBite       = Weaponskill "Frostbite" GreatSword [Induration]
freezeBite      = Weaponskill "Freezebite" GreatSword [Detonation, Induration]
shockwavw       = Weaponskill "Shockwave" GreatSword [Reverberation]
crescentMoon    = Weaponskill "Crescent Moon" GreatSword [Scission]
sickleMoon      = Weaponskill "Sickle Moon" GreatSword [Scission, Impaction]
spinningSlash   = Weaponskill "Spinning Slash" GreatSword [Fragmentation]
groundStrike    = Weaponskill "Ground Strike" GreatSword [Fragmentation, Distortion]
herculeanSlash  = Weaponskill "Herculean Slash" GreatSword [Detonation, Induration, Impaction]
resolution      = Weaponskill "Resolution" GreatSword [Fragmentation, Scission]
scourge         = Weaponskill "Scourge" GreatSword [Light, Fusion]
torcleaver      = Weaponskill "Torcleaver" GreatSword [Light, Distortion]
dimidiation     = Weaponskill "Dimidiation" GreatSword [Light, Fragmentation]
ragingAxe       = Weaponskill "Raging Axe" Axe [Detonation, Impaction]
smashAxe        = Weaponskill "Smash Axe" Axe [Reverberation, Induration]
galeAxe         = Weaponskill "Gale Axe" Axe [Detonation]
avalanchingAxe  = Weaponskill "Avalanching Axe" Axe [Scission, Impaction]
spinningAxe     = Weaponskill "Spinning Axe" Axe [Liquefaction, Scission, Impaction]
rampage         = Weaponskill "Rampage" Axe [Scission]
calamity        = Weaponskill "Calamity" Axe [Scission, Impaction]
mistralAxe      = Weaponskill "Mistral Axe" Axe [Fusion]
decimation      = Weaponskill "Decimation" Axe [Fusion, Reverberation]
boraAxe         = Weaponskill "Bora Axe" Axe [Scission, Detonation]
ruinator        = Weaponskill "Ruinator" Axe [Distortion, Detonation]
onslaught       = Weaponskill "Onslaught" Axe [Darkness, Gravitation]
cloudsplitter   = Weaponskill "Cloudsplitter" Axe [Darkness, Fragmentation]
primalRend      = Weaponskill "Primal Rend" Axe [Gravitation, Reverberation]
shieldBreak     = Weaponskill "Shield Break" GreatAxe [Impaction]
ironTempest     = Weaponskill "Iron Tempest" GreatAxe [Scission]
sturmwind       = Weaponskill "Sturmwind" GreatAxe [Scission, Reverberation]
armorBreak      = Weaponskill "Armor Break" GreatAxe [Impaction]
keenEdge        = Weaponskill "Keen Edge" GreatAxe [Compression]
weaponBreak     = Weaponskill "Weapon Break" GreatAxe [Impaction]
ragingRush      = Weaponskill "Raging Rush" GreatAxe [Reverberation, Induration]
fullBreak       = Weaponskill "Full Break" GreatAxe [Distortion]
steelCyclone    = Weaponskill "Steel Cyclone" GreatAxe [Distortion, Detonation]
fellCleave      = Weaponskill "Fell Cleave" GreatAxe [Scission, Detonation]
upheaval        = Weaponskill "Upheaval" GreatAxe [Fusion, Compression]
metatronTorment = Weaponskill "Metatron Torment" GreatAxe [Light, Fusion]
ukkoFury        = Weaponskill "Ukko's Fury" GreatAxe [Light, Fusion]
kingJustice     = Weaponskill "King's Justice" GreatAxe [Fragmentation, Scission]
slice           = Weaponskill "Slice" Scythe [Scission]
darkHarvest     = Weaponskill "Dark Harvest" Scythe [Reverberation]
shadowOfDeath   = Weaponskill "Shadow of Death" Scythe [Reverberation, Induration]
nightmareScythe = Weaponskill "Nightmare Scythe" Scythe [Scission, Compression]
spinningScythe  = Weaponskill "Spinning Scythe" Scythe [Scission, Reverberation]
vorpalScythe    = Weaponskill "Vorpal Scythe" Scythe [Scission, Transfixion]
guillotine      = Weaponskill "Guillotine" Scythe [Induration]
crossReaper     = Weaponskill "Cross Reaper" Scythe [Distortion]
spiralHell      = Weaponskill "Spiral Hell" Scythe [Distortion, Scission]
infernalScythe  = Weaponskill "Infernal Scythe" Scythe [Reverberation, Compression]
entropy         = Weaponskill "Entropy" Scythe [Gravitation, Reverberation]
catastrophe     = Weaponskill "Catastrophe" Scythe [Darkness, Gravitation]
quietus         = Weaponskill "Quietus" Scythe [Darkness, Distortion]
insurgency      = Weaponskill "Insurgency" Scythe [Fusion, Compression]
doubleThrust    = Weaponskill "Double Thrust" Polearm [Transfixion]
thunderThrust   = Weaponskill "Thunder Thrust" Polearm [Impaction, Transfixion]
raidenThrust    = Weaponskill "Raiden Thrust" Polearm [Impaction, Transfixion]
legSweep        = Weaponskill "Leg Sweep" Polearm [Impaction]
pentaThrust     = Weaponskill "Penta Thrust" Polearm [Compression]
vorpalThust     = Weaponskill "Vorpal Thrust" Polearm [Reverberation, Transfixion]
skewer          = Weaponskill "Skewer" Polearm [Impaction, Transfixion]
wheelingThrust  = Weaponskill "Wheeling Thrust" Polearm [Fusion]
impulseDrive    = Weaponskill "Impulse Drive" Polearm [Gravitation, Induration]
sonicThrust     = Weaponskill "Sonic Thrust" Polearm [Scission, Transfixion]
stardiver       = Weaponskill "Stardiver" Polearm [Gravitation, Transfixion]
geirskogul      = Weaponskill "Geirskogul" Polearm [Light, Distortion]
camlannTorment  = Weaponskill "Camlann's Torment" Polearm [Light, Fragmentation]
drakesbane      = Weaponskill "Drakesbane" Polearm [Fusion, Transfixion]
bladeRin        = Weaponskill "Blade: Rin" Katana [Transfixion]
bladeRetsu      = Weaponskill "Blade: Retsu" Katana [Scission]
bladeTeki       = Weaponskill "Blade: Teki" Katana [Reverberation]
bladeTo         = Weaponskill "Blade: To" Katana [Detonation, Induration]
bladeChi        = Weaponskill "Blade: Chi" Katana [Impaction, Transfixion]
bladeEi         = Weaponskill "Blade: Ei" Katana [Compression]
bladeJin        = Weaponskill "Blade: Jin" Katana [Detonation, Impaction]
bladeTen        = Weaponskill "Blade: Ten" Katana [Gravitation]
bladeKu         = Weaponskill "Blade: Ku" Katana [Gravitation, Transfixion]
bladeYu         = Weaponskill "Blade: Yu" Katana [Scission, Reverberation]
bladeShun       = Weaponskill "Blade: Shun" Katana [Fusion, Impaction]
bladeMetsu      = Weaponskill "Blade: Metsu" Katana [Darkness, Fragmentation]
bladeHi         = Weaponskill "Blade: Hi" Katana [Darkness, Gravitation]
bladeKamu       = Weaponskill "Blade: Kamu" Katana [Fragmentation, Compression]
bladeEnpi       = Weaponskill "Tachi: Enpi" GreatKatana [Transfixion, Scission]
tachiHobaku     = Weaponskill "Tachi: Hobaku" GreatKatana [Induration]
tachiGoten      = Weaponskill "Tachi: Goten" GreatKatana [Transfixion, Impaction]
tachiKagero     = Weaponskill "Tachi: Kagero" GreatKatana [Liquefaction]
tachiJinpu      = Weaponskill "Tachi: Jinpu" GreatKatana [Scission, Detonation]
tachiKoki       = Weaponskill "Tachi: Koki" GreatKatana [Reverberation, Impaction]
tachiYukikaze   = Weaponskill "Tachi: Yukikaze" GreatKatana [Induration, Detonation]
tachiGekko      = Weaponskill "Tachi: Gekko" GreatKatana [Distortion, Reverberation]
tachiKasha      = Weaponskill "Tachi: Kasha" GreatKatana [Fusion, Compression]
tachiAgeha      = Weaponskill "Tachi: Ageha" GreatKatana [Compression, Scission]
tachiShoha      = Weaponskill "Tachi: Shoha" GreatKatana [Fragmentation, Compression]
tachiKaiten     = Weaponskill "Tachi: Kaiten" GreatKatana [Light, Fragmentation]
tachiFudo       = Weaponskill "Tachi: Fudo" GreatKatana [Light, Distortion]
tachiRana       = Weaponskill "Tachi: Rana" GreatKatana [Gravitation, Induration]
shiningStrike   = Weaponskill "Shining Strike" Club [Impaction]
seraphStrike    = Weaponskill "Seraph Strike" Club [Impaction]
brainshaker     = Weaponskill "Brainshaker" Club [Reverberation]
starlight       = Weaponskill "Starlight" Club []
moonlight       = Weaponskill "Moonlight" Club []
skullbreaker    = Weaponskill "Skullbreaker" Club [Induration, Reverberation]
trueStrike      = Weaponskill "True Strike" Club [Detonation, Impaction]
judgment        = Weaponskill "Judgment" Club [Impaction]
hexaStrike      = Weaponskill "Hexa Strike" Club [Fusion]
blackHalo       = Weaponskill "Black Halo" Club [Fragmentation, Compression]
flashNova       = Weaponskill "Flash Nova" Club [Reverberation, Induration]
realmrazer      = Weaponskill "Realmrazer" Club [Fusion, Impaction]
randgrith       = Weaponskill "Randgrith" Club [Light, Fragmentation]
dagan           = Weaponskill "Dagan" Club []
mysticBoon      = Weaponskill "Mystic Boon" Club []
exudation       = Weaponskill "Exudation" Club [Darkness, Fragmentation]
heavySwing      = Weaponskill "Heavy Swing" Staff [Impaction]
rockCrusher     = Weaponskill "Rock Crusher" Staff [Impaction]
earthCrusher    = Weaponskill "Earth Crusher" Staff [Detonation, Impaction]
starburst       = Weaponskill "Starburst" Staff [Reverberation, Compression]
sunburst        = Weaponskill "Sunburst" Staff [Reverberation, Compression]
shellCrusher    = Weaponskill "Shell Crusher" Staff [Detonation]
fullSwing       = Weaponskill "Full Swing" Staff [Liquefaction, Impaction]
spiritTaker     = Weaponskill "Spirit Taker" Staff []
retribution     = Weaponskill "Retribution" Staff [Gravitation, Reverberation]
cataclysm       = Weaponskill "Cataclysm" Staff [Reverberation, Compression]
shattersoul     = Weaponskill "Shattersoul" Staff [Gravitation, Induration]
gateOfTartarus  = Weaponskill "Gate of Tartarus" Staff [Darkness, Distortion]
myrkr           = Weaponskill "Myrkr" Staff []
vidohunir       = Weaponskill "Vidohunir" Staff [Fragmentation, Distortion]
garlandOfBliss  = Weaponskill "Garland of Bliss" Staff [Fusion, Reverberation]
omniscience     = Weaponskill "Omniscience" Staff [Gravitation, Transfixion]
flamingArrow    = Weaponskill "Flaming Arrow" Archery [Liquefaction, Transfixion]
piercingArrow   = Weaponskill "Piercing Arrow" Archery [Reverberation, Transfixion]
dullingArrow    = Weaponskill "Dulling Arrow" Archery [Liquefaction, Transfixion]
sidewinder      = Weaponskill "Sidewinder" Archery [Reverberation, Detonation, Transfixion]
blastArrow      = Weaponskill "Blast Arrow" Archery [Induration, Transfixion]
archingArrow    = Weaponskill "Arching Arrow" Archery [Fusion]
empyrealArrow   = Weaponskill "Empyreal Arrow" Archery [Fusion, Transfixion]
refulgentArrow  = Weaponskill "Refulgent Arrow" Archery [Reverberation, Transfixion]
apexArrow       = Weaponskill "Apex Arrow" Archery [Fragmentation, Transfixion]
namasArrow      = Weaponskill "Namas Arrow" Archery [Light, Distortion]
jishnuRadiance  = Weaponskill "Jishnu's Radiance" Archery [Light, Fusion]
hotShot         = Weaponskill "Hot Shot" Marksmanship [Liquefaction, Transfixion]
splitShot       = Weaponskill "Split Shot" Marksmanship [Reverberation, Transfixion]
sniperShot      = Weaponskill "Sniper Shot" Marksmanship [Liquefaction, Transfixion]
slugShot        = Weaponskill "Slug Shot" Marksmanship [Reverberation, Detonation, Transfixion]
blastShot       = Weaponskill "Blast Shot" Marksmanship [Induration, Transfixion]
heavyShot       = Weaponskill "Heavy Shot" Marksmanship [Fusion]
detonator       = Weaponskill "Detonator" Marksmanship [Fusion, Transfixion]
numbingShot     = Weaponskill "Numbing Shot" Marksmanship [Induration, Detonation, Impaction]
lastStand       = Weaponskill "Last Stand" Marksmanship [Fusion, Reverberation]
coronach        = Weaponskill "Coronach" Marksmanship [Darkness, Fragmentation]
wildfire        = Weaponskill "Wildfire" Marksmanship [Darkness, Gravitation]
trueflight      = Weaponskill "Trueflight" Marksmanship [Fragmentation, Scission]
leadenSalute    = Weaponskill "Leaden Salute" Marksmanship [Gravitation, Transfixion]


weaponSkills :: [Weaponskill]
weaponSkills = 
  [
    combo,
    shoulderTackle,
    oneInchPunch,
    backhandBlow,
    ragingFists,
    spinningAttack,
    howlingFist,
    dragonKick,
    asuranFists,
    tornadoKick,
    shijinSpiral,
    finalHeaven,
    victorySmite,
    asceticsFury,
    stringingPummel,
    waspSting,
    gustSlash,
    shadowstitch,
    viperBite,
    cyclone,
    energySteal,
    energyDrain,
    dancingEdge,
    sharkBite,
    evisceration,
    aeolianEdge,
    exenterator,
    mercyStroke,
    rudraStorm,
    mandalicStab,
    mordantRime,
    pyrrhicKleos,
    fastBlade,
    burningBlade,
    redLotusBlade,
    flatBlade,
    shiningBlade,
    seraphBlade,
    circleBlade,
    spiritsWithin,
    vorpalBlade,
    swiftBlade,
    savageBlade,
    sanguineBlade,
    requiescat,
    knightsOfRound,
    chantDuCygne,
    deathBlossom,
    atonement,
    expiacion,
    hardSlash,
    powerSlash,
    frostBite,
    freezeBite,
    shockwavw,
    crescentMoon,
    sickleMoon,
    spinningSlash,
    groundStrike,
    herculeanSlash,
    resolution,
    scourge,
    torcleaver,
    dimidiation,
    ragingAxe,
    smashAxe,
    galeAxe,
    avalanchingAxe,
    spinningAxe,
    rampage,
    calamity,
    mistralAxe,
    decimation,
    boraAxe,
    ruinator,
    onslaught,
    cloudsplitter,
    primalRend,
    shieldBreak,
    ironTempest,
    sturmwind,
    armorBreak,
    keenEdge,
    weaponBreak,
    ragingRush,
    fullBreak,
    steelCyclone,
    fellCleave,
    upheaval,
    metatronTorment,
    ukkoFury,
    kingJustice,
    slice,
    darkHarvest,
    shadowOfDeath,
    nightmareScythe,
    spinningScythe,
    vorpalScythe,
    guillotine,
    crossReaper,
    spiralHell,
    infernalScythe,
    entropy,
    catastrophe,
    quietus,
    insurgency,
    doubleThrust,
    thunderThrust,
    raidenThrust,
    legSweep,
    pentaThrust,
    vorpalThust,
    skewer,
    wheelingThrust,
    impulseDrive,
    sonicThrust,
    stardiver,
    geirskogul,
    camlannTorment,
    drakesbane,
    bladeRin,
    bladeRetsu,
    bladeTeki,
    bladeTo,
    bladeChi,
    bladeEi,
    bladeJin,
    bladeTen,
    bladeKu,
    bladeYu,
    bladeShun,
    bladeMetsu,
    bladeHi,
    bladeKamu,
    bladeEnpi,
    tachiHobaku,
    tachiGoten,
    tachiKagero,
    tachiJinpu,
    tachiKoki,
    tachiYukikaze,
    tachiGekko,
    tachiKasha,
    tachiAgeha,
    tachiShoha,
    tachiKaiten,
    tachiFudo,
    tachiRana,
    shiningStrike,
    seraphStrike,
    brainshaker,
    starlight,
    moonlight,
    skullbreaker,
    trueStrike,
    judgment,
    hexaStrike,
    blackHalo,
    flashNova,
    realmrazer,
    randgrith,
    dagan,
    mysticBoon,
    exudation,
    heavySwing,
    rockCrusher,
    earthCrusher,
    starburst,
    sunburst,
    shellCrusher,
    fullSwing,
    spiritTaker,
    retribution,
    cataclysm,
    shattersoul,
    gateOfTartarus,
    myrkr,
    vidohunir,
    garlandOfBliss,
    omniscience,
    flamingArrow,
    piercingArrow,
    dullingArrow,
    sidewinder,
    blastArrow,
    archingArrow,
    empyrealArrow,
    refulgentArrow,
    apexArrow,
    namasArrow,
    jishnuRadiance,
    hotShot,
    splitShot,
    sniperShot,
    slugShot,
    blastShot,
    heavyShot,
    detonator,
    numbingShot,
    lastStand,
    coronach,
    wildfire,
    trueflight,
    leadenSalute
  ]
