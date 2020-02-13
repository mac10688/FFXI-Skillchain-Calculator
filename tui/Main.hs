{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Main where

import Brick.Main as Main
import Brick.Widgets.Core as C
import Brick.Types as T
import Brick.AttrMap as A
import Brick.Widgets.Border as B
import Brick.Util as U
import qualified Brick.Widgets.List as L
import qualified Brick.Focus as F

import Graphics.Vty.Attributes
import Graphics.Vty.Input.Events

import Control.Lens.Lens
import Control.Lens.TH
import Control.Lens.Setter
import Control.Lens.Getter

import Control.Monad as M
import Data.Text as Text
import Data.Vector as V

import SkillchainData as D
import Skillchain as SC

data Name = Wt1Field | Wt2Field | Wt3Field | Wt4Field | Wt5Field | Wt6Field | SkillchainComboList 
  deriving (Eq, Ord, Show)

data ChooseableWeaponTypes = NoWeapon | ChosenWeapon D.WeaponType deriving (Eq, Ord, Read)

instance Show ChooseableWeaponTypes where
  show NoWeapon = "No Weapon"
  show (ChosenWeapon w) = show w

allChooseableWeaponTypes :: [ChooseableWeaponTypes]
allChooseableWeaponTypes = NoWeapon : (ChosenWeapon <$> [(minBound :: D.WeaponType)..])

genericListWeaponTypes :: Name -> L.GenericList Name V.Vector ChooseableWeaponTypes
genericListWeaponTypes n = L.list n (V.fromList allChooseableWeaponTypes) 1

type List a = L.GenericList Name V.Vector a

data UIState = UIState
  {
   _wOne :: List ChooseableWeaponTypes
  ,_wTwo :: List ChooseableWeaponTypes
  ,_wThree :: List ChooseableWeaponTypes
  ,_wFour :: List ChooseableWeaponTypes
  ,_wFive :: List ChooseableWeaponTypes
  ,_wSix :: List ChooseableWeaponTypes
  ,_focusRing :: F.FocusRing Name
  ,_displayCombo :: List D.SkillchainCombination
  }

makeLenses ''UIState

drawUI :: UIState -> [T.Widget Name]
drawUI s = return $ C.vBox $ [createWeaponListBoxes, createComboDisplay] ?? s

createWeaponListBoxes :: UIState -> T.Widget Name
createWeaponListBoxes s = 
  C.vLimit 10 
  $ C.hBox 
  $ [ wtChoice "Weapon 1" wOne
    , wtChoice "Weapon 2" wTwo
    , wtChoice "Weapon 3" wThree
    , wtChoice "Weapon 4" wFour
    , wtChoice "Weapon 5" wFive
    , wtChoice "Weapon 6" wSix] ?? s

wtChoice :: String -> Lens' UIState (List ChooseableWeaponTypes) -> UIState -> T.Widget Name
wtChoice label lens s = 
  B.borderWithLabel (str label) 
  $ F.withFocusRing (s^.focusRing) (L.renderList renderListItem) (s^.lens)

renderCombo :: Bool -> D.SkillchainCombination -> T.Widget Name
renderCombo b sc = createComboRow sc

renderListItem :: (Show a) => Bool -> a -> T.Widget Name
renderListItem b i = if b then
                        C.str $ show i
                     else
                        C.str $ show i

createComboDisplay :: UIState -> T.Widget Name
createComboDisplay s = 
  B.borderWithLabel (str "Skillchain Combinations") 
  $ F.withFocusRing (s^.focusRing) (L.renderList renderCombo) (s^.displayCombo)
    
createComboRow :: D.SkillchainCombination -> T.Widget Name
createComboRow (D.ScStart ws cont) = C.hBox $ showWs ws <+> C.str " -> ": (createComboRow' cont)

createComboRow' :: D.ScContinuation -> [T.Widget Name]
createComboRow' (D.ScContinuation ws sc (Just continue)) = showWs ws <+> C.str " -> " <+> showSc sc <+> C.str " -> ": createComboRow' continue
createComboRow' (D.ScContinuation ws sc Nothing) = [showWs ws <+> C.str " -> " <+> showSc sc]

showWs :: D.Weaponskill -> T.Widget Name
showWs ws = C.withAttr (A.attrName "Weaponskill") $ C.str $ (show ws)

showSc :: D.Skillchain -> T.Widget Name
showSc sc = case sc of
              Liquefaction -> lvlOneSkillchain "Liquefaction"
              Impaction -> lvlOneSkillchain "Impaction"
              Detonation -> lvlOneSkillchain "Detonation"
              Scission -> lvlOneSkillchain "Scission"
              Reverberation -> lvlOneSkillchain "Reverberation"
              Induration -> lvlOneSkillchain "Induration"
              Compression -> lvlOneSkillchain "Compression"
              Transfixion -> lvlOneSkillchain "Transfixion"
              Fusion -> lvlTwoSkillchain "Liquefaction" "Impaction" "Fusion"
              Fragmentation -> lvlTwoSkillchain "Detonation" "Impaction" "Fragmentation"
              Gravitation -> lvlTwoSkillchain "Compression" "Scission" "Gravitation"
              Distortion -> lvlTwoSkillchain "Reverberation" "Induration" "Distortion"
              Light -> lvlOneSkillchain "Light"
              otherwise -> lvlOneSkillchain "Skillchain"
            where
              lvlOneSkillchain a = C.withAttr (A.attrName a) $ C.str $ show sc

              lvlTwoSkillchain :: String -> String -> String -> T.Widget Name
              lvlTwoSkillchain _ _ [] = emptyWidget
              lvlTwoSkillchain a b (x:xs) = (C.withAttr (A.attrName a) $ C.str $ [x]) <+> (lvlTwoSkillchain b a xs)
                
makeLabel :: String -> T.Widget Name -> T.Widget Name
makeLabel s = (<=>) $ C.padBottom (T.Pad 1) . C.padTop (T.Pad 1) $ (C.str s)

handleEvent :: UIState -> T.BrickEvent Name () -> T.EventM Name (T.Next UIState)
handleEvent s (VtyEvent (EvKey (KChar 'q') []))  = Main.halt s
handleEvent s (VtyEvent (EvKey (KChar '\t') [])) = Main.continue $ s & focusRing %~ F.focusNext
handleEvent s (VtyEvent (EvKey KBackTab [])) = Main.continue $ s & focusRing %~ F.focusPrev
handleEvent s (VtyEvent (EvKey KEnter [])) =
  let
    weaponTypes = getWeaponTypes [s^.wOne, s^.wTwo, s^.wThree, s^.wFour, s^.wFive, s^.wSix]
    skillchains = scCombinations weaponTypes
  in
  Main.continue $ s & displayCombo.~ (L.list SkillchainComboList (fromList skillchains) 1)
handleEvent s (T.VtyEvent ev) = do
  let currentFocus = F.focusGetCurrent $ s^.focusRing
  case currentFocus of
    Just name ->
      case name of
        Wt1Field -> do
          newList <- L.handleListEvent ev $ s^.wOne
          Main.continue $ s & wOne.~newList
        Wt2Field -> do
          newList <- L.handleListEvent ev $ s^.wTwo
          Main.continue $ s & wTwo.~newList
        Wt3Field -> do
          newList <- L.handleListEvent ev $ s^.wThree
          Main.continue $ s & wThree.~newList
        Wt4Field -> do
          newList <- L.handleListEvent ev $ s^.wFour
          Main.continue $ s & wFour.~newList
        Wt5Field -> do
          newList <- L.handleListEvent ev $ s^.wFive
          Main.continue $ s & wFive.~newList
        Wt6Field -> do
          newList <- L.handleListEvent ev $ s^.wSix
          Main.continue $ s & wSix.~newList
        SkillchainComboList -> do
          newList <- L.handleListEvent ev $ s^.displayCombo
          Main.continue $ s & displayCombo.~newList
    Nothing -> Main.continue s

handleEvent s ev = do
  Main.continue s

getWeaponTypes :: [List ChooseableWeaponTypes] -> [D.WeaponType]
getWeaponTypes list = [w | Just (_, ChosenWeapon w) <- L.listSelectedElement <$> list]

theMap :: A.AttrMap
theMap= A.attrMap defAttr [(A.attrName "Red", fg red)
                          ,(A.attrName "Blue", fg blue)
                          ,(A.attrName "Weaponskill", fg $ rgbColor 155 155 0)
                          ,(A.attrName "Skillchain", fg yellow)
                          ,(A.attrName "Liquefaction", fg $ rgbColor 255 0 0)
                          ,(A.attrName "Impaction", fg $ rgbColor 147 112 219)
                          ,(A.attrName "Detonation", fg $ rgbColor 50 205 50)
                          ,(A.attrName "Scission", fg $ rgbColor 184 134 11)
                          ,(A.attrName "Reverberation", fg $ rgbColor 0 206 209)
                          ,(A.attrName "Induration", fg $ rgbColor 0 255 255)
                          ,(A.attrName "Compression", fg $ rgbColor 90 90 90)
                          ,(A.attrName "Transfixion", fg $ rgbColor 200 200 200)
                          ,(A.attrName "Light", fg $ rgbColor 230 230 230)
                          ,(L.listSelectedAttr, fg white)
                          ,(L.listSelectedFocusedAttr, fg red)]

initUIState :: UIState
initUIState = UIState
  {
   _wOne = genericListWeaponTypes Wt1Field
  ,_wTwo = genericListWeaponTypes Wt2Field
  ,_wThree = genericListWeaponTypes Wt3Field
  ,_wFour = genericListWeaponTypes Wt4Field
  ,_wFive = genericListWeaponTypes Wt5Field
  ,_wSix = genericListWeaponTypes Wt6Field
  ,_focusRing = F.focusRing [Wt1Field, Wt2Field, Wt3Field, Wt4Field, Wt5Field, Wt6Field, SkillchainComboList]
  , _displayCombo = L.list SkillchainComboList (fromList []) 1
  }

app :: Main.App UIState () Name
app = Main.App  { appDraw = drawUI
                , appChooseCursor = neverShowCursor
                , appHandleEvent = handleEvent
                , appStartEvent = return
                , appAttrMap = const theMap
                }

main :: IO ()
main = do
  let state = initUIState
  void $ Main.defaultMain app state


