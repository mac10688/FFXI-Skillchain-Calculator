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
  $ [wtChoice wOne, wtChoice wTwo, wtChoice wThree, wtChoice wFour, wtChoice wFive, wtChoice wSix] ?? s

wtChoice :: Lens' UIState (List ChooseableWeaponTypes) -> UIState -> T.Widget Name
wtChoice lens s = F.withFocusRing (s^.focusRing) (L.renderList renderListItem) (s^.lens)

renderListItem :: (Show a) => Bool -> a -> T.Widget Name
renderListItem b i = if b then
                        C.str $ show i
                     else
                        C.str $ show i

createComboDisplay :: UIState -> T.Widget Name
createComboDisplay s =
    F.withFocusRing (s^.focusRing) (L.renderList renderListItem) (s^.displayCombo)
    
createComboRow :: D.SkillchainCombination -> T.Widget Name
createComboRow (D.ScStart ws cont) = C.hBox $ showWs ws <+> C.str " -> ": (createComboRow' cont)

createComboRow' :: D.ScContinuation -> [T.Widget Name]
createComboRow' (D.ScContinuation ws sc (Just continue)) = showWs ws <+> C.str " -> " <+> showSc sc <+> C.str " -> ": createComboRow' continue
createComboRow' (D.ScContinuation ws sc Nothing) = [showWs ws <+> C.str " -> " <+> showSc sc]

showWs :: D.Weaponskill -> T.Widget Name
showWs ws = C.withAttr (A.attrName "Red") $ C.str $ (show ws)

showSc :: D.Skillchain -> T.Widget Name
showSc sc = C.withAttr (A.attrName "Blue") $ C.str $ (show sc)

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
theMap= A.attrMap defAttr [(A.attrName "Red", red `on` black)
                          ,(A.attrName "Blue", blue `on` black)
                          ,(L.listSelectedAttr, white `on` black)
                          ,(L.listSelectedFocusedAttr, red `on` black)]

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


