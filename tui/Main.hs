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
  $ F.withFocusRing (s^.focusRing) (L.renderList renderWeaponType) (s^.lens)

renderCombo :: Bool -> D.SkillchainCombination -> T.Widget Name
renderCombo b sc = createComboRow sc

renderWeaponType :: Bool -> ChooseableWeaponTypes -> T.Widget Name
renderWeaponType _ i = C.str $ show i

createComboDisplay :: UIState -> T.Widget Name
createComboDisplay s = 
  B.borderWithLabel (str "Skillchain Combinations") 
  $ F.withFocusRing (s^.focusRing) (L.renderList renderCombo) (s^.displayCombo)
    
createComboRow :: D.SkillchainCombination -> T.Widget Name
createComboRow (D.ScStart ws cont) = C.hBox $ showWs ws <+> C.str " -> ": (createComboRow' cont)

createComboRow' :: D.ScContinuation -> [T.Widget Name]
createComboRow' (D.ScContinuation ws sc (Just continue)) = showWsAndSc ws sc <+> C.str " -> " : createComboRow' continue
createComboRow' (D.ScContinuation ws sc Nothing) = [showWsAndSc ws sc]

showWsAndSc :: D.Weaponskill -> D.Skillchain -> T.Widget Name
showWsAndSc ws sc = showWs ws <+> C.str " -> " <+> showSc sc

showWs :: D.Weaponskill -> T.Widget Name
showWs ws = C.withAttr (A.attrName "Weaponskill") $ C.str $ (show ws)

showSc :: D.Skillchain -> T.Widget Name
showSc sc = case sc of
              Liquefaction -> createOneColorWidget "Liquefaction"
              Impaction -> createOneColorWidget "Impaction"
              Detonation -> createOneColorWidget "Detonation"
              Scission -> createOneColorWidget "Scission"
              Reverberation -> createOneColorWidget "Reverberation"
              Induration -> createOneColorWidget "Induration"
              Compression -> createOneColorWidget "Compression"
              Transfixion -> createOneColorWidget "Transfixion"
              Fusion -> createAlternateColorWidget "Liquefaction" "Impaction" "Fusion"
              Fragmentation -> createAlternateColorWidget "Detonation" "Impaction" "Fragmentation"
              Gravitation -> createAlternateColorWidget "Compression" "Scission" "Gravitation"
              Distortion -> createAlternateColorWidget "Reverberation" "Induration" "Distortion"
              Light -> createOneColorWidget "Light"
              Light2 -> createOneColorWidget "Light"
              Radiance -> createOneColorWidget "Radiance"
              Darkness -> createOneColorWidget "Dark"
              Darkness2 -> createOneColorWidget "Dark"
              Umbra -> createOneColorWidget "Umbra"
            where
              createOneColorWidget :: String -> Widget Name
              createOneColorWidget a = C.withAttr (A.attrName a) $ C.str $ show sc

              createAlternateColorWidget :: String -> String -> String -> T.Widget Name
              createAlternateColorWidget _ _ [] = emptyWidget
              createAlternateColorWidget a b (x:xs) = (C.withAttr (A.attrName a) $ C.str $ [x]) <+> (createAlternateColorWidget b a xs)
                
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
    Just name -> do
      case name of
                    Wt1Field -> myHandleEvent wOne
                    Wt2Field -> myHandleEvent wTwo
                    Wt3Field -> myHandleEvent wThree
                    Wt4Field -> myHandleEvent wFour
                    Wt5Field -> myHandleEvent wFive
                    Wt6Field -> myHandleEvent wSix
                    SkillchainComboList -> myHandleEvent displayCombo

    Nothing -> Main.continue s
  where
    myHandleEvent :: Lens' UIState (List a) -> T.EventM Name (T.Next UIState)
    myHandleEvent lens = do newList <- L.handleListEvent ev $ s^.lens
                            Main.continue $ s & lens.~newList

handleEvent s ev = do
  Main.continue s

getWeaponTypes :: [List ChooseableWeaponTypes] -> [D.WeaponType]
getWeaponTypes list = [w | Just (_, ChosenWeapon w) <- L.listSelectedElement <$> list]

theMap :: A.AttrMap
theMap= A.attrMap defAttr [(A.attrName "Background", bg $ rgbColor 0 0 0)
                          ,(A.attrName "Weaponskill", fg $ rgbColor 148 148 248)
                          ,(A.attrName "Liquefaction", fg $ rgbColor 255 0 0)
                          ,(A.attrName "Impaction", fg $ rgbColor 147 112 219)
                          ,(A.attrName "Detonation", fg $ rgbColor 50 205 50)
                          ,(A.attrName "Scission", fg $ rgbColor 184 134 11)
                          ,(A.attrName "Reverberation", fg $ rgbColor 0 206 209)
                          ,(A.attrName "Induration", fg $ rgbColor 0 255 255)
                          ,(A.attrName "Compression", fg $ rgbColor 169 169 169)
                          ,(A.attrName "Transfixion", fg $ rgbColor 200 200 200)
                          ,(A.attrName "Light", fg $ rgbColor 230 230 230)
                          ,(A.attrName "Radiance", fg $ rgbColor 255 255 255)
                          ,(A.attrName "Dark", fg $ rgbColor 128 128 128)
                          ,(A.attrName "Umbra", fg $ rgbColor 105 105 105)
                          ,(L.listSelectedAttr, bg $ rgbColor 1 1 1)
                          ,(L.listSelectedFocusedAttr, bg $ rgbColor 0 0 128)]

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


