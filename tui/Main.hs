{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Main where

import Brick.Main
import Brick.Forms
import Brick.Widgets.Core
import Brick.Types
import Brick.AttrMap
import Brick.Widgets.Border
import Brick.Util

import Graphics.Vty.Attributes
import Graphics.Vty.Input.Events

import Control.Lens.Lens
import Control.Lens.TH
import Control.Lens.Setter
import Control.Lens.Getter

import Control.Monad
import Data.Text
import Data.Vector

import SkillchainData

data Name = Ws1Field | Ws2Field | Ws3Field | Ws4Field | Ws5Field | Ws6Field deriving (Eq, Ord, Show)

data FormState = FormState
  { 
    _wsOne :: Maybe WeaponType
  , _wsTwo :: Maybe WeaponType
  , _wsThree :: Maybe WeaponType
  , _wsFour :: Maybe WeaponType
  , _wsFive :: Maybe WeaponType
  , _wsSix :: Maybe WeaponType
  } deriving (Show)

type MyForm = Form FormState () Name

data UIState = UIState
  {
    _displayCombo :: [SkillchainCombination]
  , _myForm :: MyForm
  }

makeLenses ''UIState
makeLenses ''FormState

drawUI :: UIState -> [Widget Name]
drawUI = return . vLimit 10 . renderForm . setFormConcat (hBox . fmap border) . _myForm

mkForm :: FormState -> MyForm
mkForm = newForm 
  [makeLabel "Weapon Type 1" @@= (createListField wsOne Ws1Field)
  ,makeLabel "Weapon Type 2" @@= (createListField wsTwo Ws2Field)
  ,makeLabel "Weapon Type 3" @@= (createListField wsThree Ws3Field)
  ,makeLabel "Weapon Type 4" @@= (createListField wsFour Ws4Field)
  ,makeLabel "Weapon Type 5" @@= (createListField wsFive Ws5Field)
  ,makeLabel "Weapon Type 6" @@= (createListField wsSix Ws6Field)
  ]

makeLabel :: String -> Widget Name -> Widget Name
makeLabel s = (<=>) $ padBottom (Pad 1) . padTop (Pad 1) $ (str s)

createListField :: (Ord n, Show n) => Lens' s (Maybe WeaponType) -> n -> s-> FormFieldState s () n
createListField lens name s = listField 
  (const $ fromList allWeaponTypes) 
  lens
  (\b s -> if b then withAttr focusedFormInputAttr $ str (show s) else str (show s)) 
  1
  name
  s

handleEvent :: UIState -> BrickEvent Name () -> EventM Name (Next UIState)
handleEvent s (VtyEvent (EvKey (KChar 'q') []))  = halt s
handleEvent s (VtyEvent (EvKey KEnter [])) = halt s
handleEvent s ev = do
  newForm <- handleFormEvent ev $ s^.myForm
  continue $ s & myForm.~newForm--UIState { _displayCombo = [], _myForm = newForm }

theMap :: AttrMap
theMap= attrMap defAttr [(focusedFormInputAttr, red `on` blue)]

initFormState :: FormState
initFormState = FormState
  {
    _wsOne = Nothing
  , _wsTwo = Nothing
  , _wsThree = Nothing
  , _wsFour = Nothing
  , _wsFive = Nothing
  , _wsSix = Nothing
  }

initUIState :: UIState
initUIState = UIState
  {
    _myForm = mkForm initFormState
  , _displayCombo = []
  }

app :: App UIState () Name
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const theMap
          }

main :: IO ()
main = do
  let state = initUIState
  void $ defaultMain app state


