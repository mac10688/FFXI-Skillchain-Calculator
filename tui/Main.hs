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
import Brick.Widgets.List

import Graphics.Vty.Attributes
import Graphics.Vty.Input.Events

import Control.Lens.Lens
import Control.Lens.TH
import Control.Lens.Setter
import Control.Lens.Getter

import Control.Monad
import Data.Text
import Data.Vector

import SkillchainData as D
import Skillchain

data Name = Ws1Field | Ws2Field | Ws3Field | Ws4Field | Ws5Field | Ws6Field | SkillchainComboList deriving (Eq, Ord, Show)

data ChooseableWeaponTypes = NoWeapon | ChosenWeapon D.WeaponType deriving (Eq, Show, Ord, Read)

allChooseableWeaponTypes :: [ChooseableWeaponTypes]
allChooseableWeaponTypes = NoWeapon : (ChosenWeapon <$> [(minBound :: D.WeaponType)..])

data FormState = FormState
  { 
    _wsOne :: Maybe ChooseableWeaponTypes
  , _wsTwo :: Maybe ChooseableWeaponTypes
  , _wsThree :: Maybe ChooseableWeaponTypes
  , _wsFour :: Maybe ChooseableWeaponTypes
  , _wsFive :: Maybe ChooseableWeaponTypes
  , _wsSix :: Maybe ChooseableWeaponTypes
  } deriving (Show)

type MyForm = Form FormState () Name

data UIState = UIState
  {
    _displayCombo :: GenericList Name Vector D.SkillchainCombination
  , _myForm :: MyForm
  }

makeLenses ''UIState
makeLenses ''FormState

drawUI :: UIState -> [Widget Name]
drawUI state = return $ vBox $ [createFormSection, createDisplay] ?? state

createFormSection :: UIState -> Widget Name
createFormSection = vLimit 10 . renderForm . setFormConcat (hBox . fmap border) . _myForm

createDisplay :: UIState -> Widget Name
createDisplay state =
  let
    combinations = state^.displayCombo
  in
    renderList (\b sc -> if b then withAttr (attrName "Red") $ str $ show sc else str $ show sc) False combinations
    
createComboRow :: D.SkillchainCombination -> Widget Name
createComboRow (D.ScStart ws cont) = hBox $ showWs ws <+> str " -> ": (createComboRow' cont)

createComboRow' :: D.ScContinuation -> [Widget Name]
createComboRow' (D.ScContinuation ws sc (Just continue)) = showWs ws <+> str " -> " <+> showSc sc <+> str " -> ": createComboRow' continue
createComboRow' (D.ScContinuation ws sc Nothing) = [showWs ws <+> str " -> " <+> showSc sc]

showWs :: D.Weaponskill -> Widget Name
showWs ws = withAttr (attrName "Red") $ str $ (show ws)

showSc :: D.Skillchain -> Widget Name
showSc sc = withAttr (attrName "Blue") $ str $ (show sc)

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

createListField :: (Ord n, Show n) => Lens' s (Maybe ChooseableWeaponTypes) -> n -> s-> FormFieldState s () n
createListField lens name s = listField 
  (const $ fromList allChooseableWeaponTypes) 
  lens
  (\b s -> case s of
             NoWeapon -> if b then withAttr focusedFormInputAttr $ str "No Weapon" else str "No Weapon" 
             ChosenWeapon w -> if b then withAttr focusedFormInputAttr $ str (show w) else str (show w)) 
  1
  name
  s

getFormState :: UIState -> FormState
getFormState state = state ^. myForm . (to formState)

handleEvent :: UIState -> BrickEvent Name () -> EventM Name (Next UIState)
handleEvent s (VtyEvent (EvKey (KChar 'q') []))  = halt s
handleEvent s (VtyEvent (EvKey (KChar 'j') [])) = 
  let
    combo = s^.displayCombo
  in
  continue $ s & displayCombo.~ (listMoveDown combo)
handleEvent s (VtyEvent (EvKey KEnter [])) =
  let
    fs = getFormState s
    possibleWeaponTypesChosen = [fs^.wsOne, fs^.wsTwo, fs^.wsThree, fs^.wsFour, fs^.wsFive, fs^.wsSix]
    weaponTypes = [wt | Just (ChosenWeapon wt) <- possibleWeaponTypesChosen]
    skillchains = scCombinations weaponTypes
  in
  continue $ s & displayCombo.~ (list SkillchainComboList (fromList skillchains) 1)
handleEvent s ev = do
  newForm <- handleFormEvent ev $ s^.myForm
  continue $ s & myForm.~newForm

theMap :: AttrMap
theMap= attrMap defAttr [(focusedFormInputAttr, red `on` blue)
                        ,(attrName "Red", red `on` black)
                        ,(attrName "Blue", blue `on` black)]

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
  , _displayCombo = list SkillchainComboList (fromList []) 1
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


