import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Url
import Http exposing (..)
import Json.Decode exposing (..)
import Dict exposing (Dict, empty, insert, remove, get, size, values)
import List exposing (intersperse)
import String exposing (concat)

import Dropdown exposing (..)

main : Program () Model Msg
main =
  Browser.application
  { init = init
  , view = view
  , update = update
  , subscriptions = subscriptions
  , onUrlChange = UrlChanged
  , onUrlRequest = LinkClicked
  }

weaponsToChoose : List Dropdown.Item
weaponsToChoose = 
  [
    { value = "Club", text = "Club", enabled = True }
  , { value = "Sword", text = "Sword", enabled = True }
  ]

chosenWeapons : Int -> Options Msg
chosenWeapons index =
  { items = weaponsToChoose
  , emptyItem = Just { value = "N/A", text = "Pick a weapon", enabled = True }
  , onChange = (\mItem -> case mItem of
      Just item -> ChosenWeapon index <| Just item
      Nothing -> ChosenWeapon index Nothing
    )
  }

-- MODEL

type alias Model =
  { key : Nav.Key
  , url : Url.Url
  , weaponskills : List Ws
  , chosenWeapons : Dict Int String
  }

type alias Ws =
  { ws1 : String
  , ws2 : String
  , sc : String
  }

wsDecoder : Decoder Ws
wsDecoder =
  map3 Ws
    (field "ws1" string)
    (field "ws2" string)
    (field "sc" string)

init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key = ( Model key url [] empty, Cmd.none )

-- UPDATE

type Msg
  = LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url
  | GotWsInformation (Result Http.Error (List Ws))
  | ChosenWeapon Int (Maybe String)
  | GetItems

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    LinkClicked urlRequest ->
      case urlRequest of
        Browser.Internal url ->
          ( model, Nav.pushUrl model.key (Url.toString url) )

        Browser.External href ->
          ( model, Nav.load href )

    UrlChanged url ->
      ( { model | url = url }
      , Cmd.none
      )

    ChosenWeapon index weapon -> 
      case weapon of
        Just wep -> update GetItems { model | chosenWeapons = insert index wep model.chosenWeapons } 
        Nothing -> update GetItems { model | chosenWeapons = remove index model.chosenWeapons }
    
    --GetItems ->
      --( model, case (model.weapon1, model.weapon2) of
          --(Just w1, Just w2) -> Http.get
            --{ url = "http://localhost:3000/sc/" ++ w1 ++ "/" ++ w2 
            --, expect = Http.expectJson GotWsInformation (Json.Decode.list wsDecoder)
            --}
          --_ -> Cmd.none
      --)
    GetItems ->
      (model, 
      if (size model.chosenWeapons > 1) then
          Http.get
            { url = "http://localhost:3000/sc/" ++ urlHelper model.chosenWeapons
            , expect = Http.expectJson GotWsInformation (Json.Decode.list wsDecoder) 
            }
      else
        Cmd.none)

    GotWsInformation
        result ->
      case result of
        Ok fullText ->
          ( { model | weaponskills = fullText }, Cmd.none)
        Err error ->
          case error of
            BadUrl urlStuff ->
              ( { model | weaponskills = []}, Cmd.none)
            Timeout ->
              ( { model | weaponskills = []}, Cmd.none)
            NetworkError ->
              ( { model | weaponskills = []}, Cmd.none)
            BadStatus x ->
              ( { model | weaponskills = []}, Cmd.none)
            BadBody body ->
              ( { model | weaponskills = []}, Cmd.none)


urlHelper : Dict Int String -> String
urlHelper dict =
  concat <| intersperse "," <| values dict

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none

-- VIEW

view : Model -> Browser.Document Msg
view model =
  { title = "Skillchain Finder"
  , body =
    [ Dropdown.dropdown (chosenWeapons 1) [] Nothing
    , Dropdown.dropdown (chosenWeapons 2) [] Nothing
    , Dropdown.dropdown (chosenWeapons 3) [] Nothing
    , 
        table [] <| List.map (\combo -> 
          tr []
            [ td [] [text combo.ws1]
            , td [] [text combo.ws2]
            , td [] [text combo.sc ]]) model.weaponskills
    ]
  }

viewLink : String -> Html msg
viewLink path =
  li [] [ a [ href path ] [text path ] ]
