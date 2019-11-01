import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Url
import Http exposing (..)
import Json.Decode exposing (..)

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

firstWeapon : Dropdown.Options Msg
firstWeapon =
  { items = weaponsToChoose
  , emptyItem = Just { value = "N/A", text = "Pick a weapon", enabled = True }
  , onChange = (\mItem -> case mItem of
                            Just item -> Weapon1Changed <| Just item
                            Nothing -> Weapon1Changed Nothing
    )
  }

secondWeapon : Options Msg
secondWeapon =
  { items = weaponsToChoose
  , emptyItem = Just { value = "N/A", text = "Pick a weapon", enabled = True }
  , onChange = (\mItem -> case mItem of
      Just item -> Weapon2Changed <| Just item
      Nothing -> Weapon2Changed Nothing
    )
  }

-- MODEL

type alias Model =
  { key : Nav.Key
  , url : Url.Url
  , weaponskills : List Ws
  , weapon1 : Maybe String
  , weapon2 : Maybe String
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
init flags url key = ( Model key url [] Nothing Nothing, Cmd.none )

-- UPDATE

type Msg
  = LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url
  | GotText (Result Http.Error (List Ws))
  | Weapon1Changed (Maybe String)
  | Weapon2Changed (Maybe String)
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

    Weapon1Changed weapon -> 
      update GetItems { model | weapon1 = weapon } 
    
    Weapon2Changed weapon ->
      update GetItems { model | weapon2 = weapon }

    GetItems ->
      ( model, case (model.weapon1, model.weapon2) of
          (Just w1, Just w2) -> Http.get
            { url = "http://localhost:3000/sc/" ++ w1 ++ "/" ++ w2 
            , expect = Http.expectJson GotText (Json.Decode.list wsDecoder)
            }
          _ -> Cmd.none
      )
    GotText 
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

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none

-- VIEW

view : Model -> Browser.Document Msg
view model =
  { title = "Skillchain Finder"
  , body =
    [ text "The current URL is: "
    , b [] [ text (Url.toString model.url) ]
    , ul []
      [ viewLink "/home"
      , viewLink "/profile"
      , viewLink "/reviews/the-century-of-the-shelf"
      , viewLink "/reviews/public-opinion"
      , viewLink "/reviews/shah-of-shahs"
      ]
    , Dropdown.dropdown firstWeapon [] Nothing
    , Dropdown.dropdown secondWeapon [] Nothing
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
