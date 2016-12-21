import Html exposing (..)
import Html.Attributes exposing (..)
import Hover
import Focus



main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL


type alias Model =
  { hovered : Bool
  , focused : Bool
  }


init : (Model, Cmd Msg)
init =
  (Model False False, Cmd.none)



-- UPDATE


type Msg
  = Hover Bool
  | Focus Bool


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Hover hovered ->
      ({ model | hovered = hovered }, Cmd.none)

    Focus focused ->
      ({ model | focused = focused }, Cmd.none)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Hover.hover "hoverable" Hover
    , Focus.focus "focusable" Focus
    ]




-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ div
        [ style (("background-color", if model.hovered then "gold" else "silver") :: boxStyle)
        , id "hoverable"
        ] [ text "hover" ]
    , div
        [ style (("background-color", if model.focused then "gold" else "silver") :: boxStyle)
        , id ""
        ] [ text "focus" ]
    , input [ id "focusable" ] []
    ]


boxStyle : List (String, String)
boxStyle =
  [ ("width", "100px")
  , ("height", "100px")
  , ("margin", "5px")
  ]
