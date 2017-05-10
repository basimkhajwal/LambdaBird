import Html exposing (Html, program)
import Task

import Window
import AnimationFrame

import Color exposing (..)
import Collage exposing (..)
import Element exposing (..)
import Text exposing (..)

-- CONSTANTS

canvasSize : Window.Size
canvasSize =
    { width = 1000
    , height = 600
    }

menuBackground : Form
menuBackground =
    rect (toFloat canvasSize.width) (toFloat canvasSize.height)
        |> filled (rgb 230 230 230)

-- MODEL
type GameState
    = Menu
    | Play

type alias Model =
    { size : Window.Size
    , state : GameState
    }

init : ( Model, Cmd Msg )
init =
    let
        resizeTask : Result x Window.Size -> Msg
        resizeTask res =
            case res of
                Ok size -> Resize size
                Err _ -> NoOp

        startModel : Model
        startModel =
            { size = Window.Size 0 0
            , state = Menu
            }
    in
        ( startModel
        , Task.attempt resizeTask Window.size
        )

-- MESSAGES

type Msg
    = NoOp
    | Resize Window.Size
    | StartGame

-- VIEW
view : Model -> Html Msg
view model = menuModel model

menuModel : Model -> Html Msg
menuModel model =
    toHtml <|
    container model.size.width model.size.height middle <|
    collage canvasSize.width canvasSize.height
        [ menuBackground
        , move (0, 250) <|
          text (
            fromString "Lambda Bird"
            |> monospace
            |> Text.height 35
            |> Text.color darkRed
            |> bold
          )
        ]

-- UPDATE

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp -> (model, Cmd.none)
        Resize size -> ({ model | size = size}, Cmd.none)
        StartGame -> ({ model | state = Play}, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model
    = Sub.batch
        [ Window.resizes Resize
        ]

-- MAIN
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }