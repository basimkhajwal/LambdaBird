
import Html exposing (Html, program)
import Time exposing (..)
import Task
import Random

import Window
import AnimationFrame
import Mouse

import Color exposing (..)
import Collage exposing (..)
import Element exposing (..)
import Transform
import Text exposing (..)

-- CONSTANTS

canvasSize : Window.Size
canvasSize =
    { width = 1000
    , height = 600
    }
    
birdSpeed : Float
birdSpeed = 100

birdJump : Float
birdJump = 250

gravity : Float
gravity = 300

birdSize : Float
birdSize = 30

pipeWidth : Float
pipeWidth = 50

pipeSpacing : Float
pipeSpacing = 200

gapHeight : Float
gapHeight = 120

pipeGreen : Color
pipeGreen = rgb 20 200 20

bgColor : Color
bgColor = rgb 230 230 230

-- MODEL

type alias Pipe =
    { x: Float
    , y: Float
    }

type GameState
    = Menu
    | Play

type alias Model =
    { size : Window.Size
    , state : GameState
    , x : Float
    , y : Float
    , dy : Float
    , lastPipeX : Float
    , pipes : List Pipe
    }

init : ( Model, Cmd Msg )
init =
    let resizeTask : Result x Window.Size -> Msg
        resizeTask res =
            case res of
                Ok size -> Resize size
                Err _ -> NoOp
    in (startModel, Task.attempt resizeTask Window.size)

startModel : Model
startModel =
    { size = Window.Size 0 0
    , state = Menu
    , x = 0
    , y = 0
    , dy = 0
    , lastPipeX = 600
    , pipes =
        [ { x = 600
          , y = 200
          }
        ]
    }

-- MESSAGES

type Msg
    = NoOp
    | Resize Window.Size
    | MouseClick Mouse.Position
    | GameUpdate Time
    | GeneratePipe Float
    | StartGame

-- VIEW
view : Model -> Html Msg
view model =
    makeCanvas model
    <| case model.state of
            Menu -> menuModel
            Play -> gameModel model

makeCanvas : Model -> List Form -> Html Msg
makeCanvas model items =
    toHtml
    <| container model.size.width model.size.height middle
    <| collage canvasSize.width canvasSize.height items

drawPipe : Pipe -> Form
drawPipe pipe =
    group
    [ rect pipeWidth (toFloat canvasSize.height)
      |> filled pipeGreen
      |> move (pipe.x, 0)
    , rect (pipeWidth+2) gapHeight
      |> filled bgColor
      |> move (pipe.x, pipe.y)
    ]

drawBird : Model -> Form
drawBird model =
    rect birdSize birdSize
    |> filled red
    |> move (model.x, model.y)

gameModel : Model -> List Form
gameModel model =
    let
        bird = drawBird model
        pipes = List.map drawPipe model.pipes
        worldToScreen = Transform.translation -model.x 0
    in
        [ rect (toFloat canvasSize.width) (toFloat canvasSize.height)
          |> filled bgColor
        , groupTransform worldToScreen pipes
        , groupTransform worldToScreen [bird]
        ]

menuModel : List Form
menuModel =
    [ rect (toFloat canvasSize.width) (toFloat canvasSize.height)
      |> filled bgColor
    , fromString "Lambda Bird"
      |> monospace
      |> Text.height 35
      |> Text.color darkRed
      |> bold
      |> text
      |> move (0, 250)
    , fromString "Click to start"
      |> monospace
      |> Text.height 25
      |> Text.color (rgb 20 20 20)
      |> text
    ]

-- UPDATE

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp -> (model, Cmd.none)
        Resize size -> ({ model | size = size}, Cmd.none)
        StartGame -> ({ model | state = Play}, Cmd.none)

        GeneratePipe y ->
            let
                newPipe =
                    { x = model.lastPipeX + pipeSpacing
                    , y = y
                    }
            in
                ( { model
                  | pipes = newPipe :: model.pipes
                  , lastPipeX = newPipe.x
                  }
                , Cmd.none
                )

        MouseClick pos ->
            case model.state of
                Menu -> update StartGame model
                Play -> ({ model | dy = birdJump}, Cmd.none)

        GameUpdate delta ->
            case model.state of
                Menu -> (model, Cmd.none)
                Play ->
                    model
                    |> updateBird (inSeconds delta)
                    |> filterPipes
                    |> updatePipes

updateBird : Float -> Model -> Model
updateBird delta model =
    { model
    | x = model.x + delta * birdSpeed
    , y = model.y + delta*model.dy - 0.5*delta*delta*gravity
    , dy = model.dy - delta*gravity
    }

filterPipes : Model -> Model
filterPipes model =
    { model
    | pipes =
        List.filter (\p -> p.x - model.x > -(toFloat canvasSize.width)/2 - 50) model.pipes
    }

updatePipes : Model -> (Model, Cmd Msg)
updatePipes model =
    let
        updateRequired = model.lastPipeX < model.x + (toFloat canvasSize.width) / 2.0 - 50

        pipeBound : Float
        pipeBound = gapHeight/2 + 10 - (toFloat canvasSize.height)/2

        generatePipe : Cmd Msg
        generatePipe = Random.generate GeneratePipe (Random.float -pipeBound pipeBound)
    in
        if updateRequired then
            (model, generatePipe)
        else
            (model, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model
    = Sub.batch
        [ Window.resizes Resize
        , Mouse.clicks MouseClick
        , AnimationFrame.diffs GameUpdate
        ]

-- MAIN
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }