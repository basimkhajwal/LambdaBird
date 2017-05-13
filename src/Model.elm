module Model exposing (..)

import Color exposing (..)
import Mouse
import Task
import Time exposing (Time)
import Window

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
    , falling : Bool
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
    , falling = False
    , lastPipeX = 600
    , pipes =
        [ { x = 600
          , y = 0
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
    | EndGame

-- CONSTANTS

canvasSize : Window.Size
canvasSize =
    { width = 700
    , height = 900
    }

birdSpeed : Float
birdSpeed = 120

birdJump : Float
birdJump = 300

gravity : Float
gravity = 500

birdSize : Float
birdSize = 30

pipeWidth : Float
pipeWidth = 52

pipeSpacing : Float
pipeSpacing = 250

gapHeight : Float
gapHeight = 220

pipeGreen : Color
pipeGreen = rgb 20 200 20

bgColor : Color
bgColor = rgb 78 192 202
