import Html exposing (Html, program)

import Task
import Window
import AnimationFrame
import Time exposing (..)
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

pipeGreen : Color
pipeGreen = rgb 20 200 20

menuBackground : Form
menuBackground =
    rect (toFloat canvasSize.width) (toFloat canvasSize.height)
        |> filled (rgb 230 230 230)

-- MODEL

type alias Bird =
    { x : Float
    , y : Float
    , dy: Float
    }

type alias Pipe = 
    { x: Float
    , height: Float
    , up: Bool
    }
    
type alias PlayState =
    { bird : Bird
    , pipes : List Pipe
    }

type GameState
    = Menu
    | Play PlayState

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
        
initGame : PlayState
initGame = 
    { bird =
        { x = 0
        , y = 0
        , dy = 0
        }
    , pipes =
        [ { x = 100
          , height = 200
          , up = True
          }
        , { x = -200
          , height = 150
          , up = False
          }
        ]
    }

-- MESSAGES

type Msg
    = NoOp
    | Resize Window.Size
    | MouseClick Mouse.Position
    | GameUpdate Time
    | StartGame

-- VIEW
view : Model -> Html Msg
view model = 
    makeCanvas model <|
        case model.state of
            Menu -> menuModel
            Play state -> gameModel state
            
makeCanvas : Model -> List Form -> Html Msg
makeCanvas model items =
    toHtml <|
    container model.size.width model.size.height middle <|
    collage canvasSize.width canvasSize.height items

drawPipe : Pipe -> Form
drawPipe pipe = 
    rect pipeWidth pipe.height
        |> filled pipeGreen
        |> move (pipe.x, -pipe.height/2)
        |>  if pipe.up then
                move (0, (toFloat canvasSize.height) / 2)
            else
                move (0, pipe.height - (toFloat canvasSize.height) / 2)
    
drawBird : Bird -> Form
drawBird bird = 
    rect birdSize birdSize
    |> filled red
    |> move (bird.x, bird.y)

gameModel : PlayState -> List Form
gameModel state = 
    let
        bird = drawBird state.bird
        pipes = List.map drawPipe state.pipes
        worldToScreen = Transform.translation -state.bird.x 0
    in
        [groupTransform worldToScreen (bird :: pipes)]

menuModel : List Form
menuModel =
    [ menuBackground
    , move (0, 250) <|
      text (
        fromString "Lambda Bird"
        |> monospace
        |> Text.height 35
        |> Text.color darkRed
        |> bold
      )
    , move (0, 0) <|
      text (
        fromString "Click to start"
        |> monospace
        |> Text.height 25
        |> Text.color (rgb 20 20 20)
      )
    ]

-- UPDATE

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp -> (model, Cmd.none)
        Resize size -> ({ model | size = size}, Cmd.none)
        StartGame -> ({ model | state = Play initGame}, Cmd.none)
        MouseClick pos ->
            case model.state of
                Menu -> update StartGame model
                Play playState ->
                    let
                        oldBird = playState.bird
                        newBird = { oldBird | dy = birdJump }
                        newPlayState = { playState | bird = newBird }
                    in
                        ({ model | state = Play newPlayState }, Cmd.none)
        GameUpdate delta ->
            case model.state of
                Menu -> (model, Cmd.none)
                Play playState ->
                    let
                        newBird = updateBird (inSeconds delta) playState.bird
                        newPlayState = { playState | bird = newBird }
                    in
                        ({ model | state = Play newPlayState }, Cmd.none)

updatePipes :

updateBird : Float -> Bird -> Bird
updateBird delta bird =
    { x = bird.x + delta * birdSpeed
    , y = bird.y + delta*bird.dy - 0.5*delta*delta*gravity
    , dy = bird.dy - delta*gravity
    }

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