module View exposing (view)

import Collage exposing (..)
import Color exposing (..)
import Element exposing (..)
import Html exposing (Html)
import Html.Attributes exposing (rel, type_, href)
import Text exposing (..)
import Transform

import Model exposing (..)

view : Model -> Html Msg
view model =
    makeCanvas model
    <| case model.state of
            Menu -> menuModel
            Play -> gameModel model

makeCanvas : Model -> List Form -> Html Msg
makeCanvas model items =
    let
        canvas =
            toHtml
            <| container model.size.width model.size.height middle
            <| collage canvasSize.width canvasSize.height items
    in
        Html.div []
            [ Html.node "link" [rel "stylesheet", type_ "text/css", href "../assets/fonts.css"] []
            , canvas
            ]


backgroundImg : Form
backgroundImg =
    tiledImage canvasSize.width 109 "../assets/sky.png"
    |> toForm
    |> scale 2
    |> move (0, 112 + 109 - (toFloat canvasSize.height)/2)

groundImg : Form
groundImg =
    tiledImage (canvasSize.width*2) 112 "../assets/land.png"
    |> toForm
    |> move ((toFloat canvasSize.width) / 2, 112/2-(toFloat canvasSize.height)/2)

pipeImg : Form
pipeImg =
    tiledImage 52 canvasSize.height "../assets/pipe.png"
    |> toForm

pipeUpImg : Form
pipeUpImg =
    tiledImage 52 26 "../assets/pipe-up.png"
    |> toForm

pipeDownImg : Form
pipeDownImg =
    tiledImage 52 26 "../assets/pipe-down.png"
    |> toForm

birdImg : Form
birdImg =
    image (round birdSize) (round birdSize) "../assets/bird.png"
    |> toForm

drawPipe : Pipe -> Form
drawPipe pipe =
    group
        [ pipeImg
          |> move (pipe.x, pipe.y + gapHeight/2 + (toFloat canvasSize.height)/2)
        , pipeImg
          |> move (pipe.x, pipe.y - gapHeight/2 - (toFloat canvasSize.height)/2)
        , pipeDownImg
          |> move (pipe.x, pipe.y + gapHeight/2 + 13)
        , pipeUpImg
          |> move (pipe.x, pipe.y - gapHeight/2 - 13)
        ]

drawBird : Model -> Form
drawBird model =
    birdImg
    |> move (model.x, model.y)

drawGround : Model -> Form
drawGround model =
    move (-1.0 * toFloat ((ceiling model.x) % 336), 0) groundImg

getScore : Model -> Int
getScore model =
    if floor model.x >= 600 then
        1 + floor((model.x - 600) / pipeSpacing)
    else
        0

scoreTextStyle : Style
scoreTextStyle =
    { typeface = ["impact"]
    , height = Just 60
    , color = rgb 250 250 250
    , bold = False
    , italic = False
    , line = Nothing
    }

gameModel : Model -> List Form
gameModel model =
    let
        bird = drawBird model
        pipes = List.map drawPipe model.pipes
        worldToScreen = Transform.translation -model.x 0
    in
        [ rect (toFloat canvasSize.width) (toFloat canvasSize.height)
          |> filled bgColor
        , backgroundImg
        , groupTransform worldToScreen pipes
        , groupTransform worldToScreen [bird]
        , drawGround model
        , getScore model
          |> toString
          |> fromString
          |> style { scoreTextStyle | color = darkGray }
          |> text
          |> scale 1.1
          |> move (0, 350)
        , getScore model
          |> toString
          |> fromString
          |> style scoreTextStyle
          |> text
          |> move (0, 350)
        ]

flappyTextStyle : Style
flappyTextStyle =
    { typeface = ["FlappyBirdText"]
    , height = Just 125
    , color = darkBlue
    , bold = True
    , italic = False
    , line = Nothing
    }

menuModel : List Form
menuModel =
    [ rect (toFloat canvasSize.width) (toFloat canvasSize.height)
      |> filled bgColor
    , backgroundImg
    , groundImg
    , fromString "LAMBDA BIRD"
      |> style flappyTextStyle
      |> text
      |> move (0, 250)
    , birdImg
      |> scale 1
      |> move (0, 50)
    , fromString "CLICK to START"
      |> style
          { flappyTextStyle
          | height = Just 55
          , color = black
          }
      |> text
      |> move (0, -50)
    ]

