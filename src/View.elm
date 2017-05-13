module View exposing (view)

import Collage exposing (..)
import Color exposing (..)
import Element exposing (..)
import Html exposing (Html)
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

drawGround : Model -> Form
drawGround model =
    move (-1.0 * toFloat ((ceiling model.x) % 336), 0) groundImg

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
        , drawGround model
        , groupTransform worldToScreen pipes
        , groupTransform worldToScreen [bird]
        ]

menuModel : List Form
menuModel =
    [ rect (toFloat canvasSize.width) (toFloat canvasSize.height)
      |> filled bgColor
    , backgroundImg
    , groundImg
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

