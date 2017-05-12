module Main exposing (main)

import AnimationFrame
import Html exposing (Html, program)
import Mouse
import Window

import Model exposing (..)
import Update exposing (update)
import View exposing (view)

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Window.resizes Resize
        , Mouse.clicks MouseClick
        , AnimationFrame.diffs GameUpdate
        ]

main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }