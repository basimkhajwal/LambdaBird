module Update exposing (update)

import Random
import Task
import Time exposing (..)

import Model exposing (..)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp -> (model, Cmd.none)
        Resize size -> ({ model | size = size}, Cmd.none)
        StartGame -> ({ model | state = Play}, Cmd.none)
        EndGame -> ({startModel | size = model.size}, Cmd.none)
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
                Play ->
                    if model.falling then
                        (model, Cmd.none)
                    else
                        ({ model | dy = birdJump}, Cmd.none)

        GameUpdate delta ->
            case model.state of
                Menu -> (model, Cmd.none)
                Play ->
                    model
                    |> updateFalling
                    |> updateBird (inSeconds delta)
                    |> filterPipes
                    |> updatePipes
                    |> updateDeath

updateFalling : Model -> Model
updateFalling model =
    let
        positionDist : Float
        positionDist = birdSize/2 + pipeWidth/2

        pipeCollision =
            model.pipes
            |> List.filter (\p -> abs (p.x - model.x) <= positionDist)
            |> List.any (\p -> abs (p.y - model.y) + birdSize/2 >= gapHeight/2)
    in
        if pipeCollision then
            { model | falling = True }
        else
            model

updateDeath : (Model, Cmd Msg) -> (Model, Cmd Msg)
updateDeath (model, msg) =
    if model.y <= -(toFloat canvasSize.height)/2 then
        ( model
        , Task.perform identity (Task.succeed EndGame)
        )
    else
        (model, msg)

updateBird : Float -> Model -> Model
updateBird delta model =
    let
        xSpeed =
            if model.falling then
                0
            else
                birdSpeed
    in
        { model
        | x = model.x + delta * xSpeed
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

