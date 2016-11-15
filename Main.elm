module Main exposing (..)

import Html exposing (Html)
import Html.Attributes as HtmlA
import Html.Events exposing (onClick)
import Time exposing (Time, millisecond)
import World exposing (World, tick)
import View exposing (Renderer)
import Types exposing (ViewPort, Ant, Grid)
import Random


type alias Model =
    { world : World
    , viewPort : ViewPort
    , speed : Float
    , renderer : Renderer
    , turbo : Bool
    , running : Bool
    }


type Msg
    = Tick Time
    | Faster
    | Slower
    | Grow
    | SwitchRenderer
    | Reset
    | ResetWithRandom
    | ToggleTurbo
    | NewWorld World
    | TogglePause


newModel : Model
newModel =
    { world = World.init
    , viewPort = { min = -10, max = 10, size = 500 }
    , speed = 256
    , renderer = View.defaultRenderer
    , turbo = False
    , running = False
    }


view : Model -> Html Msg
view ({ world, viewPort, speed, renderer } as model) =
    let
        startOrStop =
            case model.running of
                True ->
                    "stop"

                False ->
                    "start"
    in
        Html.div []
            [ Html.button [ onClick TogglePause ] [ Html.text startOrStop ]
            , Html.button [ onClick Faster ] [ Html.text "faster" ]
            , Html.button [ onClick Slower ] [ Html.text "slower" ]
            , Html.button [ onClick Grow ] [ Html.text "grow" ]
            , Html.button [ onClick SwitchRenderer ] [ Html.text (toString renderer) ]
            , Html.button [ onClick Reset ] [ Html.text "reset" ]
            , Html.button [ onClick ResetWithRandom ] [ Html.text "random new game" ]
            , Html.button [ onClick ToggleTurbo ] [ Html.text ("turbo: " ++ (toString model.turbo)) ]
            , Html.div [] [ View.view renderer world.grid world.ant viewPort ]
            ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ world, speed, viewPort } as model) =
    case msg of
        Faster ->
            ( { model | speed = speed / 2 }, Cmd.none )

        Slower ->
            ( { model | speed = speed * 2 }, Cmd.none )

        Tick t ->
            let
                tickTimes times =
                    List.foldl (\_ old -> tick old) world (List.range 1 times)

                newWorld =
                    case model.turbo of
                        False ->
                            tickTimes 1

                        True ->
                            tickTimes 10
            in
                ( { model | world = newWorld }, Cmd.none )

        Grow ->
            ( { model
                | viewPort =
                    { viewPort
                        | min = viewPort.min - 5
                        , max = viewPort.max + 5
                    }
              }
            , Cmd.none
            )

        SwitchRenderer ->
            ( { model | renderer = View.switchRenderer model.renderer }, Cmd.none )

        Reset ->
            ( { model | world = World.init }, Cmd.none )

        NewWorld world ->
            ( { model | world = world }, Cmd.none )

        ToggleTurbo ->
            ( { model | turbo = not model.turbo }, Cmd.none )

        ResetWithRandom ->
            ( model, Random.generate NewWorld World.generator )

        TogglePause ->
            ( { model | running = not model.running }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    if not model.running then
        Sub.none
    else
        Time.every model.speed Tick


main =
    Html.program
        { init = ( newModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
