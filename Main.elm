module Main exposing (..)

import Html exposing (Html)
import Html.Attributes as HtmlA
import Html.Events exposing (onClick)
import Html.App as App
import Time exposing (Time, millisecond)
import World exposing (tick)
import View exposing (Renderer)
import Types exposing (ViewPort, Ant, Grid)


type alias Model =
    { grid : Grid
    , ant : Ant
    , viewPort : ViewPort
    , speed : Float
    , renderer : Renderer
    }


type Msg
    = Tick Time
    | Faster
    | Slower
    | Grow
    | SwitchRenderer


newModel : Model
newModel =
    let
        ( grid, ant ) =
            World.init
    in
        { grid = grid
        , ant = ant
        , viewPort = { min = -10, max = 10, size = 500 }
        , speed = 256
        , renderer = View.defaultRenderer
        }


view : Model -> Html Msg
view ({ grid, ant, viewPort, speed, renderer } as model) =
    Html.div []
        [ Html.button [ onClick Faster ] [ Html.text "faster" ]
        , Html.button [ onClick Slower ] [ Html.text "slower" ]
        , Html.button [ onClick Grow ] [ Html.text "grow" ]
        , Html.button [ onClick SwitchRenderer ] [ Html.text "switch renderer" ]
        , View.view renderer grid ant viewPort
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ grid, ant, speed, viewPort } as model) =
    case msg of
        Faster ->
            ( { model | speed = speed / 2 }, Cmd.none )

        Slower ->
            ( { model | speed = speed * 2 }, Cmd.none )

        Tick t ->
            let
                ( grid, ant ) =
                    tick ( grid, ant )
            in
                ( { model | grid = grid, ant = ant }, Cmd.none )

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


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every model.speed Tick


main =
    App.program
        { init = ( newModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
