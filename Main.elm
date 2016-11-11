module Main exposing (..)

import Html exposing (Html)
import Html.Attributes as HtmlA
import Html.Events exposing (onClick)
import Html.App as App
import Svg exposing (Svg, svg, rect)
import Svg.Attributes as SvgA
import Time exposing (Time, millisecond)
import World exposing (colorAtPosition, tick, Ant(..), Color(..), Grid, Direction(..), Position)


type alias ViewPort =
    { min : Int
    , max : Int
    , size : Int
    }


type alias Model =
    { grid : Grid
    , ant : Ant
    , viewPort : ViewPort
    , speed : Float
    }


type Msg
    = Tick Time
    | Faster
    | Slower
    | Grow


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
        }


scale : ViewPort -> Int
scale { min, max, size } =
    size // (max - min) |> abs


view : Model -> Html Msg
view ({ grid, ant, viewPort, speed } as model) =
    let
        scaleFactor =
            scale viewPort

        min =
            viewPort.min * (scale viewPort) |> toString

        size =
            viewPort.size * 2 |> toString

        viewBoxString =
            min ++ " " ++ min ++ " " ++ (viewPort.size * 2 |> toString) ++ " " ++ (viewPort.size * 2 |> toString)
    in
        Html.div []
            [ Html.button [ onClick Faster ] [ Html.text "faster" ]
            , Html.button [ onClick Slower ] [ Html.text "slower" ]
            , Html.button [ onClick Grow ] [ Html.text "grow" ]
            , svg
                [ SvgA.width size
                , SvgA.height size
                , SvgA.viewBox viewBoxString
                ]
                [ (viewGrid viewPort grid), (viewAnt viewPort ant) ]
            ]


viewAnt : ViewPort -> Ant -> Svg msg
viewAnt viewPort (Ant ( x, y ) direction) =
    let
        scaleFactor =
            scale viewPort

        half =
            scaleFactor // 2 |> toString

        full =
            scaleFactor |> toString

        pathString =
            "M" ++ half ++ " 0 L" ++ full ++ " " ++ half ++ " L0 " ++ half ++ " L" ++ half ++ " 0"

        arrow =
            Svg.path
                [ SvgA.d pathString
                , SvgA.style "fill: #76daff"
                ]
                []

        rotate =
            let
                rotation =
                    case direction of
                        North ->
                            "0"

                        East ->
                            "90"

                        South ->
                            "180"

                        West ->
                            "270"
            in
                "rotate(" ++ rotation ++ "," ++ half ++ "," ++ half ++ ")"

        translate =
            "translate(" ++ (x * scaleFactor |> toString) ++ "," ++ (y * scaleFactor |> toString) ++ ")"
    in
        Svg.g [ SvgA.transform (translate ++ "," ++ rotate) ] [ arrow ]


viewGrid : ViewPort -> Grid -> Svg msg
viewGrid viewPort grid =
    let
        positionsToDraw =
            let
                square row column =
                    ( column, row )

                rows row =
                    List.map (square row) [viewPort.min..viewPort.max]
            in
                List.concatMap rows [viewPort.min..viewPort.max]
    in
        Svg.g []
            (List.map (\pos -> ( pos, colorAtPosition grid pos )) positionsToDraw
                |> List.map (\( pos, color ) -> viewSquare viewPort pos color)
            )


viewSquare : ViewPort -> Position -> Color -> Svg msg
viewSquare viewPort ( x, y ) color =
    let
        colorString =
            case color of
                White ->
                    "white"

                Black ->
                    "black"

        scaleFactor =
            scale viewPort
    in
        rect
            [ SvgA.x (x * scaleFactor |> toString)
            , SvgA.y (y * scaleFactor |> toString)
            , SvgA.width (toString scaleFactor)
            , SvgA.height (toString scaleFactor)
            , SvgA.fill colorString
            , SvgA.stroke "black"
            ]
            []


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
