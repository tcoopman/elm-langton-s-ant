module Main exposing (..)

import Html exposing (Html)
import Html.App as App
import Dict exposing (Dict)
import Svg exposing (Svg, svg, rect)
import Svg.Attributes as SvgA
import Time exposing (Time, millisecond)


type Direction
    = North
    | East
    | South
    | West


type alias Position =
    ( Int, Int )


type Ant
    = Ant Position Direction


type Color
    = Black
    | White


type alias Grid =
    Dict Position Color


colorAtPosition : Grid -> Position -> Color
colorAtPosition grid position =
    Dict.get position grid
        |> Maybe.withDefault White


type alias ViewPort =
    { xMin : Int
    , yMin : Int
    , xMax : Int
    , yMax : Int
    , scale : Int
    }


type alias Model =
    { grid : Grid
    , ant : Ant
    , viewPort : ViewPort
    }


type Msg
    = Tick Time


newModel : Model
newModel =
    { grid = Dict.empty
    , ant = Ant ( 0, 0 ) West
    , viewPort = { xMin = -10, yMin = -10, xMax = 10, yMax = 10, scale = 20 }
    }


view : Model -> Html Msg
view { grid, ant, viewPort } =
    let
        widthString =
            viewPort.xMax - viewPort.xMin |> (*) viewPort.scale |> abs |> toString

        heightString =
            viewPort.yMax - viewPort.yMin |> (*) viewPort.scale |> abs |> toString

        viewBoxString =
            (viewPort.xMin * viewPort.scale |> toString) ++ " " ++ (viewPort.yMin * viewPort.scale |> toString) ++ " " ++ widthString ++ " " ++ heightString
    in
        svg
            [ SvgA.width widthString
            , SvgA.height heightString
            , SvgA.viewBox viewBoxString
            ]
            [ (viewGrid viewPort grid), (viewAnt viewPort ant) ]


viewAnt : ViewPort -> Ant -> Svg msg
viewAnt { scale } (Ant ( x, y ) direction) =
    let
        half =
            scale // 2 |> toString

        full =
            scale |> toString

        pathString =
            "M" ++ half ++ " 0 L" ++ full ++ " " ++ half ++ " L0 " ++ half ++ " L" ++ half ++ " 0"

        arrow =
            Svg.path
                [ SvgA.d (Debug.log "path: " pathString)
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
            "translate(" ++ (x * scale |> toString) ++ "," ++ (y * scale |> toString) ++ ")"
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
                    List.map (square row) [viewPort.xMin..viewPort.xMax]
            in
                List.concatMap rows [viewPort.yMin..viewPort.yMax]
    in
        Svg.g []
            (List.map (\pos -> ( pos, colorAtPosition grid pos )) positionsToDraw
                |> List.map (\( pos, color ) -> viewSquare viewPort pos color)
            )


viewSquare : ViewPort -> Position -> Color -> Svg msg
viewSquare { xMin, xMax, yMin, yMax, scale } ( x, y ) color =
    let
        colorString =
            case color of
                White ->
                    "white"

                Black ->
                    "black"
    in
        rect
            [ SvgA.x (x * scale |> toString)
            , SvgA.y (y * scale |> toString)
            , SvgA.width (toString scale)
            , SvgA.height (toString scale)
            , SvgA.fill colorString
            , SvgA.stroke "black"
            ]
            []


rotateRight : Ant -> Ant
rotateRight (Ant position direction) =
    case direction of
        North ->
            Ant position East

        East ->
            Ant position South

        South ->
            Ant position West

        West ->
            Ant position North


rotateLeft : Ant -> Ant
rotateLeft (Ant position direction) =
    case direction of
        North ->
            Ant position West

        West ->
            Ant position South

        South ->
            Ant position East

        East ->
            Ant position North


flipColor : Grid -> Position -> Grid
flipColor grid position =
    case colorAtPosition grid position of
        Black ->
            Dict.remove position grid

        White ->
            Dict.insert position Black grid


moveForward : Ant -> Ant
moveForward (Ant ( x, y ) direction) =
    case direction of
        North ->
            Ant ( x, y - 1 ) direction

        East ->
            Ant ( x + 1, y ) direction

        South ->
            Ant ( x, y + 1 ) direction

        West ->
            Ant ( x - 1, y ) direction


tick : ( Grid, Ant ) -> ( Grid, Ant )
tick ( grid, ant ) =
    case ant of
        Ant position direction ->
            case colorAtPosition grid position of
                White ->
                    ( flipColor grid position, ant |> rotateRight |> moveForward )

                Black ->
                    ( flipColor grid position, ant |> rotateLeft |> moveForward )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            let
                ( grid, ant ) =
                    tick ( model.grid, model.ant )
            in
                ( { model | grid = grid, ant = ant }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every (2000 * millisecond) Tick


main =
    App.program
        { init = ( newModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
