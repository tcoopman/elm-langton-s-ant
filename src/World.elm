module World exposing (tick, init, colorAtPosition, Ant(..), Color(..), Grid, Direction(..), Position)

import Dict exposing (Dict)


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


init : ( Grid, Ant )
init =
    ( Dict.empty, Ant ( 0, 0 ) West )


colorAtPosition : Grid -> Position -> Color
colorAtPosition grid position =
    Dict.get position grid
        |> Maybe.withDefault White


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
