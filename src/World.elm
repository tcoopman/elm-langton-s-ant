module World exposing (tick, init, colorAtPosition, generator)

import Dict
import Random
import Random.Dict as RandomDict
import Random.Extra as RandomExtra
import Types exposing (ViewPort, Direction(..), Position, Ant(..), Color(..), Grid)


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


generator : Random.Generator ( Grid, Ant )
generator =
    let
        min =
            -10

        max =
            10

        randomPosition =
            Random.pair (Random.int min max) (Random.int min max)

        randomColor =
            RandomExtra.choice White Black

        randomGrid =
            RandomDict.dict 100 randomPosition randomColor

        randomDirection =
            RandomExtra.sample [ North, East, South, West ]
                |> Random.map (Maybe.withDefault North)

        randomAnt =
            Random.map2 (\p d -> Ant p d) randomPosition randomDirection
    in
        Random.pair randomGrid randomAnt
