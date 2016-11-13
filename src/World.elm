module World exposing (World, tick, init, stateAtPosition, generator)

import Array exposing (Array)
import Dict
import Random
import Random.Dict as RandomDict
import Random.Extra as RandomExtra
import Types exposing (ViewPort, Direction(..), Position, Ant(..), State, Grid, LR(..))


type alias World =
    { grid : Grid
    , ant : Ant
    , genome : Genome
    }


type Color
    = White


type alias Genome =
    Array LR


defaultGenome : Genome
defaultGenome =
    Array.fromList [ Left, Right, Right, Left ]


genomeSize : Genome -> Int
genomeSize genome =
    Array.length genome


lr : Genome -> State -> LR
lr genome state =
    let
        maybeLr =
            Array.get state genome
    in
        case maybeLr of
            Just lr ->
                lr

            Nothing ->
                Debug.crash
                    ("this is an impossible state: incorrect value was: "
                        ++ (toString state)
                    )


init : World
init =
    { grid = Dict.empty
    , ant = Ant ( 0, 0 ) West
    , genome = defaultGenome
    }


stateAtPosition : Grid -> Position -> State
stateAtPosition grid position =
    Dict.get position grid
        |> Maybe.withDefault 0


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


changeState : Grid -> Position -> Genome -> Grid
changeState grid position genome =
    let
        currentState =
            stateAtPosition grid position

        nextState =
            (currentState + 1) % (genomeSize genome)
    in
        if nextState == 0 then
            Dict.remove position grid
        else
            Dict.insert position nextState grid


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


tick : World -> World
tick { grid, ant, genome } =
    case ant of
        Ant position direction ->
            let
                rotate =
                    let
                        currentState =
                            stateAtPosition grid position
                    in
                        case lr genome currentState of
                            Left ->
                                rotateLeft

                            Right ->
                                rotateRight
            in
                { grid = changeState grid position genome
                , ant = ant |> rotate |> moveForward
                , genome = genome
                }


generator : Random.Generator World
generator =
    let
        min =
            -10

        max =
            10

        randomPosition =
            Random.pair (Random.int min max) (Random.int min max)

        randomState =
            Random.int 0 ((genomeSize defaultGenome) - 1)

        randomGrid =
            RandomDict.dict 100 randomPosition randomState

        randomDirection =
            RandomExtra.sample [ North, East, South, West ]
                |> Random.map (Maybe.withDefault North)

        randomAnt =
            Random.map2 (\p d -> Ant p d) randomPosition randomDirection
    in
        Random.map2
            (\grid ant ->
                { grid = grid
                , ant = ant
                , genome = defaultGenome
                }
            )
            randomGrid
            randomAnt
