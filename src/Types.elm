module Types exposing (..)

import Dict exposing (Dict)


type alias ViewPort =
    { min : Int
    , max : Int
    , size : Int
    }


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
