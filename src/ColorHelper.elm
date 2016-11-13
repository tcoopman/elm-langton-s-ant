module ColorHelper exposing (stateToColor, stateToColorString)

import Types exposing (State)
import Color exposing (Color)


stateToColorTupple : State -> ( Int, Int, Int )
stateToColorTupple state =
    case state of
        0 ->
            ( 54, 115, 139 )

        1 ->
            ( 217, 169, 95 )

        2 ->
            ( 198, 148, 87 )

        3 ->
            ( 161, 150, 128 )

        4 ->
            ( 179, 134, 119 )

        5 ->
            ( 137, 126, 81 )

        6 ->
            ( 131, 131, 97 )

        7 ->
            ( 172, 122, 51 )

        8 ->
            ( 163, 128, 70 )

        9 ->
            ( 104, 121, 113 )

        _ ->
            ( 54, 115, 139 )


stateToColor : State -> Color
stateToColor state =
    stateToColorTupple state
        |> \( r, g, b ) -> Color.rgb r g b


stateToColorString : State -> String
stateToColorString state =
    stateToColorTupple state
        |> \( r, g, b ) -> "rgb(" ++ (toString r) ++ "," ++ (toString g) ++ "," ++ (toString b) ++ ")"
