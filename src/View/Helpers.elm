module View.Helpers exposing (scale, positionsToDraw)

import Types exposing (ViewPort, Grid, Position)
import Dict


scale : ViewPort -> Int
scale { min, max, size } =
    size // (max - min) |> abs


positionsToDraw : ViewPort -> Grid -> List Position
positionsToDraw viewPort grid =
    Dict.keys grid
        |> List.filter (\( x, y ) -> (x > viewPort.min && x < viewPort.max) && (y > viewPort.min && y < viewPort.max))
