module View.Helpers exposing (scale, positionsToDraw)

import Types exposing (ViewPort, Grid, Position)


scale : ViewPort -> Int
scale { min, max, size } =
    size // (max - min) |> abs


positionsToDraw : ViewPort -> Grid -> List Position
positionsToDraw viewPort grid =
    let
        square row column =
            ( column, row )

        rows row =
            List.map (square row) [viewPort.min..viewPort.max]
    in
        List.concatMap rows [viewPort.min..viewPort.max]
