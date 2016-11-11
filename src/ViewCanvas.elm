module ViewCanvas exposing (view)

import Html exposing (Html)
import Element
import Collage exposing (Form)
import Types exposing (ViewPort)
import World exposing (colorAtPosition, Ant(..), Color(..), Grid, Direction(..), Position)
import Color as CanvasColor exposing (black, white)


scale : ViewPort -> Int
scale { min, max, size } =
    size // (max - min) |> abs


view : Grid -> Ant -> ViewPort -> Html msg
view grid ant viewPort =
    let
        forms =
            viewGrid viewPort grid
    in
        Element.toHtml (Collage.collage viewPort.size viewPort.size forms)


viewGrid : ViewPort -> Grid -> List Form
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
        List.map (\pos -> ( pos, colorAtPosition grid pos )) positionsToDraw
            |> List.map (\( pos, color ) -> createSquare viewPort pos color)


createSquare : ViewPort -> Position -> Color -> Form
createSquare viewPort ( x, y ) color =
    let
        canvasColor =
            case color of
                White ->
                    white

                Black ->
                    black

        scaleFactor =
            scale viewPort |> toFloat
    in
        Collage.rect scaleFactor scaleFactor
            |> Collage.filled canvasColor
            |> Collage.move ( (toFloat x) * scaleFactor, (toFloat y) * scaleFactor )
