module View.ViewCanvas exposing (view)

import Html exposing (Html)
import Element
import Collage exposing (Form)
import Types exposing (ViewPort, Ant(..), Color(..), Grid, Direction(..), Position)
import World exposing (colorAtPosition)
import Color as CanvasColor exposing (black, white)
import View.Helpers exposing (scale, positionsToDraw)


view : Grid -> Ant -> ViewPort -> Html msg
view grid ant viewPort =
    let
        forms =
            viewGrid viewPort grid
    in
        Element.toHtml (Collage.collage viewPort.size viewPort.size forms)


viewGrid : ViewPort -> Grid -> List Form
viewGrid viewPort grid =
    List.map (\pos -> ( pos, colorAtPosition grid pos )) (positionsToDraw viewPort grid)
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
