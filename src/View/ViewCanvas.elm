module View.ViewCanvas exposing (view)

import Html exposing (Html)
import Element
import Collage exposing (Form)
import Types exposing (ViewPort, Ant(..), Grid, State, Direction(..), Position)
import World exposing (stateAtPosition)
import View.Helpers exposing (scale, positionsToDraw)
import ColorHelper


view : Grid -> Ant -> ViewPort -> Html msg
view grid ant viewPort =
    let
        forms =
            viewGrid viewPort grid
    in
        Element.toHtml (Collage.collage viewPort.size viewPort.size forms)


viewGrid : ViewPort -> Grid -> List Form
viewGrid viewPort grid =
    List.map (\pos -> ( pos, stateAtPosition grid pos )) (positionsToDraw viewPort grid)
        |> List.map (\( pos, state ) -> createSquare viewPort pos state)


createSquare : ViewPort -> Position -> State -> Form
createSquare viewPort ( x, y ) state =
    let
        canvasColor =
            ColorHelper.stateToColor state

        scaleFactor =
            scale viewPort |> toFloat
    in
        Collage.rect scaleFactor scaleFactor
            |> Collage.filled canvasColor
            |> Collage.move ( (toFloat x) * scaleFactor, (toFloat y) * scaleFactor )
