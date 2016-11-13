module View exposing (Renderer, view, defaultRenderer, switchRenderer)

import Html exposing (Html)
import View.ViewCanvas as ViewCanvas
import View.ViewSvg as ViewSvg
import Types exposing (ViewPort, Ant, Grid, Position)
import World


type Renderer
    = Svg
    | Canvas


defaultRenderer =
    Svg


switchRenderer : Renderer -> Renderer
switchRenderer renderer =
    case renderer of
        Svg ->
            Canvas

        Canvas ->
            Svg


view : Renderer -> Grid -> Ant -> ViewPort -> Html msg
view renderer grid ant viewPort =
    case renderer of
        Svg ->
            ViewSvg.view grid ant viewPort

        Canvas ->
            ViewCanvas.view grid ant viewPort
