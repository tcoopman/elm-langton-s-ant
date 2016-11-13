module View.ViewSvg exposing (view)

import Svg exposing (Svg, svg, rect)
import Svg.Attributes as SvgA
import Types exposing (ViewPort, Ant(..), Grid, Direction(..), State, Position)
import World exposing (stateAtPosition)
import View.Helpers exposing (scale, positionsToDraw)
import ColorHelper


view : Grid -> Ant -> ViewPort -> Svg msg
view grid ant viewPort =
    let
        scaleFactor =
            scale viewPort

        min =
            viewPort.min * (scale viewPort) |> toString

        size =
            viewPort.size * 2 |> toString

        viewBoxString =
            min ++ " " ++ min ++ " " ++ (viewPort.size * 2 |> toString) ++ " " ++ (viewPort.size * 2 |> toString)
    in
        svg
            [ SvgA.width size
            , SvgA.height size
            , SvgA.viewBox viewBoxString
            ]
            [ (viewGrid viewPort grid), (viewAnt viewPort ant) ]


viewAnt : ViewPort -> Ant -> Svg msg
viewAnt viewPort (Ant ( x, y ) direction) =
    let
        scaleFactor =
            scale viewPort

        half =
            scaleFactor // 2 |> toString

        full =
            scaleFactor |> toString

        pathString =
            "M" ++ half ++ " 0 L" ++ full ++ " " ++ half ++ " L0 " ++ half ++ " L" ++ half ++ " 0"

        arrow =
            Svg.path
                [ SvgA.d pathString
                , SvgA.style "fill: #76daff"
                ]
                []

        rotate =
            let
                rotation =
                    case direction of
                        North ->
                            "0"

                        East ->
                            "90"

                        South ->
                            "180"

                        West ->
                            "270"
            in
                "rotate(" ++ rotation ++ "," ++ half ++ "," ++ half ++ ")"

        translate =
            "translate(" ++ (x * scaleFactor |> toString) ++ "," ++ (y * scaleFactor |> toString) ++ ")"
    in
        Svg.g [ SvgA.transform (translate ++ "," ++ rotate) ] [ arrow ]


viewGrid : ViewPort -> Grid -> Svg msg
viewGrid viewPort grid =
    Svg.g []
        (List.map (\pos -> ( pos, stateAtPosition grid pos )) (positionsToDraw viewPort grid)
            |> List.map (\( pos, state ) -> viewSquare viewPort pos state)
        )


viewSquare : ViewPort -> Position -> State -> Svg msg
viewSquare viewPort ( x, y ) state =
    let
        color =
            ColorHelper.stateToColorString state

        scaleFactor =
            scale viewPort
    in
        rect
            [ SvgA.x (x * scaleFactor |> toString)
            , SvgA.y (y * scaleFactor |> toString)
            , SvgA.width (toString scaleFactor)
            , SvgA.height (toString scaleFactor)
            , SvgA.fill color
            , SvgA.stroke "black"
            ]
            []
