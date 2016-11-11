module ViewSvg exposing (view)

import Svg exposing (Svg, svg, rect)
import Svg.Attributes as SvgA
import Types exposing (ViewPort)
import World exposing (colorAtPosition, Ant(..), Color(..), Grid, Direction(..), Position)


scale : ViewPort -> Int
scale { min, max, size } =
    size // (max - min) |> abs


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
        Svg.g []
            (List.map (\pos -> ( pos, colorAtPosition grid pos )) positionsToDraw
                |> List.map (\( pos, color ) -> viewSquare viewPort pos color)
            )


viewSquare : ViewPort -> Position -> Color -> Svg msg
viewSquare viewPort ( x, y ) color =
    let
        colorString =
            case color of
                White ->
                    "white"

                Black ->
                    "black"

        scaleFactor =
            scale viewPort
    in
        rect
            [ SvgA.x (x * scaleFactor |> toString)
            , SvgA.y (y * scaleFactor |> toString)
            , SvgA.width (toString scaleFactor)
            , SvgA.height (toString scaleFactor)
            , SvgA.fill colorString
            , SvgA.stroke "black"
            ]
            []
