module Icon exposing (..)

import Svg exposing (Svg)
import Svg.Attributes as SvgAttr


type Icon
    = Icon
        { toSvg : Src
        , color : Maybe String
        , disabled : Bool
        , size : Size
        }


inColor : String -> Icon -> Icon
inColor color (Icon icon) =
    Icon { icon | color = Just color }


type Size
    = Small
    | Normal
    | Big
    | ReallyBig


type alias Attributes =
    { colorHexStr : String
    , fillOpacityStr : String
    , width : Svg.Attribute Never
    , height : Svg.Attribute Never
    , rectArea : String
    , hoverBoxW : Svg.Attribute Never
    , hoverBoxH : Svg.Attribute Never
    , disabled : Bool
    }


type alias Src =
    Attributes -> Svg Never


new : Src -> Icon
new src =
    Icon
        { toSvg = src
        , color = Nothing
        , disabled = False
        , size = Normal
        }


sizeToPxStr : Size -> String
sizeToPxStr size =
    String.fromInt <| sizeToPxInt size


sizeToPxInt : Size -> Int
sizeToPxInt size =
    case size of
        Small ->
            12

        Normal ->
            16

        Big ->
            24

        ReallyBig ->
            64


toHtml : Icon -> Svg msg
toHtml (Icon ({ toSvg } as icon)) =
    let
        rectArea : String
        rectArea =
            ((icon.size |> sizeToPxInt |> toFloat) * 1.3333)
                |> String.fromFloat
    in
    toSvg
        { height = SvgAttr.height (sizeToPxStr icon.size)
        , width = SvgAttr.width (sizeToPxStr icon.size)
        , rectArea = rectArea
        , hoverBoxH = SvgAttr.height rectArea
        , hoverBoxW = SvgAttr.width rectArea
        , colorHexStr =
            icon.color
                |> Maybe.withDefault "currentColor"
        , fillOpacityStr =
            if icon.disabled then
                ".24"

            else
                "1"
        , disabled = icon.disabled
        }
        |> Svg.map never


caret__Up : Src
caret__Up { height, width, colorHexStr } =
    Svg.svg
        [ width
        , height
        , SvgAttr.viewBox "0 0 16 16"
        , SvgAttr.fill "none"
        ]
        [ Svg.path
            [ SvgAttr.d "M3.33624 11.5758C3.48391 11.8378 3.76131 12 4.06207 12H12.1671C12.4677 11.9999 12.7449 11.8378 12.8925 11.576C13.0402 11.3142 13.0354 10.9931 12.88 10.7358L8.82749 4.04C8.67651 3.79056 8.40614 3.63817 8.11457 3.63817C7.823 3.63817 7.55263 3.79056 7.40165 4.04L3.34915 10.7358C3.1937 10.993 3.18877 11.314 3.33624 11.5758Z"
            , SvgAttr.fill colorHexStr
            ]
            []
        ]


caret__Down : Src
caret__Down { height, width, colorHexStr, fillOpacityStr, disabled } =
    Svg.svg
        [ width
        , height
        , SvgAttr.viewBox "0 0 16 16"
        ]
        [ Svg.g
            [ SvgAttr.fill "none"
            , SvgAttr.fillRule "evenodd"
            ]
            [ Svg.g []
                [ Svg.g []
                    [ Svg.path
                        [ SvgAttr.d "M0 0H16V16H0z"
                        , SvgAttr.transform "translate(-765 -152) translate(765 152)"
                        ]
                        []
                    , Svg.g
                        [ SvgAttr.fill colorHexStr
                        , SvgAttr.fillOpacity fillOpacityStr
                        , SvgAttr.fillRule "nonzero"
                        ]
                        [ Svg.path
                            [ SvgAttr.d "M9.89.406C9.74.156 9.455 0 9.147 0H.852C.545 0 .262.155.11.406c-.151.25-.146.557.013.803L4.27 7.616c.155.238.432.384.73.384s.575-.146.73-.384l4.147-6.407c.16-.246.164-.553.013-.803z"
                            , SvgAttr.transform "translate(-765 -152) translate(765 152) translate(3 4)"
                            ]
                            []
                        , Svg.rect
                            [ SvgAttr.fillOpacity ".1"
                            , SvgAttr.rx "4"
                            ]
                            []
                        ]
                    ]
                ]
            ]
        ]


caret__UpAndDown : Src
caret__UpAndDown { height, width, colorHexStr, fillOpacityStr } =
    Svg.svg
        [ width
        , height
        , SvgAttr.viewBox "0 0 16 16"
        ]
        [ Svg.g
            [ SvgAttr.fill "none"
            , SvgAttr.fillRule "evenodd"
            ]
            [ Svg.g
                []
                [ Svg.g
                    []
                    [ Svg.path
                        [ SvgAttr.d "M0 0H16V16H0z"
                        , SvgAttr.transform "translate(-168 -552) translate(168 552)"
                        ]
                        []
                    , Svg.g
                        [ SvgAttr.fill colorHexStr
                        , SvgAttr.fillOpacity fillOpacityStr
                        , SvgAttr.fillRule "nonzero"
                        ]
                        [ Svg.path
                            [ SvgAttr.d "M5.934 8.203C5.844 8.078 5.674 8 5.488 8H.511c-.184 0-.354.078-.445.203-.09.125-.088.279.008.402l2.488 3.203c.093.12.259.192.438.192s.345-.073.438-.192l2.488-3.203c.096-.123.099-.277.008-.402z"
                            , SvgAttr.transform "translate(-168 -552) translate(168 552) translate(5 2)"
                            ]
                            []
                        , Svg.path
                            [ SvgAttr.d "M5.934.203C5.844.078 5.674 0 5.488 0H.511C.327 0 .157.078.066.203c-.09.125-.088.279.008.402l2.488 3.203c.093.12.259.192.438.192s.345-.073.438-.192L5.926.605c.096-.123.099-.277.008-.402z"
                            , SvgAttr.transform "translate(-168 -552) translate(168 552) translate(5 2) matrix(1 0 0 -1 0 4)"
                            ]
                            []
                        ]
                    ]
                ]
            ]
        ]


magnifyingGlass : Src
magnifyingGlass { height, width, colorHexStr } =
    Svg.svg
        [ width
        , height
        , SvgAttr.viewBox "0 0 16 16"
        ]
        [ Svg.g
            [ SvgAttr.fill "none"
            , SvgAttr.fillRule "evenodd"
            ]
            [ Svg.g
                [ SvgAttr.fill colorHexStr
                , SvgAttr.fillRule "nonzero"
                ]
                [ Svg.g []
                    [ Svg.g
                        []
                        [ Svg.path
                            [ SvgAttr.d "M15.61 13.731l-3.095-3.096c1.952-2.924 1.37-6.853-1.348-9.084-2.717-2.23-6.682-2.035-9.166.452-2.485 2.487-2.68 6.455-.449 9.172 2.23 2.718 6.157 3.3 9.079 1.346l3.095 3.095c.524.512 1.36.512 1.884 0 .52-.52.52-1.364 0-1.885zM6.84 2.008c2.667 0 4.83 2.164 4.83 4.833 0 2.67-2.163 4.833-4.83 4.833-2.668 0-4.831-2.164-4.831-4.833.003-2.668 2.164-4.83 4.83-4.833z"
                            , SvgAttr.transform "translate(-224 -32) translate(208 16) translate(16 16)"
                            ]
                            []
                        ]
                    ]
                ]
            ]
        ]
