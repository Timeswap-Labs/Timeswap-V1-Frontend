module Utility.Tooltip exposing (belowAlignLeft, belowAlignRight)

import Data.Theme as Theme exposing (Theme)
import Element
    exposing
        ( Attribute
        , Element
        , alignLeft
        , alignRight
        , centerX
        , column
        , el
        , fill
        , height
        , html
        , htmlAttribute
        , moveDown
        , padding
        , shrink
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Html.Attributes
import Svg
import Svg.Attributes
import Utility.ThemeColor as ThemeColor


belowAlignLeft :
    Theme
    -> Element msg
    -> Element msg
belowAlignLeft theme element =
    column
        [ width fill
        , height shrink
        , moveDown 4
        ]
        [ el
            [ centerX ]
            (triangleUp theme)
        , el
            [ width shrink
            , height shrink
            , alignLeft
            , padding 12
            , theme |> ThemeColor.tooltipBG |> Background.color
            , Border.rounded 4
            , shadow
            ]
            element
        ]


belowAlignRight :
    Theme
    -> Element msg
    -> Element msg
belowAlignRight theme element =
    column
        [ width fill
        , height shrink
        , moveDown 4
        ]
        [ el
            [ centerX ]
            (triangleUp theme)
        , el
            [ width shrink
            , height shrink
            , alignRight
            , padding 12
            , theme |> ThemeColor.tooltipBG |> Background.color
            , Border.rounded 4
            , shadow
            ]
            element
        ]


triangleUp : Theme -> Element msg
triangleUp theme =
    Svg.svg
        [ Svg.Attributes.width "15"
        , Svg.Attributes.height "7"
        , Svg.Attributes.fill "none"
        , Svg.Attributes.xmlSpace "http://www.w3.org/2000/svg"
        ]
        [ Svg.path
            [ Svg.Attributes.d "M14.5 7L0.5 7L7.5 6.11959e-07L14.5 7Z"
            , Svg.Attributes.fill
                (case theme of
                    Theme.Dark ->
                        "#0F1426"

                    Theme.Light ->
                        "#E6E8EF"
                )
            ]
            []
        ]
        |> html


triangleLeft : Element msg
triangleLeft =
    Svg.svg
        [ Svg.Attributes.width "7"
        , Svg.Attributes.height "14"
        , Svg.Attributes.fill "none"
        , Svg.Attributes.xmlSpace "http://www.w3.org/2000/svg"
        ]
        [ Svg.path
            [ Svg.Attributes.d "M7 0L7 14L-3.0598e-07 7L7 0Z"
            , Svg.Attributes.fill "#0F1426"
            ]
            []
        ]
        |> html


shadow : Attribute msg
shadow =
    Html.Attributes.style
        "box-shadow"
        "0px 6px 12px -4px rgba(0, 0, 0, 0.08), 0px 20px 24px -4px rgba(0, 0, 0, 0.2)"
        |> htmlAttribute
