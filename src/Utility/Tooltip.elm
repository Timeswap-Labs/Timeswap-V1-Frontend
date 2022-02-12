module Utility.Tooltip exposing (belowAlignLeft, belowAlignRight)

import Data.Theme exposing (Theme)
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
import Utility.Color as Color
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
            triangleUp
        , el
            [ width shrink
            , height shrink
            , alignLeft
            , padding 12
            , Background.color Color.dark500
            , Border.rounded 4
            , shadow
            ]
            element
        ]


belowAlignRight :
    Element msg
    -> Element msg
belowAlignRight element =
    column
        [ width fill
        , height shrink
        , moveDown 4
        ]
        [ el
            [ centerX ]
            triangleUp
        , el
            [ width shrink
            , height shrink
            , alignRight
            , padding 12
            , Background.color Color.dark500
            , Border.rounded 4
            , shadow
            ]
            element
        ]


triangleUp : Element msg
triangleUp =
    Svg.svg
        [ Svg.Attributes.width "15"
        , Svg.Attributes.height "7"
        , Svg.Attributes.fill "none"
        , Svg.Attributes.xmlSpace "http://www.w3.org/2000/svg"
        ]
        [ Svg.path
            [ Svg.Attributes.d "M14.5 7L0.5 7L7.5 6.11959e-07L14.5 7Z"
            , Svg.Attributes.fill "#0F1426"
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
