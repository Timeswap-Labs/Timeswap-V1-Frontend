module Utility.Tooltip exposing (below)

import Element
    exposing
        ( Attribute
        , Element
        , Length
        , centerX
        , column
        , el
        , fill
        , height
        , html
        , htmlAttribute
        , moveDown
        , padding
        , paragraph
        , shrink
        , text
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html.Attributes
import Svg
import Svg.Attributes
import Utility.Color as Color
import Utility.Direction as Direction exposing (Direction)


below :
    { width : Length
    , align : Direction ()
    , move : Direction Int
    , text : String
    }
    -> Element msg
below param =
    column
        [ width fill
        , height shrink
        , moveDown 4
        ]
        [ el
            [ centerX ]
            triangleUp
        , paragraph
            [ width param.width
            , height shrink
            , padding 12
            , param.align |> Direction.toAlign
            , param.move |> Direction.toMove
            , Background.color Color.dark500
            , Border.rounded 4
            , shadow
            , Font.size 12
            , Font.color Color.transparent300
            ]
            [ text param.text ]
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
