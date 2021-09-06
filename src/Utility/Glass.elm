module Utility.Glass exposing
    ( darkPrimary
    , lightPrimary
    , lightPrimaryModal
    , lightTransparent
    , lightTransparent100
    , lightTransparent300
    , lightWhite
    , lightWhiteModal
    )

import Data.Backdrop exposing (Backdrop(..))
import Element
    exposing
        ( Attribute
        , Color
        , Element
        , alpha
        , behindContent
        , el
        , fill
        , fromRgb
        , height
        , html
        , htmlAttribute
        , none
        , rgba255
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Html.Attributes
import Svg
import Svg.Attributes
import Utility.Color as Color


lightPrimary : Int -> List (Attribute msg)
lightPrimary =
    light Color.primary500


darkPrimary : Int -> List (Attribute msg)
darkPrimary =
    dark Color.primary500


lightWhite : Int -> List (Attribute msg)
lightWhite =
    glass 0.02 0.02 blur21 (rgba255 255 255 255 0.02)


lightWhiteModal : Int -> List (Attribute msg)
lightWhiteModal corner =
    [ behindContent <| gradientBorder corner
    , el
        ([ width fill
         , height fill
         , alpha 0.32
         ]
            ++ lightWhite corner
        )
        none
        |> behindContent
    , Border.rounded corner
    ]


lightTransparent : Int -> List (Attribute msg)
lightTransparent =
    light Color.transparent500


lightTransparent300 : Int -> List (Attribute msg)
lightTransparent300 =
    glassAngle 0.2 0.08 (pi / 2) blur40 Color.transparent300


lightTransparent100 : Int -> List (Attribute msg)
lightTransparent100 =
    glass 0.24 0.12 blur40 Color.completelyTransparent


lightPrimaryModal : Backdrop -> Int -> List (Attribute msg)
lightPrimaryModal backdrop corner =
    case backdrop of
        Supported ->
            lightPrimary corner

        NotSupported ->
            solid Color.darkModal corner


light : Color -> Int -> List (Attribute msg)
light =
    glass 0.12 0.02 blur21


dark : Color -> Int -> List (Attribute msg)
dark =
    glass 0.24 0.12 blur40


solid : Color -> Int -> List (Attribute msg)
solid color corner =
    [ Background.color color
    , behindContent <| gradientBorder corner
    , Border.rounded corner
    ]


glass : Float -> Float -> String -> Color -> Int -> List (Attribute msg)
glass zero hundred blur color corner =
    glassAngle zero hundred pi blur color corner


glassAngle : Float -> Float -> Float -> String -> Color -> Int -> List (Attribute msg)
glassAngle zero hundred angle blur color corner =
    gradient zero hundred angle blur color
        ++ [ behindContent <| gradientBorder corner
           , Border.rounded corner
           ]


gradient : Float -> Float -> Float -> String -> Color -> List (Attribute msg)
gradient zero hundred angle blur color =
    [ Background.gradient
        { angle = angle
        , steps =
            [ changeAlpha zero color
            , changeAlpha hundred color
            ]
        }
    , htmlAttribute <| Html.Attributes.style "-webkit-backdrop-filter" blur
    , htmlAttribute <| Html.Attributes.style "backdrop-filter" blur
    ]


type alias Rgb =
    { red : Float
    , green : Float
    , blue : Float
    , alpha : Float
    }


changeAlpha : Float -> Color -> Color
changeAlpha alpha color =
    let
        rgb : Rgb
        rgb =
            color
                |> Element.toRgb
    in
    { rgb | alpha = alpha }
        |> fromRgb


blur21 : String
blur21 =
    "blur(21px)"


blur40 : String
blur40 =
    "blur(40px)"


gradientBorder : Int -> Element msg
gradientBorder corner =
    Svg.svg
        [ Svg.Attributes.width "100%"
        , Svg.Attributes.height "100%"
        , Svg.Attributes.fill "none"
        , Svg.Attributes.xmlSpace "http://www.w3.org/2000/svg"
        ]
        [ Svg.defs []
            [ Svg.linearGradient
                [ Svg.Attributes.id "gradient"
                , Svg.Attributes.x1 "0%"
                , Svg.Attributes.y1 "0%"
                , Svg.Attributes.x2 "0%"
                , Svg.Attributes.y2 "100%"
                ]
                [ Svg.stop
                    [ Svg.Attributes.offset "0%"
                    , Svg.Attributes.style "stop-color:rgb(255,255,255);stop-opacity:0.1"
                    ]
                    []
                , Svg.stop
                    [ Svg.Attributes.offset "100%"
                    , Svg.Attributes.style "stop-color:rgb(255,255,255);stop-opacity:0.0"
                    ]
                    []
                ]
            ]
        , Svg.rect
            [ Svg.Attributes.width "100%"
            , Svg.Attributes.height "100%"
            , Svg.Attributes.rx <| String.fromInt corner
            , Svg.Attributes.ry <| String.fromInt corner
            , Svg.Attributes.stroke "url(#gradient)"
            , Svg.Attributes.style "stroke-width:1"
            ]
            []
        ]
        |> html
