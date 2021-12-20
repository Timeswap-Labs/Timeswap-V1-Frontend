module Utility.Glass exposing (background)

import Data.Backdrop as Backdrop exposing (Backdrop)
import Element
    exposing
        ( Attribute
        , Element
        , behindContent
        , centerX
        , centerY
        , el
        , fill
        , height
        , htmlAttribute
        , none
        , paddingXY
        , shrink
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Html
import Html.Attributes
import Utility.Color as Color


background : Backdrop -> Attribute msg
background backdrop =
    el
        ([ width fill
         , height fill
         , Border.rounded 8
         ]
            ++ (case backdrop of
                    Backdrop.Supported ->
                        [ Background.color Color.background
                        , "blur(60px)"
                            |> Html.Attributes.style "-webkit-backdrop-filter"
                            |> htmlAttribute
                        , "blur(60px)"
                            |> Html.Attributes.style "backdrop-filter"
                            |> htmlAttribute
                        ]

                    Backdrop.NotSupported ->
                        [ Background.color Color.solid ]
               )
        )
        none
        |> behindContent
