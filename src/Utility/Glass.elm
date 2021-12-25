module Utility.Glass exposing (background, warning)

import Data.Backdrop as Backdrop exposing (Backdrop)
import Element
    exposing
        ( Attribute
        , htmlAttribute
        )
import Element.Background as Background
import Html.Attributes
import Utility.Color as Color


background : Backdrop -> List (Attribute msg)
background backdrop =
    case backdrop of
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


warning : Backdrop -> List (Attribute msg)
warning backdrop =
    case backdrop of
        Backdrop.Supported ->
            [ Background.color Color.warning100
            , "blur(60px)"
                |> Html.Attributes.style "-webkit-backdrop-filter"
                |> htmlAttribute
            , "blur(60px)"
                |> Html.Attributes.style "backdrop-filter"
                |> htmlAttribute
            ]

        Backdrop.NotSupported ->
            [ Background.color Color.warning500 ]
