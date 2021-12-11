module Utility.Glass exposing (background, outsideModal)

import Data.Backdrop as Backdrop exposing (Backdrop)
import Element
    exposing
        ( Attribute
        , Element
        , behindContent
        , el
        , fill
        , height
        , htmlAttribute
        , none
        , paddingXY
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
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


outsideModal :
    { model | backdrop : Backdrop }
    ->
        { onClick : msg
        , modal : Element msg
        }
    -> Element msg
outsideModal { backdrop } { onClick, modal } =
    el
        [ width fill
        , height fill
        , paddingXY 0 72
        , el
            [ width fill
            , height fill
            , el
                ([ width fill
                 , height fill
                 , Events.onClick onClick
                 , Background.color Color.outside
                 ]
                    ++ (case backdrop of
                            Backdrop.Supported ->
                                [ "blur(10px)"
                                    |> Html.Attributes.style "-webkit-backdrop-filter"
                                    |> htmlAttribute
                                , "blur(10px)"
                                    |> Html.Attributes.style "backdrop-filter"
                                    |> htmlAttribute
                                ]

                            Backdrop.NotSupported ->
                                []
                       )
                )
                none
                |> behindContent
            ]
            none
            |> behindContent
        ]
        modal
