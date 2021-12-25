module Modal.Outside exposing (view)

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


view :
    { model | backdrop : Backdrop }
    ->
        { onClick : msg
        , modal : Element msg
        }
    -> Element msg
view { backdrop } { onClick, modal } =
    el
        [ width fill
        , height fill
        , paddingXY 0 80
        , el
            ([ width fill
             , height fill
             , Events.onClick onClick
             ]
                ++ (case backdrop of
                        Backdrop.Supported ->
                            [ Background.color Color.outside
                            , "blur(10px)"
                                |> Html.Attributes.style "-webkit-backdrop-filter"
                                |> htmlAttribute
                            , "blur(10px)"
                                |> Html.Attributes.style "backdrop-filter"
                                |> htmlAttribute
                            ]

                        Backdrop.NotSupported ->
                            [ Background.color Color.solid ]
                   )
            )
            none
            |> behindContent
        ]
        (el
            [ width shrink
            , height shrink
            , centerX
            , centerY
            ]
            modal
        )
