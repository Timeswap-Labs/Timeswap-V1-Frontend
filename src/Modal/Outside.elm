module Modal.Outside exposing (view)

import Data.Backdrop as Backdrop exposing (Backdrop)
import Element
    exposing
        ( Element
        , behindContent
        , centerX
        , centerY
        , el
        , fill
        , height
        , none
        , paddingXY
        , shrink
        , width
        )
import Element.Background as Background
import Element.Events as Events
import Utility.Blur as Blur
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
        , paddingXY 0 100
        , el
            ([ width fill
             , height fill
             , Events.onClick onClick
             ]
                ++ (case backdrop of
                        Backdrop.Supported ->
                            Background.color Color.outside
                                :: Blur.ten

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
