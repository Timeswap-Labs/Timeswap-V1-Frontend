module Modal.Outside exposing (view)

import Data.Backdrop as Backdrop exposing (Backdrop)
import Data.Theme exposing (Theme)
import Element
    exposing
        ( Element
        , behindContent
        , centerX
        , centerY
        , el
        , fill
        , height
        , inFront
        , none
        , paddingXY
        , scrollbars
        , shrink
        , width
        )
import Element.Background as Background
import Element.Events as Events
import Utility.Blur as Blur
import Utility.Color as Color
import Utility.Pointer as Pointer
import Utility.ThemeColor as ThemeColor


view :
    { model | backdrop : Backdrop, theme : Theme }
    ->
        { onClick : msg
        , modal : Element msg
        }
    -> Element msg
view { backdrop, theme } { onClick, modal } =
    el
        [ width fill
        , height fill
        , el
            ([ width fill
             , height fill
             , Events.onClick onClick
             ]
                ++ (case backdrop of
                        Backdrop.Supported ->
                            (theme
                                |> ThemeColor.modalOutside
                                |> Background.color
                            )
                                :: Blur.ten

                        Backdrop.NotSupported ->
                            [ Background.color Color.solid ]
                   )
            )
            none
            |> behindContent
        , el
            [ width fill
            , height fill
            , paddingXY 0 100
            , scrollbars
            , Pointer.off
            ]
            (el
                [ width shrink
                , height shrink
                , centerX
                , centerY
                , Pointer.on
                ]
                modal
            )
            |> inFront
        ]
        none
