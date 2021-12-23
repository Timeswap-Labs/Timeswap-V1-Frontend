module Utility.Loading exposing (view)

import Animator exposing (Timeline)
import Animator.Css
import Element
    exposing
        ( Element
        , Option
        , el
        , focusStyle
        , height
        , html
        , layoutWith
        , noStaticStyleSheet
        , none
        , padding
        , px
        , row
        , shrink
        , spacing
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Utility.Color as Color


view : Timeline () -> Element msg
view timeline =
    row
        [ width shrink
        , height shrink
        , padding 2
        , spacing 2
        ]
        [ blinkingCircle 0.2 timeline
        , blinkingCircle 0.1 timeline
        , blinkingCircle 0 timeline
        ]


blinkingCircle : Float -> Timeline () -> Element msg
blinkingCircle offset timeline =
    el
        [ width shrink
        , height shrink
        ]
        (Animator.Css.div timeline
            [ Animator.Css.opacity
                (\() ->
                    Animator.wave 0.05 1
                        |> Animator.shift offset
                        |> Animator.loop (Animator.millis 1000)
                )
            ]
            []
            [ layoutWith { options = options }
                [ width <| px 5
                , height <| px 5
                , Background.color Color.transparent100
                , Border.rounded 999
                ]
                none
            ]
            |> html
        )


options : List Option
options =
    [ { borderColor = Nothing
      , backgroundColor = Nothing
      , shadow = Nothing
      }
        |> focusStyle
    , noStaticStyleSheet
    ]
