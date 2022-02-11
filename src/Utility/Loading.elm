module Utility.Loading exposing (view)

import Animator exposing (Timeline)
import Animator.Css
import Data.Theme exposing (Theme)
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
import Utility.ThemeColor as ThemeColor


view : Timeline () -> Theme -> Element msg
view timeline theme =
    row
        [ width shrink
        , height shrink
        , padding 2
        , spacing 2
        ]
        [ blinkingCircle 0.2 theme timeline
        , blinkingCircle 0.1 theme timeline
        , blinkingCircle 0 theme timeline
        ]


blinkingCircle : Float -> Theme -> Timeline () -> Element msg
blinkingCircle offset theme timeline =
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
                , theme |> ThemeColor.textLight |> Background.color
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
