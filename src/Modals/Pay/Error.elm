module Modals.Pay.Error exposing (insufficientAsset)

import Element
    exposing
        ( Element
        , centerX
        , centerY
        , el
        , fill
        , height
        , paddingEach
        , px
        , shrink
        , text
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Utility.Color as Color


insufficientAsset : Element msg
insufficientAsset =
    el
        [ width fill
        , height <| px 44
        , paddingEach
            { top = 0
            , right = 16
            , bottom = 0
            , left = 10
            }
        , centerX
        , centerY
        , Background.color Color.negative100
        , Border.rounded 4
        , Font.size 16
        , Font.color Color.light500
        ]
        (el
            [ width shrink
            , height shrink
            , centerX
            , centerY
            , Font.bold
            , Font.size 16
            , Font.color Color.negative500
            ]
            (text "Insufficient asset")
        )
