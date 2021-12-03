module Page.Transaction.Info exposing (emptyAPR, emptyCDP)

import Element
    exposing
        ( Element
        , column
        , el
        , fill
        , height
        , none
        , padding
        , px
        , shrink
        , spacing
        , text
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Utility.Color as Color


emptyAPR : Element Never
emptyAPR =
    column
        [ width fill
        , height shrink
        , padding 12
        , spacing 5
        , Background.color Color.transparent100
        , Border.rounded 8
        ]
        [ el
            [ width shrink
            , height shrink
            , Font.size 14
            , Font.color Color.primary400
            ]
            (text "APR")
        , el
            [ width fill
            , height <| px 24
            ]
            none
        ]


emptyCDP : Element Never
emptyCDP =
    column
        [ width fill
        , height shrink
        , padding 12
        , spacing 5
        , Background.color Color.transparent100
        , Border.rounded 8
        ]
        [ el
            [ width shrink
            , height shrink
            , Font.size 14
            , Font.color Color.primary400
            ]
            (text "CDP")
        , el
            [ width fill
            , height <| px 24
            ]
            none
        ]