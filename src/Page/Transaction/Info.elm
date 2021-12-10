module Page.Transaction.Info exposing (emptyAPR, emptyCDP)

import Element
    exposing
        ( Element
        , column
        , el
        , fill
        , height
        , none
        , px
        , shrink
        , spacing
        , text
        , width
        )
import Element.Font as Font
import Utility.Color as Color


emptyAPR : Element Never
emptyAPR =
    column
        [ width fill
        , height shrink
        , spacing 5
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
        , spacing 5
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
