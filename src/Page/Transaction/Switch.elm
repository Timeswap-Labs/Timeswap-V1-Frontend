module Page.Transaction.Switch exposing (empty)

import Element
    exposing
        ( Element
        , centerX
        , centerY
        , el
        , fill
        , height
        , padding
        , px
        , row
        , spacing
        , text
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Utility.Color as Color


empty : Element Never
empty =
    row
        [ width fill
        , height <| px 36
        , padding 4
        , spacing 16
        , Background.color Color.primary100
        , Border.rounded 8
        ]
        [ emptyRecommended
        , emptyCustomize
        ]


emptyRecommended : Element Never
emptyRecommended =
    el
        [ width fill
        , height fill
        , Background.color Color.primary500
        , Border.rounded 4
        ]
        (el
            [ centerX
            , centerY
            , Font.color Color.light100
            , Font.size 14
            ]
            (text "Recommended")
        )


emptyCustomize : Element Never
emptyCustomize =
    el
        [ width fill
        , height fill
        ]
        (el
            [ centerX
            , centerY
            , Font.color Color.light100
            , Font.size 14
            ]
            (text "Customize")
        )
