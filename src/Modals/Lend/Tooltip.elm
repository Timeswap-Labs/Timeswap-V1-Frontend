module Modals.Lend.Tooltip exposing
    ( Tooltip(..)
    , assetBalance
    )

import Element
    exposing
        ( Element
        , alignRight
        , centerX
        , column
        , el
        , fill
        , height
        , padding
        , paddingEach
        , shrink
        , text
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Utility.Color as Color
import Utility.Tooltip as Tooltip


type Tooltip
    = AssetBalance


assetBalance : String -> Element msg
assetBalance string =
    column
        [ width fill
        , height shrink
        , paddingEach
            { top = 4
            , right = 0
            , bottom = 0
            , left = 0
            }
        ]
        [ el
            [ centerX ]
            Tooltip.triangleUp
        , el
            [ width shrink
            , height shrink
            , padding 12
            , alignRight
            , Background.color Color.dark500
            , Border.rounded 4
            , Tooltip.shadow
            , Font.size 12
            ]
            (text string)
        ]
