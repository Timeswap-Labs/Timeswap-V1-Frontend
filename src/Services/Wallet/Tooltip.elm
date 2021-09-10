module Services.Wallet.Tooltip exposing
    ( Tooltip(..)
    , balance
    )

import Data.Token exposing (Token)
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
    = Balance Token


balance : String -> Element msg
balance string =
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
            , Font.color Color.transparent300
            ]
            (text string)
        ]
