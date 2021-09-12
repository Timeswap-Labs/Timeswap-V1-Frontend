module Services.Settings.Tooltip exposing
    ( Tooltip(..)
    , deadline
    , slippage
    )

import Element
    exposing
        ( Element
        , alignTop
        , centerY
        , el
        , height
        , moveUp
        , padding
        , paddingEach
        , paragraph
        , px
        , row
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
    = Slippage
    | Deadline


slippage : Element msg
slippage =
    row
        [ width shrink
        , alignTop
        , paddingEach
            { top = 0
            , right = 0
            , bottom = 0
            , left = 4
            }
        ]
        [ el
            [ alignTop
            , height <| px 16
            ]
            (el [ centerY ] Tooltip.triangleLeft)
        , paragraph
            [ width <| px 240
            , height shrink
            , padding 12
            , alignTop
            , moveUp 12
            , Background.color Color.dark500
            , Border.rounded 4
            , Tooltip.shadow
            , Font.size 12
            , Font.color Color.transparent300
            ]
            [ """Your transaction will revert if the price moves unfavorably
                by more than your slippage tolerance percent.
                """
                |> text
            ]
        ]


deadline : Element msg
deadline =
    row
        [ width shrink
        , alignTop
        , paddingEach
            { top = 0
            , right = 0
            , bottom = 0
            , left = 4
            }
        ]
        [ el
            [ alignTop
            , height <| px 16
            ]
            (el [ centerY ] Tooltip.triangleLeft)
        , paragraph
            [ width <| px 240
            , height shrink
            , padding 12
            , alignTop
            , moveUp 12
            , Background.color Color.dark500
            , Border.rounded 4
            , Tooltip.shadow
            , Font.size 12
            , Font.color Color.transparent300
            ]
            [ """Your transaction will revert if it is pending by
                more than your transaction deadline duration.
                """
                |> text
            ]
        ]
