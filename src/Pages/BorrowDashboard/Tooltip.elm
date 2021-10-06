module Pages.BorrowDashboard.Tooltip exposing
    ( Tooltip(..)
    , amount
    )

import Data.Device as Device exposing (Device)
import Data.Pool exposing (Pool)
import Data.TokenId exposing (TokenId)
import Element
    exposing
        ( Element
        , alignLeft
        , alignTop
        , centerX
        , centerY
        , column
        , el
        , fill
        , height
        , moveUp
        , padding
        , paddingEach
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
    = Debt Pool TokenId
    | Collateral Pool TokenId


amount : Device -> String -> Element msg
amount device string =
    if device |> Device.isPhoneOrTablet then
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
                , alignLeft
                , Background.color Color.dark500
                , Border.rounded 4
                , Tooltip.shadow
                , Font.size 12
                , Font.color Color.transparent300
                ]
                (text string)
            ]

    else
        row
            [ width shrink
            , height shrink
            , paddingEach
                { top = 0
                , right = 0
                , bottom = 0
                , left = 4
                }
            ]
            [ el
                [ alignTop
                , height <| px 24
                ]
                (el [ centerY ] Tooltip.triangleLeft)
            , el
                [ width shrink
                , height shrink
                , padding 12
                , alignTop
                , moveUp 6
                , Background.color Color.dark500
                , Border.rounded 4
                , Tooltip.shadow
                , Font.size 12
                , Font.color Color.transparent300
                ]
                (text string)
            ]
