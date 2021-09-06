module Modals.Borrow.Tooltip exposing
    ( Tooltip(..)
    , collateral
    , collateralBalance
    , debt
    , dues
    )

import Element
    exposing
        ( Element
        , alignRight
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
    = CollateralBalance
    | Dues
    | Debt
    | Collateral
    | TransactionInfo


collateralBalance : String -> Element msg
collateralBalance string =
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


dues : Element msg
dues =
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
            [ """You will receive a collateralized debt NFT. There is a 
                default setting where the interest rate and collateral 
                factor is chosen for you. Switch to advance setting to 
                customize your CDP.
                """
                |> text
            ]
        ]


debt : Element msg
debt =
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
            [ """This is the debt you pay before maturity to unlock your collateral. 
                You can choose not to pay and forfeit the collateral if you 
                believe the value of the collateral is less than the value 
                of the debt near maturity.
                """
                |> text
            ]
        ]


collateral : Element msg
collateral =
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
            [ """This is the amount of collateral locked. You should pay back 
                the debt to unlock this collateral if you believe the value of 
                the collateral is greater than the value of the debt near maturity.
                """
                |> text
            ]
        ]
