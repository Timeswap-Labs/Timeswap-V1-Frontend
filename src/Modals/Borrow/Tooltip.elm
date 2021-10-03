module Modals.Borrow.Tooltip exposing
    ( Tooltip(..)
    , amount
    , collateral
    , collateralBalance
    , debt
    , dues
    , transactionInfo
    )

import Data.Pair as Pair exposing (Pair)
import Data.Slippage as Slippage exposing (Slippage)
import Data.Token as Token
import Element
    exposing
        ( Element
        , alignLeft
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
        , paddingXY
        , paragraph
        , px
        , row
        , shrink
        , spacing
        , text
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Utility.Color as Color
import Utility.Tooltip as Tooltip
import Utility.Truncate as Truncate


type Tooltip
    = CollateralBalance
    | CollateralFactor
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


amount : String -> Element msg
amount string =
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


transactionInfo : Pair -> ( String, String ) -> Slippage -> Element msg
transactionInfo pair ( maxDebt, maxCollateral ) slippage =
    column
        [ width <| px 480
        , height shrink
        , centerX
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
        , column
            [ width fill
            , height shrink
            , padding 12
            , spacing 12
            , alignRight
            , Background.color Color.dark500
            , Border.rounded 4
            , Tooltip.shadow
            , Font.size 14
            ]
            [ row
                [ width fill
                , height shrink
                , paddingXY 0 3
                , spacing 72
                ]
                [ el
                    [ width shrink
                    , height shrink
                    , alignLeft
                    , Font.regular
                    , Font.color Color.transparent300
                    ]
                    (text "Maximum debt receive")
                , row
                    [ alignRight
                    , spacing 4
                    ]
                    [ row
                        [ width shrink
                        , Font.regular
                        ]
                        (maxDebt
                            |> Truncate.fade
                            |> (\( nonFaded, faded ) ->
                                    [ el
                                        [ Font.color Color.transparent500 ]
                                        (text nonFaded)
                                    , el
                                        [ Font.color Color.transparent200 ]
                                        (text faded)
                                    ]
                               )
                        )
                    , el
                        [ Font.regular
                        , Font.color Color.transparent300
                        ]
                        (pair
                            |> Pair.toAsset
                            |> Token.toSymbol
                            |> text
                        )
                    ]
                ]
            , row
                [ width fill
                , height shrink
                , paddingXY 0 3
                , spacing 72
                ]
                [ el
                    [ width shrink
                    , height shrink
                    , spacing 24
                    , alignLeft
                    , Font.regular
                    , Font.color Color.transparent300
                    ]
                    (text "Maximum collateral lock")
                , row
                    [ alignRight
                    , spacing 4
                    ]
                    [ row
                        [ width shrink
                        , Font.regular
                        ]
                        (maxCollateral
                            |> Truncate.fade
                            |> (\( nonFaded, faded ) ->
                                    [ el
                                        [ Font.color Color.transparent500 ]
                                        (text nonFaded)
                                    , el
                                        [ Font.color Color.transparent200 ]
                                        (text faded)
                                    ]
                               )
                        )
                    , el
                        [ Font.regular
                        , Font.color Color.transparent300
                        ]
                        (pair
                            |> Pair.toCollateral
                            |> Token.toSymbol
                            |> text
                        )
                    ]
                ]
            , row
                [ width fill
                , height shrink
                , paddingXY 0 3
                , spacing 72
                ]
                [ el
                    [ width shrink
                    , height shrink
                    , alignLeft
                    , Font.regular
                    , Font.color Color.transparent300
                    ]
                    (text "Slippage tolerance")
                , el
                    [ alignRight
                    , Font.regular
                    , Font.color Color.transparent500
                    ]
                    (slippage
                        |> Slippage.toPercent
                        |> text
                    )
                ]
            ]
        ]
