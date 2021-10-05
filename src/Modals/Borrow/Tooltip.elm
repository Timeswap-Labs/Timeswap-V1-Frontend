module Modals.Borrow.Tooltip exposing
    ( Tooltip(..)
    , amount
    , collateral
    , collateralBalance
    , debt
    , dues
    , transactionInfo
    )

import Data.Device as Device exposing (Device)
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
        , rotate
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
                , alignRight
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


dues : Device -> Element msg
dues device =
    """You will receive a collateralized debt NFT. There is a 
        default setting where the interest rate and collateral 
        factor is chosen for you. Switch to advance setting to 
        customize your CDP.
    """
        |> text
        |> (\element ->
                if device |> Device.isPhone then
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
                        , paragraph
                            [ width <| px 240
                            , height shrink
                            , padding 12
                            , centerX
                            , Background.color Color.dark500
                            , Border.rounded 4
                            , Tooltip.shadow
                            , Font.size 12
                            , Font.color Color.transparent300
                            ]
                            [ element ]
                        ]

                else
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
                            [ element ]
                        ]
           )


debt : Device -> Element msg
debt device =
    """This is the debt you pay before maturity to unlock your collateral. 
        You can choose not to pay and forfeit the collateral if you 
        believe the value of the collateral is less than the value 
        of the debt near maturity.
    """
        |> text
        |> (\element ->
                if device |> Device.isPhone then
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
                        , paragraph
                            [ width <| px 240
                            , height shrink
                            , padding 12
                            , centerX
                            , Background.color Color.dark500
                            , Border.rounded 4
                            , Tooltip.shadow
                            , Font.size 12
                            , Font.color Color.transparent300
                            ]
                            [ element ]
                        ]

                else
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
                            [ element ]
                        ]
           )


collateral : Device -> Element msg
collateral device =
    """This is the amount of collateral locked. You should pay back 
        the debt to unlock this collateral if you believe the value of 
        the collateral is greater than the value of the debt near maturity.
    """
        |> text
        |> (\element ->
                if device |> Device.isPhone then
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
                        , paragraph
                            [ width <| px 240
                            , height shrink
                            , padding 12
                            , centerX
                            , Background.color Color.dark500
                            , Border.rounded 4
                            , Tooltip.shadow
                            , Font.size 12
                            , Font.color Color.transparent300
                            ]
                            [ element ]
                        ]

                else
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
                            [ element ]
                        ]
           )


transactionInfo : Device -> Pair -> ( String, String ) -> Slippage -> Element msg
transactionInfo device pair ( maxDebt, maxCollateral ) slippage =
    if device |> Device.isPhone then
        column
            [ width <| px 340
            , height shrink
            , centerX
            , paddingEach
                { top = 4
                , right = 0
                , bottom = 0
                , left = 0
                }
            ]
            [ column
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
                [ column
                    [ alignLeft
                    , spacing 4
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
                        [ alignLeft
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
                , column
                    [ alignLeft
                    , spacing 4
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
                        [ alignLeft
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
                , column
                    [ alignLeft
                    , spacing 4
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
                        [ alignLeft
                        , Font.regular
                        , Font.color Color.transparent500
                        ]
                        (slippage
                            |> Slippage.toPercent
                            |> text
                        )
                    ]
                ]
            , el
                [ centerX
                , rotate pi
                ]
                Tooltip.triangleUp
            ]

    else
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
