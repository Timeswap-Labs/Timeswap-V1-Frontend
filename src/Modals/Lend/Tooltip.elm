module Modals.Lend.Tooltip exposing
    ( Tooltip(..)
    , amount
    , assetBalance
    , bond
    , claims
    , insurance
    , transactionInfo
    )

import Data.Device as Device exposing (Device)
import Data.Pair as Pair exposing (Pair)
import Data.Remote exposing (Remote(..))
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
        , moveLeft
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
    = AssetBalance
    | CollateralFactor
    | Claims
    | Bond
    | Insurance
    | TransactionInfo


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


claims : Device -> Element msg
claims device =
    """You will receive bond ERC20 tokens and insurance ERC20 tokens. 
        There is a default setting where the interest rate and collateral 
        factor is chosen for you. Switch to advance setting to 
        customize your risk parameter. 
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


bond : Device -> Element msg
bond device =
    """This is the amount of asset you should receive after maturity. 
        At near maturity, if the price of collateral crash, and enough 
        borrowers default, you may not receive the full amount.
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


insurance : Device -> Element msg
insurance device =
    """For the percentage of promised asset you don't receive after maturity, 
        you will receive up to the same percentage of your insurance value, 
        which comes from the collateral forfeited by borrowers.
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
                            , moveLeft 48
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
transactionInfo device pair ( minBond, minInsurance ) slippage =
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
                        (text "Minimum bond receive")
                    , row
                        [ alignLeft
                        , spacing 4
                        ]
                        [ row
                            [ width shrink
                            , Font.regular
                            ]
                            (minBond
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
                        (text "Minimum insurance receive")
                    , row
                        [ alignLeft
                        , spacing 4
                        ]
                        [ row
                            [ width shrink
                            , Font.regular
                            ]
                            (minInsurance
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
                        (text "Minimum bond receive")
                    , row
                        [ alignRight
                        , spacing 4
                        ]
                        [ row
                            [ width shrink
                            , Font.regular
                            ]
                            (minBond
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
                        (text "Minimum insurance receive")
                    , row
                        [ alignRight
                        , spacing 4
                        ]
                        [ row
                            [ width shrink
                            , Font.regular
                            ]
                            (minInsurance
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
