module Modals.Lend.Tooltip exposing
    ( Tooltip(..)
    , amount
    , assetBalance
    , bond
    , claims
    , insurance
    , transactionInfo
    )

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


claims : Element msg
claims =
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
            [ """You will receive bond ERC20 tokens and insurance ERC20 tokens. 
                There is a default setting where the interest rate and collateral 
                factor is chosen for you. Switch to advance setting to 
                customize your risk parameter. 
                """
                |> text
            ]
        ]


bond : Element msg
bond =
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
            [ """This is the amount of asset you should receive after maturity. 
                At near maturity, if the price of collateral crash, and enough 
                borrowers default, you may not receive the full amount.
              """
                |> text
            ]
        ]


insurance : Element msg
insurance =
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
            [ """For the percentage of promised asset you don't receive after maturity, 
                you will receive up to the same percentage of your insurance value, 
                which comes from the collateral forfeited by borrowers.
              """
                |> text
            ]
        ]


transactionInfo : Pair -> ( String, String ) -> Slippage -> Element msg
transactionInfo pair ( minBond, minInsurance ) slippage =
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
                    [ el
                        [ Font.regular
                        , Font.color Color.transparent500
                        ]
                        (text minBond)
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
                    [ el
                        [ Font.regular
                        , Font.color Color.transparent500
                        ]
                        (text minInsurance)
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
