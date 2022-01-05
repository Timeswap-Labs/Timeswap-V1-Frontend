module Page.Transaction.Borrow.Borrow.Disabled exposing
    ( Transaction(..)
    , init
    , view
    )

import Blockchain.Main as Blockchain exposing (Blockchain)
import Blockchain.User.Main as User
import Data.Images exposing (Images)
import Data.Mode as Mode
import Data.Pair as Pair
import Data.Percent exposing (Percent)
import Data.Pool exposing (Pool)
import Data.Theme exposing (Theme)
import Data.Token exposing (Token)
import Element
    exposing
        ( Element
        , alpha
        , centerY
        , column
        , el
        , fill
        , height
        , none
        , padding
        , paddingXY
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
import Element.Region as Region
import Page.Transaction.Info as Info
import Page.Transaction.MaxButton as MaxButton
import Page.Transaction.Output as Output
import Page.Transaction.Slider as Slider
import Page.Transaction.Switch as Switch
import Page.Transaction.Textbox as Textbox
import Utility.Color as Color


type Transaction
    = Default String
    | DefaultMax String
    | Slider SliderInput
    | Debt DebtInput
    | Collateral CollateralInput
    | AdvancedMax MaxInput


type alias SliderInput =
    { assetOut : String
    , percent : Percent
    }


type alias DebtInput =
    { assetOut : String
    , percent : Percent
    , debtIn : String
    }


type alias CollateralInput =
    { assetOut : String
    , percent : Percent
    , collateralIn : String
    }


type alias MaxInput =
    { collateralIn : String
    , percent : Percent
    }


init : Transaction
init =
    Default ""


view :
    { model | images : Images, theme : Theme }
    -> Blockchain
    -> Pool
    -> Transaction
    ->
        { first : Element Never
        , second : Element Never
        }
view model blockchain pool transaction =
    { first =
        transaction
            |> assetOutSection
                model
                (pool.pair |> Pair.toAsset)
    , second =
        transaction
            |> duesInSection model blockchain pool
    }


assetOutSection :
    { model | images : Images, theme : Theme }
    -> Token
    -> Transaction
    -> Element Never
assetOutSection model asset transaction =
    column
        [ Region.description "borrow asset"
        , width <| px 343
        , height shrink
        , padding 16
        , spacing 10
        , alpha 0.2
        , Background.color Color.primary100
        , Border.rounded 8
        ]
        [ el
            [ width shrink
            , height shrink
            , Font.size 14
            , paddingXY 0 3
            , Font.color Color.primary400
            ]
            (text "Amount to Borrow")
        , (case transaction of
            Default assetOut ->
                assetOut

            Slider { assetOut } ->
                assetOut

            Debt { assetOut } ->
                assetOut

            Collateral { assetOut } ->
                assetOut

            _ ->
                ""
          )
            |> (\assetOut ->
                    Textbox.disabled model
                        { token = asset
                        , text = assetOut
                        , description = "borrow asset textbox"
                        }
               )
        ]


duesInSection :
    { model | images : Images, theme : Theme }
    -> Blockchain
    -> Pool
    -> Transaction
    -> Element Never
duesInSection model blockchain pool transaction =
    column
        [ Region.description "claims"
        , width <| px 343
        , height shrink
        , padding 16
        , spacing 12
        , alpha 0.2
        , Background.color Color.primary100
        , Border.rounded 8
        ]
        [ { mode =
                case transaction of
                    Default _ ->
                        Mode.Recommended

                    DefaultMax _ ->
                        Mode.Recommended

                    _ ->
                        Mode.Advanced
          , theme = model.theme
          }
            |> Switch.disabled
        , (case transaction of
            Default _ ->
                Nothing

            DefaultMax _ ->
                Nothing

            Slider { percent } ->
                Just percent

            Debt { percent } ->
                Just percent

            Collateral { percent } ->
                Just percent

            AdvancedMax { percent } ->
                Just percent
          )
            |> Maybe.map (Slider.disabled model.theme)
            |> Maybe.withDefault none
        , row
            [ width fill
            , height shrink
            , spacing 16
            ]
            [ Info.emptyAPR model.theme
            , Info.emptyCDP model.theme
            ]
        , case transaction of
            Default _ ->
                column
                    [ width fill
                    , height shrink
                    , spacing 12
                    ]
                    [ pool.pair
                        |> Pair.toAsset
                        |> Just
                        |> debtInSection model
                    , pool.pair
                        |> Pair.toCollateral
                        |> Just
                        |> collateralInSection model
                            blockchain
                            ""
                    ]

            DefaultMax collateralIn ->
                column
                    [ width fill
                    , height shrink
                    , spacing 12
                    ]
                    [ pool.pair
                        |> Pair.toAsset
                        |> Just
                        |> debtInSection model
                    , pool.pair
                        |> Pair.toCollateral
                        |> Just
                        |> collateralInSection model
                            blockchain
                            collateralIn
                    ]

            Slider _ ->
                column
                    [ width fill
                    , height shrink
                    , spacing 12
                    ]
                    [ Nothing
                        |> advancedDebtInSection model
                            (pool.pair |> Pair.toAsset)
                    , Nothing
                        |> advancedCollateralInSection model
                            blockchain
                            (pool.pair |> Pair.toCollateral)
                    ]

            Debt { debtIn } ->
                column
                    [ width fill
                    , height shrink
                    , spacing 12
                    ]
                    [ debtIn
                        |> Just
                        |> advancedDebtInSection model
                            (pool.pair |> Pair.toAsset)
                    , Nothing
                        |> advancedCollateralInSection model
                            blockchain
                            (pool.pair |> Pair.toCollateral)
                    ]

            Collateral { collateralIn } ->
                column
                    [ width fill
                    , height shrink
                    , spacing 12
                    ]
                    [ Nothing
                        |> advancedDebtInSection model
                            (pool.pair |> Pair.toAsset)
                    , collateralIn
                        |> Just
                        |> advancedCollateralInSection model
                            blockchain
                            (pool.pair |> Pair.toCollateral)
                    ]

            AdvancedMax { collateralIn } ->
                column
                    [ width fill
                    , height shrink
                    , spacing 12
                    ]
                    [ Nothing
                        |> advancedDebtInSection model
                            (pool.pair |> Pair.toAsset)
                    , collateralIn
                        |> Just
                        |> advancedCollateralInSection model
                            blockchain
                            (pool.pair |> Pair.toCollateral)
                    ]
        ]


debtInSection :
    { model | images : Images, theme : Theme }
    -> Maybe Token
    -> Element Never
debtInSection model asset =
    column
        [ width fill
        , height shrink
        , spacing 10
        ]
        [ el
            [ width shrink
            , height shrink
            , Font.size 14
            , Font.color Color.primary400
            ]
            (text "Debt to Repay")
        , asset
            |> Maybe.map
                (\token ->
                    Output.empty model
                        { token = token
                        , description = "debt output"
                        }
                )
            |> Maybe.withDefault
                (el
                    [ width fill
                    , height <| px 24
                    ]
                    none
                )
        ]


collateralInSection :
    { model | images : Images, theme : Theme }
    -> Blockchain
    -> String
    -> Maybe Token
    -> Element Never
collateralInSection model blockchain collateralIn maybeCollateral =
    column
        [ width fill
        , height shrink
        , spacing 10
        ]
        [ row
            [ width fill
            , height shrink
            , spacing 6
            , centerY
            ]
            [ el
                [ width shrink
                , height shrink
                , Font.size 14
                , paddingXY 0 3
                , Font.color Color.primary400
                ]
                (text "Collateral to Lock")
            , maybeCollateral
                |> Maybe.map
                    (\collateral ->
                        blockchain
                            |> Blockchain.toUser
                            |> Maybe.andThen (User.getBalance collateral)
                            |> Maybe.map
                                (\balance ->
                                    MaxButton.disabled
                                        { token = collateral
                                        , balance = balance
                                        }
                                )
                            |> Maybe.withDefault none
                    )
                |> Maybe.withDefault none
            ]
        , maybeCollateral
            |> Maybe.map
                (\token ->
                    Output.disabledCollateral model
                        { token = token
                        , input = collateralIn
                        , description = "collateral output"
                        }
                )
            |> Maybe.withDefault
                (el
                    [ width fill
                    , height <| px 24
                    ]
                    none
                )
        ]


advancedDebtInSection :
    { model | images : Images, theme : Theme }
    -> Token
    -> Maybe String
    -> Element Never
advancedDebtInSection model asset input =
    column
        [ width fill
        , height shrink
        , spacing 10
        ]
        [ el
            [ width shrink
            , height shrink
            , Font.size 14
            , Font.color Color.primary400
            ]
            (text "Debt to Repay")
        , Textbox.disabled model
            { token = asset
            , text = input |> Maybe.withDefault ""
            , description = "debt out input"
            }
        ]


advancedCollateralInSection :
    { model | images : Images, theme : Theme }
    -> Blockchain
    -> Token
    -> Maybe String
    -> Element Never
advancedCollateralInSection model blockchain collateral input =
    column
        [ width fill
        , height shrink
        , spacing 10
        ]
        [ row
            [ width fill
            , height shrink
            , spacing 6
            , centerY
            ]
            [ el
                [ width shrink
                , height shrink
                , Font.size 14
                , paddingXY 0 3
                , Font.color Color.primary400
                ]
                (text "Collateral to Lock")
            , blockchain
                |> Blockchain.toUser
                |> Maybe.andThen (User.getBalance collateral)
                |> Maybe.map
                    (\balance ->
                        MaxButton.disabled
                            { token = collateral
                            , balance = balance
                            }
                    )
                |> Maybe.withDefault none
            ]
        , Textbox.disabled model
            { token = collateral
            , text = input |> Maybe.withDefault ""
            , description = "collateral out input"
            }
        ]
