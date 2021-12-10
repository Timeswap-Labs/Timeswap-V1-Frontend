module Page.Transaction.Liquidity.Add.Disabled exposing (Transaction(..), init, view)

import Blockchain.Main as Blockchain exposing (Blockchain)
import Blockchain.User.Main as User
import Data.Images exposing (Images)
import Data.Pair as Pair
import Data.Pool exposing (Pool)
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
import Page.Transaction.Textbox as Textbox
import Utility.Color as Color


type Transaction
    = Asset String
    | Debt String
    | Collateral String


init : Transaction
init =
    Asset ""


view :
    { model | images : Images }
    -> Blockchain
    -> Pool
    -> Transaction
    ->
        { first : Element Never
        , second : Element Never
        , third : Element Never
        }
view model blockchain pool transaction =
    { first =
        assetInSection model
            blockchain
            (pool.pair |> Pair.toAsset)
            transaction
    , second = duesOutSection model blockchain pool transaction
    , third = liquidityOutSection model pool
    }


assetInSection :
    { model | images : Images }
    -> Blockchain
    -> Token
    -> Transaction
    -> Element Never
assetInSection model blockchain asset transaction =
    column
        [ Region.description "lend asset"
        , width <| px 343
        , height shrink
        , padding 16
        , spacing 10
        , alpha 0.2
        , Background.color Color.primary100
        , Border.rounded 8
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
                (text "Amount to Lend")
            , blockchain
                |> Blockchain.toUser
                |> Maybe.andThen (User.getBalance asset)
                |> Maybe.map
                    (\balance ->
                        MaxButton.disabled
                            { token = asset
                            , balance = balance
                            }
                    )
                |> Maybe.withDefault none
            ]
        , (case transaction of
            Asset assetIn ->
                assetIn

            _ ->
                ""
          )
            |> (\assetIn ->
                    Textbox.disabled model
                        { token = asset
                        , text = assetIn
                        , description = "asset in textbox"
                        }
               )
        ]


duesOutSection :
    { model | images : Images }
    -> Blockchain
    -> Pool
    -> Transaction
    -> Element Never
duesOutSection model blockchain pool transaction =
    column
        [ Region.description "dues"
        , width <| px 343
        , height shrink
        , padding 16
        , spacing 12
        , alpha 0.2
        , Background.color Color.primary100
        , Border.rounded 8
        ]
        [ row
            [ width fill
            , height shrink
            , spacing 16
            ]
            [ Info.emptyAPR
            , Info.emptyCDP
            ]
        , column
            [ width fill
            , height shrink
            , spacing 12
            ]
            [ debtOutSection model
                (pool.pair |> Pair.toAsset)
                transaction
            , collateralOutSection model
                blockchain
                (pool.pair |> Pair.toCollateral)
                transaction
            ]
        ]


debtOutSection :
    { model | images : Images }
    -> Token
    -> Transaction
    -> Element Never
debtOutSection model asset transaction =
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
        , (case transaction of
            Debt debtOut ->
                debtOut

            _ ->
                ""
          )
            |> (\debtOut ->
                    Textbox.disabled model
                        { token = asset
                        , text = debtOut
                        , description = "debt output"
                        }
               )
        ]


collateralOutSection :
    { model | images : Images }
    -> Blockchain
    -> Token
    -> Transaction
    -> Element Never
collateralOutSection model blockchain collateral transaction =
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
        , (case transaction of
            Collateral collateralOut ->
                collateralOut

            _ ->
                ""
          )
            |> (\collateralOut ->
                    Textbox.disabled model
                        { token = collateral
                        , text = collateralOut
                        , description = "collateral output"
                        }
               )
        ]


liquidityOutSection :
    { model | images : Images }
    -> Pool
    -> Element Never
liquidityOutSection model pool =
    column
        [ Region.description "liquidity output"
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
            (text "LP Tokens")
        , Output.disabledLiquidity model
            { asset = pool.pair |> Pair.toAsset
            , collateral = pool.pair |> Pair.toCollateral
            , description = "liquidity out"
            }
        ]
