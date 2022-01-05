module Page.Transaction.Liquidity.New.Disabled exposing
    ( Transaction
    , init
    , view
    )

import Blockchain.Main as Blockchain exposing (Blockchain)
import Blockchain.User.Main as User
import Data.Images exposing (Images)
import Data.Pair as Pair
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
import Page.Transaction.Textbox as Textbox
import Utility.Color as Color


type alias Transaction =
    { assetIn : String
    , debtIn : String
    , collateralIn : String
    }


init : Transaction
init =
    { assetIn = ""
    , debtIn = ""
    , collateralIn = ""
    }


view :
    { model | images : Images, theme : Theme }
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
    , second = duesInSection model blockchain pool transaction
    , third = liqOutSection model pool
    }


assetInSection :
    { model | images : Images, theme : Theme }
    -> Blockchain
    -> Token
    -> Transaction
    -> Element Never
assetInSection model blockchain asset { assetIn } =
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
        , Textbox.disabled model
            { token = asset
            , text = assetIn
            , description = "asset in textbox"
            }
        ]


duesInSection :
    { model | images : Images, theme : Theme }
    -> Blockchain
    -> Pool
    -> Transaction
    -> Element Never
duesInSection model blockchain pool transaction =
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
            [ debtInSection model
                (pool.pair |> Pair.toAsset)
                transaction
            , collateralInSection model
                blockchain
                (pool.pair |> Pair.toCollateral)
                transaction
            ]
        ]


debtInSection :
    { model | images : Images, theme : Theme }
    -> Token
    -> Transaction
    -> Element Never
debtInSection model asset { debtIn } =
    column
        [ width fill
        , height shrink
        , spacing 10
        ]
        [ el
            [ width shrink
            , height shrink
            , Font.size 14
            , paddingXY 0 3
            , Font.color Color.primary400
            ]
            (text "Debt to Repay")
        , Textbox.disabled model
            { token = asset
            , text = debtIn
            , description = "debt output"
            }
        ]


collateralInSection :
    { model | images : Images, theme : Theme }
    -> Blockchain
    -> Token
    -> Transaction
    -> Element Never
collateralInSection model blockchain collateral { collateralIn } =
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
            , text = collateralIn
            , description = "collateral output"
            }
        ]


liqOutSection :
    { model | images : Images }
    -> Pool
    -> Element Never
liqOutSection model pool =
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
            (text "LP Tokens to Receive")
        , Output.disabledLiquidity model
            { pair = pool.pair
            , description = "liquidity out"
            }
        ]
