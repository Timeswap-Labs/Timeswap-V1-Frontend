module Page.Transaction.Liquidity.Empty exposing (view)

import Data.Images exposing (Images)
import Data.Token exposing (Token)
import Element
    exposing
        ( Element
        , alpha
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
import Page.Transaction.Output as Output
import Page.Transaction.Textbox as Textbox
import Utility.Color as Color
import Utility.Maybe as Maybe


view :
    { model | images : Images }
    ->
        { asset : Maybe Token
        , collateral : Maybe Token
        }
    ->
        { first : Element Never
        , second : Element Never
        , third : Element Never
        }
view model { asset, collateral } =
    { first = assetInSection model asset
    , second = duesOutSection model asset collateral
    , third = liquidityOutSection model asset collateral
    }


assetInSection :
    { model | images : Images }
    -> Maybe Token
    -> Element Never
assetInSection model token =
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
        [ el
            [ width shrink
            , height shrink
            , Font.size 14
            , paddingXY 0 3
            , Font.color Color.primary400
            ]
            (text "Amount to Lend")
        , token
            |> Maybe.map
                (\asset ->
                    Textbox.disabled model
                        { token = asset
                        , text = ""
                        , description = "asset in textbox"
                        }
                )
            |> Maybe.withDefault
                (Textbox.empty "asset in textbox")
        ]


duesOutSection :
    { model | images : Images }
    -> Maybe Token
    -> Maybe Token
    -> Element Never
duesOutSection model asset collateral =
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
            [ debtOutSection model asset
            , collateralOutSection model collateral
            ]
        ]


debtOutSection :
    { model | images : Images }
    -> Maybe Token
    -> Element Never
debtOutSection model asset =
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
        , asset
            |> Maybe.map
                (\token ->
                    Textbox.disabled model
                        { token = token
                        , text = ""
                        , description = "debt output"
                        }
                )
            |> Maybe.withDefault
                (Textbox.empty "debt output")
        ]


collateralOutSection :
    { model | images : Images }
    -> Maybe Token
    -> Element Never
collateralOutSection model collateral =
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
            (text "Collateral to Lock")
        , collateral
            |> Maybe.map
                (\token ->
                    Textbox.disabled model
                        { token = token
                        , text = ""
                        , description = "collateral output"
                        }
                )
            |> Maybe.withDefault
                (Textbox.empty "collateral output")
        ]


liquidityOutSection :
    { model | images : Images }
    -> Maybe Token
    -> Maybe Token
    -> Element Never
liquidityOutSection model maybeAsset maybeCollateral =
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
        , Just
            (\asset collateral ->
                Output.disabledLiquidity model
                    { asset = asset
                    , collateral = collateral
                    , description = "liquidity out"
                    }
            )
            |> Maybe.apply maybeAsset
            |> Maybe.apply maybeCollateral
            |> Maybe.withDefault
                (el
                    [ width fill
                    , height <| px 24
                    ]
                    none
                )
        ]
