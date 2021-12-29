module Page.Transaction.Borrow.Empty exposing (view)

import Data.Images exposing (Images)
import Data.Theme exposing (Theme)
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
import Page.Transaction.Switch as Switch
import Page.Transaction.Textbox as Textbox
import Utility.Color as Color


view :
    { model | images : Images, theme : Theme }
    ->
        { asset : Maybe Token
        , collateral : Maybe Token
        }
    ->
        { first : Element Never
        , second : Element Never
        }
view model { asset, collateral } =
    { first = assetOutSection model asset
    , second = duesOutSection model asset collateral
    }


assetOutSection :
    { model | images : Images, theme : Theme }
    -> Maybe Token
    -> Element Never
assetOutSection model token =
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
        , token
            |> Maybe.map
                (\asset ->
                    Textbox.disabled model
                        { token = asset
                        , text = ""
                        , description = "asset out textbox"
                        }
                )
            |> Maybe.withDefault
                (Textbox.empty "asset out textbox")
        ]


duesOutSection :
    { model | images : Images, theme : Theme }
    -> Maybe Token
    -> Maybe Token
    -> Element Never
duesOutSection model asset collateral =
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
        [ Switch.empty model.theme
        , row
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
            , spacing 16
            ]
            [ debtOutSection model asset
            , collateralOutSection model collateral
            ]
        ]


debtOutSection :
    { model | images : Images, theme : Theme }
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


collateralOutSection :
    { model | images : Images, theme : Theme }
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
            , Font.color Color.primary400
            ]
            (text "Collateral to Lock")
        , collateral
            |> Maybe.map
                (\token ->
                    Output.empty model
                        { token = token
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
