module Page.Transaction.Lend.Empty exposing (view)

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
import Page.Transaction.Switch as Switch
import Page.Transaction.Textbox as Textbox
import Utility.Color as Color


view :
    { model | images : Images }
    ->
        { asset : Maybe Token
        , collateral : Maybe Token
        }
    ->
        { first : Element Never
        , second : Element Never
        }
view model { asset, collateral } =
    { first = assetInSection model asset
    , second = claimsOutSection model asset collateral
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


claimsOutSection :
    { model | images : Images }
    -> Maybe Token
    -> Maybe Token
    -> Element Never
claimsOutSection model asset collateral =
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
        [ Switch.empty
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
            [ bondOutSection model asset
            , insuranceOutSection model collateral
            ]
        ]


bondOutSection :
    { model | images : Images }
    -> Maybe Token
    -> Element Never
bondOutSection model asset =
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
            (text "Amount to Receive")
        , asset
            |> Maybe.map
                (\token ->
                    Output.disabled model
                        { token = token
                        , description = "bond output"
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


insuranceOutSection :
    { model | images : Images }
    -> Maybe Token
    -> Element Never
insuranceOutSection model collateral =
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
            (text "Amount Protecting")
        , collateral
            |> Maybe.map
                (\token ->
                    Output.disabled model
                        { token = token
                        , description = "insurance output"
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
