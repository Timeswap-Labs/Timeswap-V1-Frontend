module Modals.Withdraw.ClaimsIn exposing (view)

import Data.Pair as Pair
import Data.Pool exposing (Pool)
import Data.Positions as Positions exposing (Positions)
import Data.Token as Token
import Data.TokenImages exposing (TokenImages)
import Element
    exposing
        ( Element
        , alignLeft
        , alignRight
        , alpha
        , behindContent
        , centerY
        , column
        , el
        , fill
        , height
        , none
        , paddingXY
        , px
        , row
        , shrink
        , spacing
        , text
        , width
        )
import Element.Font as Font
import Utility.Color as Color
import Utility.Glass as Glass
import Utility.Loading as Loading
import Utility.TokenImage as TokenImage


view :
    { model | tokenImages : TokenImages }
    -> Positions
    -> Pool
    -> Element msg
view model positions pool =
    column
        [ width fill
        , height shrink
        , spacing 12
        ]
        [ assetOutSection model positions pool
        , collateralOutSection model positions pool
        ]


assetOutSection :
    { model | tokenImages : TokenImages }
    -> Positions
    -> Pool
    -> Element msg
assetOutSection { tokenImages } positions pool =
    row
        ([ width fill
         , height <| px 77
         , paddingXY 20 0
         , spacing 14
         ]
            ++ Glass.lightWhiteModal 12
        )
        [ el
            [ width shrink
            , height shrink
            , alignLeft
            , centerY
            , Font.regular
            , Font.size 14
            , Font.color Color.light100
            ]
            (text "Asset to receive")
        , pool.pair
            |> Pair.toAsset
            |> TokenImage.icon tokenImages
                [ width <| px 32
                , alignRight
                , centerY
                ]
        , assetOutAmount positions pool
        ]


assetOutAmount : Positions -> Pool -> Element msg
assetOutAmount positions pool =
    positions
        |> Positions.getClaimReturn pool
        |> Maybe.map
            (\{ asset } ->
                row
                    [ width shrink
                    , alignRight
                    , centerY
                    , Font.bold
                    , Font.size 16
                    ]
                    [ el
                        [ width shrink
                        , Font.bold
                        , Font.color Color.transparent500
                        ]
                        (text asset)
                    , el [ Font.bold ] <| text " "
                    , el
                        [ alignRight
                        , Font.bold
                        , Font.color Color.transparent300
                        ]
                        (pool.pair
                            |> Pair.toAsset
                            |> Token.toSymbol
                            |> text
                        )
                    ]
            )
        |> Maybe.withDefault
            (el
                [ width <| px 50
                , alignRight
                , centerY
                ]
                Loading.view
            )


collateralOutSection :
    { model | tokenImages : TokenImages }
    -> Positions
    -> Pool
    -> Element msg
collateralOutSection { tokenImages } positions pool =
    row
        ([ width fill
         , height <| px 77
         , paddingXY 20 0
         , spacing 14
         ]
            ++ Glass.lightWhiteModal 12
        )
        [ el
            [ width shrink
            , height shrink
            , alignLeft
            , centerY
            , Font.regular
            , Font.size 14
            , Font.color Color.light100
            ]
            (text "Collateral to receive")
        , pool.pair
            |> Pair.toCollateral
            |> TokenImage.icon tokenImages
                [ width <| px 32
                , alignRight
                , centerY
                ]
        , collateralOutAmount positions pool
        ]


collateralOutAmount : Positions -> Pool -> Element msg
collateralOutAmount positions pool =
    positions
        |> Positions.getClaimReturn pool
        |> Maybe.map
            (\{ collateral } ->
                row
                    [ width shrink
                    , alignRight
                    , centerY
                    , Font.bold
                    , Font.size 16
                    ]
                    [ el
                        [ width shrink
                        , Font.bold
                        , Font.color Color.transparent500
                        ]
                        (text collateral)
                    , el [ Font.bold ] <| text " "
                    , el
                        [ alignRight
                        , Font.bold
                        , Font.color Color.transparent300
                        ]
                        (pool.pair
                            |> Pair.toCollateral
                            |> Token.toSymbol
                            |> text
                        )
                    ]
            )
        |> Maybe.withDefault
            (el
                [ width <| px 50
                , alignRight
                , centerY
                ]
                Loading.view
            )
