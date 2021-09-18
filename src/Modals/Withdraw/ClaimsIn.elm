module Modals.Withdraw.ClaimsIn exposing (view)

import Data.Pair as Pair exposing (Pair)
import Data.Pool exposing (Pool)
import Data.Positions as Positions exposing (Positions)
import Data.Token as Token
import Data.TokenImages exposing (TokenImages)
import Element
    exposing
        ( Element
        , alignLeft
        , alignRight
        , below
        , centerY
        , column
        , el
        , fill
        , height
        , none
        , paddingEach
        , paddingXY
        , px
        , row
        , shrink
        , spacing
        , text
        , width
        )
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Modals.Withdraw.Tooltip as Tooltip exposing (Tooltip)
import Utility.Color as Color
import Utility.Glass as Glass
import Utility.Loading as Loading
import Utility.TokenImage as TokenImage
import Utility.Truncate as Truncate


view :
    { msgs
        | onMouseEnter : Tooltip -> msg
        , onMouseLeave : msg
    }
    -> { model | tokenImages : TokenImages }
    -> Positions
    -> { modal | pool : Pool, tooltip : Maybe Tooltip }
    -> Element msg
view msgs model positions modal =
    column
        [ width fill
        , height shrink
        , spacing 12
        ]
        [ assetOutSection msgs model positions modal
        , collateralOutSection msgs model positions modal
        ]


assetOutSection :
    { msgs
        | onMouseEnter : Tooltip -> msg
        , onMouseLeave : msg
    }
    -> { model | tokenImages : TokenImages }
    -> Positions
    -> { modal | pool : Pool, tooltip : Maybe Tooltip }
    -> Element msg
assetOutSection msgs { tokenImages } positions ({ pool } as modal) =
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
        , assetOutAmount msgs positions modal
        ]


assetOutAmount :
    { msgs
        | onMouseEnter : Tooltip -> msg
        , onMouseLeave : msg
    }
    -> Positions
    -> { modal | pool : Pool, tooltip : Maybe Tooltip }
    -> Element msg
assetOutAmount msgs positions ({ pool } as modal) =
    positions
        |> Positions.getClaimReturn pool
        |> Maybe.map .asset
        |> Maybe.map (assetAmount msgs modal)
        |> Maybe.withDefault
            (el
                [ width <| px 50
                , alignRight
                , centerY
                ]
                Loading.view
            )


assetAmount :
    { msgs
        | onMouseEnter : Tooltip -> msg
        , onMouseLeave : msg
    }
    -> { modal | pool : { pool | pair : Pair }, tooltip : Maybe Tooltip }
    -> String
    -> Element msg
assetAmount msgs { pool, tooltip } assetIn =
    assetIn
        |> Truncate.amount
        |> (\{ full, truncated } ->
                truncated
                    |> Maybe.map
                        (\short ->
                            pool.pair
                                |> Pair.toAsset
                                |> Token.toSymbol
                                |> (\symbol ->
                                        row
                                            [ alignRight
                                            , centerY
                                            , paddingEach
                                                { top = 3
                                                , right = 0
                                                , bottom = 2
                                                , left = 0
                                                }
                                            , spacing 6
                                            , Font.bold
                                            , Font.size 16
                                            , Border.widthEach
                                                { top = 0
                                                , right = 0
                                                , bottom = 1
                                                , left = 0
                                                }
                                            , Border.dashed
                                            , Border.color Color.transparent200
                                            , Events.onMouseEnter (msgs.onMouseEnter Tooltip.AssetOut)
                                            , Events.onMouseLeave msgs.onMouseLeave
                                            , (case tooltip of
                                                Just Tooltip.AssetOut ->
                                                    [ full
                                                    , symbol
                                                    ]
                                                        |> String.join " "
                                                        |> Tooltip.amount

                                                _ ->
                                                    none
                                              )
                                                |> below
                                            ]
                                            [ el [ Font.color Color.transparent500 ] (text short)
                                            , el [ Font.color Color.transparent300 ] (text symbol)
                                            ]
                                   )
                        )
                    |> Maybe.withDefault
                        (row
                            [ alignRight
                            , centerY
                            , paddingXY 0 3
                            , spacing 6
                            , Font.bold
                            , Font.size 16
                            ]
                            [ el [ Font.color Color.transparent500 ] (text full)
                            , el
                                [ Font.color Color.transparent300 ]
                                (pool.pair
                                    |> Pair.toAsset
                                    |> Token.toSymbol
                                    |> text
                                )
                            ]
                        )
           )


collateralOutSection :
    { msgs
        | onMouseEnter : Tooltip -> msg
        , onMouseLeave : msg
    }
    -> { model | tokenImages : TokenImages }
    -> Positions
    -> { modal | pool : Pool, tooltip : Maybe Tooltip }
    -> Element msg
collateralOutSection msgs { tokenImages } positions ({ pool } as modal) =
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
        , collateralOutAmount msgs positions modal
        ]


collateralOutAmount :
    { msgs
        | onMouseEnter : Tooltip -> msg
        , onMouseLeave : msg
    }
    -> Positions
    -> { modal | pool : Pool, tooltip : Maybe Tooltip }
    -> Element msg
collateralOutAmount msgs positions ({ pool } as modal) =
    positions
        |> Positions.getClaimReturn pool
        |> Maybe.map .collateral
        |> Maybe.map (collateralAmount msgs modal)
        |> Maybe.withDefault
            (el
                [ width <| px 50
                , alignRight
                , centerY
                ]
                Loading.view
            )


collateralAmount :
    { msgs
        | onMouseEnter : Tooltip -> msg
        , onMouseLeave : msg
    }
    -> { modal | pool : { pool | pair : Pair }, tooltip : Maybe Tooltip }
    -> String
    -> Element msg
collateralAmount msgs { pool, tooltip } collateralOut =
    collateralOut
        |> Truncate.amount
        |> (\{ full, truncated } ->
                truncated
                    |> Maybe.map
                        (\short ->
                            pool.pair
                                |> Pair.toCollateral
                                |> Token.toSymbol
                                |> (\symbol ->
                                        row
                                            [ alignRight
                                            , centerY
                                            , paddingEach
                                                { top = 3
                                                , right = 0
                                                , bottom = 2
                                                , left = 0
                                                }
                                            , spacing 6
                                            , Font.bold
                                            , Font.size 16
                                            , Border.widthEach
                                                { top = 0
                                                , right = 0
                                                , bottom = 1
                                                , left = 0
                                                }
                                            , Border.dashed
                                            , Border.color Color.transparent200
                                            , Events.onMouseEnter (msgs.onMouseEnter Tooltip.CollateralOut)
                                            , Events.onMouseLeave msgs.onMouseLeave
                                            , (case tooltip of
                                                Just Tooltip.CollateralOut ->
                                                    [ full
                                                    , symbol
                                                    ]
                                                        |> String.join " "
                                                        |> Tooltip.amount

                                                _ ->
                                                    none
                                              )
                                                |> below
                                            ]
                                            [ el [ Font.color Color.transparent500 ] (text short)
                                            , el [ Font.color Color.transparent300 ] (text symbol)
                                            ]
                                   )
                        )
                    |> Maybe.withDefault
                        (row
                            [ alignRight
                            , centerY
                            , paddingXY 0 3
                            , spacing 6
                            , Font.bold
                            , Font.size 16
                            ]
                            [ el [ Font.color Color.transparent500 ] (text full)
                            , el
                                [ Font.color Color.transparent300 ]
                                (pool.pair
                                    |> Pair.toCollateral
                                    |> Token.toSymbol
                                    |> text
                                )
                            ]
                        )
           )
