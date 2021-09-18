module Modals.Pay.DuesIn exposing
    ( DuesIn
    , hasTransaction
    , init
    , view
    )

import Data.Balances as Balances exposing (Balances)
import Data.Maturity as Maturity
import Data.Pair as Pair exposing (Pair)
import Data.Pool exposing (Pool)
import Data.Positions as Positions exposing (Positions)
import Data.Remote exposing (Remote(..))
import Data.Token as Token
import Data.TokenId exposing (TokenId)
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
import Modals.Pay.Tooltip as Tooltip exposing (Tooltip)
import Sort.Set exposing (Set)
import Time exposing (Posix)
import Utility.Color as Color
import Utility.Glass as Glass
import Utility.Loading as Loading
import Utility.TokenImage as TokenImage
import Utility.Truncate as Truncate


type alias DuesIn =
    Remote
        ()
        { assetIn : String
        , collateralOut : String
        }


init : DuesIn
init =
    Loading


hasTransaction :
    { model | time : Posix }
    -> { user | balances : Remote () Balances }
    -> Positions
    ->
        { modal
            | pool : Pool
            , tokenIds : Set TokenId
            , duesIn : DuesIn
        }
    -> Bool
hasTransaction { time } { balances } positions { pool, tokenIds, duesIn } =
    (pool.maturity |> Maturity.isActive time)
        && (case ( balances, duesIn ) of
                ( Success successBalances, Success { assetIn } ) ->
                    (successBalances
                        |> Balances.hasEnough (pool.pair |> Pair.toAsset) assetIn
                    )
                        && (positions |> Positions.isOwner pool tokenIds)

                _ ->
                    False
           )


view :
    { msgs
        | onMouseEnter : Tooltip -> msg
        , onMouseLeave : msg
    }
    -> { model | tokenImages : TokenImages }
    ->
        { modal
            | pool : { pool | pair : Pair }
            , duesIn : DuesIn
            , tooltip : Maybe Tooltip
        }
    -> Element msg
view msgs model modal =
    column
        [ width fill
        , height shrink
        , spacing 12
        ]
        [ assetInSection msgs model modal
        , collateralOutSection msgs model modal
        ]


assetInSection :
    { msgs
        | onMouseEnter : Tooltip -> msg
        , onMouseLeave : msg
    }
    -> { model | tokenImages : TokenImages }
    ->
        { modal
            | pool : { pool | pair : Pair }
            , duesIn : Remote () { duesIn | assetIn : String }
            , tooltip : Maybe Tooltip
        }
    -> Element msg
assetInSection msgs { tokenImages } ({ pool } as modal) =
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
            (text "Total debt to pay")
        , pool.pair
            |> Pair.toAsset
            |> TokenImage.icon tokenImages
                [ width <| px 32
                , alignRight
                , centerY
                ]
        , assetInAmount msgs modal
        ]


assetInAmount :
    { msgs
        | onMouseEnter : Tooltip -> msg
        , onMouseLeave : msg
    }
    ->
        { modal
            | pool : { pool | pair : Pair }
            , duesIn : Remote () { duesIn | assetIn : String }
            , tooltip : Maybe Tooltip
        }
    -> Element msg
assetInAmount msgs ({ duesIn } as modal) =
    case duesIn of
        Loading ->
            el
                [ width <| px 50
                , alignRight
                , centerY
                ]
                Loading.view

        Failure _ ->
            none

        Success { assetIn } ->
            assetAmount msgs modal assetIn


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
                                            , Events.onMouseEnter (msgs.onMouseEnter Tooltip.AssetIn)
                                            , Events.onMouseLeave msgs.onMouseLeave
                                            , (case tooltip of
                                                Just Tooltip.AssetIn ->
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
    ->
        { modal
            | pool : { pool | pair : Pair }
            , duesIn : Remote () { duesIn | collateralOut : String }
            , tooltip : Maybe Tooltip
        }
    -> Element msg
collateralOutSection msgs { tokenImages } ({ pool } as modal) =
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
            (text "Collateral to unlock")
        , pool.pair
            |> Pair.toCollateral
            |> TokenImage.icon tokenImages
                [ width <| px 32
                , alignRight
                , centerY
                ]
        , collateralOutAmount msgs modal
        ]


collateralOutAmount :
    { msgs
        | onMouseEnter : Tooltip -> msg
        , onMouseLeave : msg
    }
    ->
        { modal
            | pool : { pool | pair : Pair }
            , duesIn : Remote () { duesIn | collateralOut : String }
            , tooltip : Maybe Tooltip
        }
    -> Element msg
collateralOutAmount msgs ({ duesIn } as modal) =
    case duesIn of
        Loading ->
            el
                [ width <| px 50
                , alignRight
                , centerY
                ]
                Loading.view

        Failure _ ->
            none

        Success { collateralOut } ->
            collateralAmount msgs modal collateralOut


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
