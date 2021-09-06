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
import Sort.Set exposing (Set)
import Time exposing (Posix)
import Utility.Color as Color
import Utility.Glass as Glass
import Utility.Loading as Loading
import Utility.TokenImage as TokenImage


type alias DuesIn =
    Remote
        { assetIn : String
        , collateralOut : String
        }


init : DuesIn
init =
    Loading


hasTransaction :
    { model | time : Posix }
    -> { user | balances : Remote Balances }
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
    { model | tokenImages : TokenImages }
    ->
        { modal
            | pool : { pool | pair : Pair }
            , duesIn : DuesIn
        }
    -> Element msg
view model modal =
    column
        [ width fill
        , height shrink
        , spacing 12
        ]
        [ assetInSection model modal
        , collateralOutSection model modal
        ]


assetInSection :
    { model | tokenImages : TokenImages }
    ->
        { modal
            | pool : { pool | pair : Pair }
            , duesIn : Remote { duesIn | assetIn : String }
        }
    -> Element msg
assetInSection { tokenImages } ({ pool } as modal) =
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
        , assetInAmount modal
        ]


assetInAmount :
    { modal
        | pool : { pool | pair : Pair }
        , duesIn : Remote { duesIn | assetIn : String }
    }
    -> Element msg
assetInAmount { pool, duesIn } =
    case duesIn of
        Loading ->
            el
                [ width <| px 50
                , alignRight
                , centerY
                ]
                Loading.view

        Failure ->
            none

        Success { assetIn } ->
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
                    (text assetIn)
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


collateralOutSection :
    { model | tokenImages : TokenImages }
    ->
        { modal
            | pool : { pool | pair : Pair }
            , duesIn : Remote { duesIn | collateralOut : String }
        }
    -> Element msg
collateralOutSection { tokenImages } ({ pool } as modal) =
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
        , collateralOutAmount modal
        ]


collateralOutAmount :
    { modal
        | pool : { pool | pair : Pair }
        , duesIn : Remote { duesIn | collateralOut : String }
    }
    -> Element msg
collateralOutAmount { pool, duesIn } =
    case duesIn of
        Loading ->
            el
                [ width <| px 50
                , alignRight
                , centerY
                ]
                Loading.view

        Failure ->
            none

        Success { collateralOut } ->
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
                    (text collateralOut)
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
