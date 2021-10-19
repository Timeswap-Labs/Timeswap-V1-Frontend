module Modals.Pay.Transaction exposing (..)

import Data.Address as Address exposing (Address)
import Data.Allowances as Allowances exposing (Allowances)
import Data.Balances as Balances exposing (Balances)
import Data.Deadline as Deadline exposing (Deadline)
import Data.ERC20 as ERC20 exposing (ERC20)
import Data.Images exposing (Images)
import Data.Maturity as Maturity
import Data.Pair as Pair exposing (Pair)
import Data.Pool exposing (Pool)
import Data.Positions as Positions exposing (Positions)
import Data.Remote exposing (Remote(..))
import Data.Token as Token
import Data.TokenId as TokenId exposing (TokenId)
import Data.Uint as Uint exposing (Uint)
import Element
    exposing
        ( Element
        , centerX
        , centerY
        , el
        , fill
        , height
        , mouseDown
        , mouseOver
        , paddingEach
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
import Element.Input as Input
import Json.Encode as Encode exposing (Value)
import Modals.Pay.DuesIn as DuesIn exposing (DuesIn)
import Modals.Pay.Error as Error
import Sort.Dict as Dict exposing (Dict)
import Sort.Set exposing (Set)
import Time exposing (Posix)
import Utility.Color as Color


type alias Transaction =
    { pool : Pool
    , to : Address
    , dict : Dict TokenId Uint
    , deadline : Int
    }


toTransaction :
    { model
        | time : Posix
        , deadline : Deadline
    }
    ->
        { user
            | address : Address
            , balances : Remote () Balances
            , allowances : Remote () Allowances
        }
    -> Positions
    ->
        { modal
            | pool : Pool
            , tokenIds : Set TokenId
            , duesIn : Remote () { due | assetIn : String }
        }
    -> Maybe Transaction
toTransaction { time, deadline } ({ address } as user) positions ({ pool, tokenIds } as modal) =
    if
        (pool.maturity |> Maturity.isActive time)
            && hasAllowance user modal
    then
        positions
            |> Positions.toDueTransaction pool tokenIds
            |> Maybe.map
                (\dict ->
                    { pool = pool
                    , to = address
                    , dict = dict
                    , deadline = deadline |> Deadline.toInt time
                    }
                )

    else
        Nothing


encode : Transaction -> Value
encode { pool, to, dict, deadline } =
    dict
        |> Dict.toList
        |> List.unzip
        |> (\( ids, maxAssetsIn ) ->
                [ ( "asset", pool.pair |> Pair.toAsset |> Token.encode )
                , ( "collateral", pool.pair |> Pair.toCollateral |> Token.encode )
                , ( "maturity", pool.maturity |> Maturity.encode )
                , ( "collateralTo", to |> Address.encode )
                , ( "ids", ids |> Encode.list TokenId.encode )
                , ( "maxAssetsIn", maxAssetsIn |> Encode.list Uint.encode )
                , ( "deadline", deadline |> Encode.int )
                ]
                    |> Encode.object
           )


encodeApprove : ERC20 -> Value
encodeApprove erc20 =
    [ ( "erc20", erc20 |> ERC20.encode ) ]
        |> Encode.object


hasAllowance :
    { user | balances : Remote () Balances, allowances : Remote () Allowances }
    -> { modal | pool : { pool | pair : Pair }, duesIn : Remote () { due | assetIn : String } }
    -> Bool
hasAllowance { balances, allowances } { pool, duesIn } =
    case ( balances, allowances, duesIn ) of
        ( Success successBalances, Success successAllowances, Success { assetIn } ) ->
            (successBalances
                |> Balances.hasEnough (pool.pair |> Pair.toAsset) assetIn
            )
                && (successAllowances
                        |> Allowances.hasEnough (pool.pair |> Pair.toAsset) assetIn
                   )

        _ ->
            False


view :
    { msgs | approvePay : Value -> msg, pay : Value -> msg }
    ->
        { model
            | time : Posix
            , deadline : Deadline
            , images : Images
        }
    ->
        { user
            | address : Address
            , balances : Remote () Balances
            , allowances : Remote () Allowances
        }
    -> Positions
    -> { modal | pool : Pool, tokenIds : Set TokenId, duesIn : DuesIn }
    -> Element msg
view msgs model user positions ({ pool } as modal) =
    if modal |> DuesIn.hasTransaction model user positions then
        pool.pair
            |> Pair.toAsset
            |> (\token ->
                    case token of
                        Token.ETH ->
                            paySection msgs model user positions modal

                        Token.ERC20 erc20 ->
                            row
                                [ width fill
                                , height shrink
                                , spacing 20
                                ]
                                [ el
                                    [ width fill
                                    , height shrink
                                    ]
                                    (approveSection msgs model user positions modal erc20)
                                , el
                                    [ width fill
                                    , height shrink
                                    ]
                                    (paySection msgs model user positions modal)
                                ]
               )

    else
        Error.insufficientAsset


approveSection :
    { msgs | approvePay : Value -> msg }
    -> { model | time : Posix }
    ->
        { user
            | address : Address
            , balances : Remote () Balances
            , allowances : Remote () Allowances
        }
    -> Positions
    -> { modal | pool : Pool, tokenIds : Set TokenId, duesIn : DuesIn }
    -> ERC20
    -> Element msg
approveSection msgs model user positions modal erc20 =
    if
        DuesIn.hasTransaction model user positions modal
            && (hasAllowance user modal |> not)
    then
        approveButton msgs erc20

    else
        disabledApprove


approveButton :
    { msgs | approvePay : Value -> msg }
    -> ERC20
    -> Element msg
approveButton msgs erc20 =
    Input.button
        [ width fill
        , height <| px 44
        , paddingEach
            { top = 0
            , right = 16
            , bottom = 0
            , left = 10
            }
        , centerX
        , centerY
        , Background.color Color.primary500
        , Border.rounded 4
        , Font.size 16
        , Font.color Color.light100
        , mouseDown [ Background.color Color.primary400 ]
        , mouseOver [ Background.color Color.primary300 ]
        ]
        { onPress =
            erc20
                |> encodeApprove
                |> msgs.approvePay
                |> Just
        , label =
            el
                [ width shrink
                , height shrink
                , centerX
                , centerY
                , Font.bold
                , Font.size 16
                , Font.color Color.light100
                ]
                (text "Approve")
        }


disabledApprove : Element msg
disabledApprove =
    el
        [ width fill
        , height <| px 44
        , paddingEach
            { top = 0
            , right = 16
            , bottom = 0
            , left = 10
            }
        , centerX
        , centerY
        , Background.color Color.primary100
        , Border.rounded 4
        , Font.size 16
        , Font.color Color.light100
        ]
        (el
            [ width shrink
            , height shrink
            , centerX
            , centerY
            , Font.bold
            , Font.size 16
            , Font.color Color.transparent100
            ]
            (text "Approve")
        )


paySection :
    { msgs | pay : Value -> msg }
    ->
        { model
            | time : Posix
            , deadline : Deadline
        }
    ->
        { user
            | address : Address
            , balances : Remote () Balances
            , allowances : Remote () Allowances
        }
    -> Positions
    -> { modal | pool : Pool, tokenIds : Set TokenId, duesIn : DuesIn }
    -> Element msg
paySection msgs model user positions modal =
    toTransaction model user positions modal
        |> Maybe.map (payButton msgs)
        |> Maybe.withDefault disabledPay


payButton :
    { msgs | pay : Value -> msg }
    -> Transaction
    -> Element msg
payButton msgs transaction =
    Input.button
        [ width fill
        , height <| px 44
        , paddingEach
            { top = 0
            , right = 16
            , bottom = 0
            , left = 10
            }
        , centerX
        , centerY
        , Background.color Color.primary500
        , Border.rounded 4
        , Font.size 16
        , Font.color Color.light100
        , mouseDown [ Background.color Color.primary400 ]
        , mouseOver [ Background.color Color.primary300 ]
        ]
        { onPress =
            transaction
                |> encode
                |> msgs.pay
                |> Just
        , label =
            el
                [ width shrink
                , height shrink
                , centerX
                , centerY
                , Font.bold
                , Font.size 16
                , Font.color Color.light100
                ]
                (text "Unlock collateral")
        }


disabledPay : Element msg
disabledPay =
    el
        [ width fill
        , height <| px 44
        , paddingEach
            { top = 0
            , right = 16
            , bottom = 0
            , left = 10
            }
        , centerX
        , centerY
        , Background.color Color.primary100
        , Border.rounded 4
        , Font.size 16
        , Font.color Color.light100
        ]
        (el
            [ width shrink
            , height shrink
            , centerX
            , centerY
            , Font.bold
            , Font.size 16
            , Font.color Color.transparent100
            ]
            (text "Unlock collateral")
        )
