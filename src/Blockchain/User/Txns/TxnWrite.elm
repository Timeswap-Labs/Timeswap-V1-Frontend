module Blockchain.User.Txns.TxnWrite exposing
    ( Flag
    , TxnWrite(..)
    , decoder
    , decoderUncomfirmed
    , encode
    , encodeUncomfirmed
    , initUnconfirmed
    )

import Data.ERC20 as ERC20 exposing (ERC20)
import Data.Pool as Pool exposing (Pool)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode exposing (Value)


type TxnWrite
    = Approve ERC20
    | Lend Pool
    | ApproveAndLend Pool
    | Borrow Pool
    | ApproveAndBorrow Pool
    | Liquidity Pool
    | Create Pool
    | Withdraw Pool
    | Pay Pool
    | Burn Pool


type alias Flag =
    { id : Int
    , write : Value
    }


initUnconfirmed : Flag -> Maybe ( Int, TxnWrite )
initUnconfirmed flag =
    flag.write
        |> Decode.decodeValue decoder
        |> Result.map
            (\write ->
                ( flag.id
                , write
                )
                    |> Just
            )
        |> Result.withDefault Nothing


decoderUncomfirmed : Decoder ( Int, TxnWrite )
decoderUncomfirmed =
    Decode.succeed Tuple.pair
        |> Pipeline.required "id" Decode.int
        |> Pipeline.required "write" decoder


decoder : Decoder TxnWrite
decoder =
    [ decoderApprove
    , decoderLend
    , decoderApproveAndLend
    , decoderBorrow
    , decoderApproveAndBorrow
    , decoderLiquidity
    , decoderCreate
    , decoderWithdraw
    , decoderPay
    , decoderBurn
    ]
        |> Decode.oneOf


decoderApprove : Decoder TxnWrite
decoderApprove =
    Decode.succeed
        (\txn erc20 ->
            if txn == "write" then
                Approve erc20 |> Decode.succeed

            else
                Decode.fail "Not a txn"
        )
        |> Pipeline.required "txn" Decode.string
        |> Pipeline.required "erc20" ERC20.decoder
        |> Decode.andThen identity


decoderLend : Decoder TxnWrite
decoderLend =
    Decode.succeed
        (\txn pool ->
            if txn == "lend" then
                Lend pool |> Decode.succeed

            else
                Decode.fail "Not a txn"
        )
        |> Pipeline.required "txn" Decode.string
        |> Pipeline.required "pool" Pool.decoder
        |> Decode.andThen identity


decoderApproveAndLend : Decoder TxnWrite
decoderApproveAndLend =
    Decode.succeed
        (\txn pool ->
            if txn == "approveAndLend" then
                ApproveAndLend pool |> Decode.succeed

            else
                Decode.fail "Not a txn"
        )
        |> Pipeline.required "txn" Decode.string
        |> Pipeline.required "pool" Pool.decoder
        |> Decode.andThen identity


decoderBorrow : Decoder TxnWrite
decoderBorrow =
    Decode.succeed
        (\txn pool ->
            if txn == "borrow" then
                Borrow pool |> Decode.succeed

            else
                Decode.fail "Not a txn"
        )
        |> Pipeline.required "txn" Decode.string
        |> Pipeline.required "pool" Pool.decoder
        |> Decode.andThen identity


decoderApproveAndBorrow : Decoder TxnWrite
decoderApproveAndBorrow =
    Decode.succeed
        (\txn pool ->
            if txn == "approveAndBorrow" then
                ApproveAndBorrow pool |> Decode.succeed

            else
                Decode.fail "Not a txn"
        )
        |> Pipeline.required "txn" Decode.string
        |> Pipeline.required "pool" Pool.decoder
        |> Decode.andThen identity


decoderLiquidity : Decoder TxnWrite
decoderLiquidity =
    Decode.succeed
        (\txn pool ->
            if txn == "liquidity" then
                Liquidity pool |> Decode.succeed

            else
                Decode.fail "Not a txn"
        )
        |> Pipeline.required "txn" Decode.string
        |> Pipeline.required "pool" Pool.decoder
        |> Decode.andThen identity


decoderCreate : Decoder TxnWrite
decoderCreate =
    Decode.succeed
        (\txn pool ->
            if txn == "create" then
                Create pool |> Decode.succeed

            else
                Decode.fail "Not a txn"
        )
        |> Pipeline.required "txn" Decode.string
        |> Pipeline.required "pool" Pool.decoder
        |> Decode.andThen identity


decoderWithdraw : Decoder TxnWrite
decoderWithdraw =
    Decode.succeed
        (\txn pool ->
            if txn == "withdraw" then
                Withdraw pool |> Decode.succeed

            else
                Decode.fail "Not a txn"
        )
        |> Pipeline.required "txn" Decode.string
        |> Pipeline.required "pool" Pool.decoder
        |> Decode.andThen identity


decoderPay : Decoder TxnWrite
decoderPay =
    Decode.succeed
        (\txn pool ->
            if txn == "pay" then
                Pay pool |> Decode.succeed

            else
                Decode.fail "Not a txn"
        )
        |> Pipeline.required "txn" Decode.string
        |> Pipeline.required "pool" Pool.decoder
        |> Decode.andThen identity


decoderBurn : Decoder TxnWrite
decoderBurn =
    Decode.succeed
        (\txn pool ->
            if txn == "burn" then
                Burn pool |> Decode.succeed

            else
                Decode.fail "Not a txn"
        )
        |> Pipeline.required "txn" Decode.string
        |> Pipeline.required "pool" Pool.decoder
        |> Decode.andThen identity


encode : TxnWrite -> Value
encode write =
    case write of
        Approve erc20 ->
            [ ( "txn", "write" |> Encode.string )
            , ( "erc20", erc20 |> ERC20.encode )
            ]
                |> Encode.object

        Lend pool ->
            [ ( "txn", "lend" |> Encode.string )
            , ( "pool", pool |> Pool.encode )
            ]
                |> Encode.object

        ApproveAndLend pool ->
            [ ( "txn", "approveAndLend" |> Encode.string )
            , ( "pool", pool |> Pool.encode )
            ]
                |> Encode.object

        Borrow pool ->
            [ ( "txn", "borrow" |> Encode.string )
            , ( "pool", pool |> Pool.encode )
            ]
                |> Encode.object

        ApproveAndBorrow pool ->
            [ ( "txn", "approveAndBorrow" |> Encode.string )
            , ( "pool", pool |> Pool.encode )
            ]
                |> Encode.object

        Liquidity pool ->
            [ ( "txn", "liquidity" |> Encode.string )
            , ( "pool", pool |> Pool.encode )
            ]
                |> Encode.object

        Create pool ->
            [ ( "txn", "create" |> Encode.string )
            , ( "pool", pool |> Pool.encode )
            ]
                |> Encode.object

        Withdraw pool ->
            [ ( "txn", "withdraw" |> Encode.string )
            , ( "pool", pool |> Pool.encode )
            ]
                |> Encode.object

        Pay pool ->
            [ ( "txn", "pay" |> Encode.string )
            , ( "pool", pool |> Pool.encode )
            ]
                |> Encode.object

        Burn pool ->
            [ ( "txn", "burn" |> Encode.string )
            , ( "pool", pool |> Pool.encode )
            ]
                |> Encode.object


encodeUncomfirmed : ( Int, TxnWrite ) -> Value
encodeUncomfirmed ( id, txnWrite ) =
    [ ( "id", id |> Encode.int )
    , ( "write", txnWrite |> encode )
    ]
        |> Encode.object
