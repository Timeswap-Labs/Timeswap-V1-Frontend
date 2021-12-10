module Blockchain.User.TxnWrite exposing (TxnWrite(..), decoder, encode)

import Data.ERC20 as ERC20 exposing (ERC20)
import Data.Pool as Pool exposing (Pool)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode exposing (Value)


type TxnWrite
    = Approve ERC20
    | Lend Pool
    | Borrow Pool
    | Liquidity Pool
    | Create Pool


decoder : Decoder TxnWrite
decoder =
    [ decoderApprove
    , decoderLend
    , decoderBorrow
    , decoderLiquidity
    , decoderCreate
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

        Borrow pool ->
            [ ( "txn", "borrow" |> Encode.string )
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
