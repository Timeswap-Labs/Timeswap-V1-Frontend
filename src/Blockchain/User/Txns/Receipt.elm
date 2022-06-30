module Blockchain.User.Txns.Receipt exposing (Answer, State(..), decoder, encode)

import Blockchain.User.Txns.TxnWrite as TxnWrite exposing (TxnWrite)
import Data.Address as Address exposing (Address)
import Data.Chain as Chain exposing (Chain)
import Data.Hash as Hash exposing (Hash)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode exposing (Value)


type alias Receipt =
    { chain : Chain
    , address : Address
    , hash : Hash
    }


type alias Answer =
    { id : Int
    , chain : Chain
    , address : Address
    , hash : Hash
    , state : State
    , txnType : Maybe TxnWrite
    }


type State
    = Failed
    | Success


encode : Receipt -> Value
encode { chain, address, hash } =
    [ ( "chain", chain |> Chain.encode )
    , ( "address", address |> Address.encode )
    , ( "hash", hash |> Hash.encode )
    ]
        |> Encode.object


decoder : Decoder Answer
decoder =
    Decode.succeed Answer
        |> Pipeline.required "id" Decode.int
        |> Pipeline.required "chain" Chain.decoder
        |> Pipeline.required "address" Address.decoder
        |> Pipeline.required "hash" Hash.decoder
        |> Pipeline.required "state" decoderState
        |> Pipeline.optional "txnType" (TxnWrite.decoder |> Decode.nullable) Nothing


decoderState : Decoder State
decoderState =
    Decode.string
        |> Decode.andThen
            (\string ->
                if string == "failed" then
                    Failed |> Decode.succeed

                else if string == "success" then
                    Success |> Decode.succeed

                else
                    Decode.fail "not a state"
            )
