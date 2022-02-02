module Blockchain.User.Write exposing (decoder, encode)

import Data.Address as Address exposing (Address)
import Data.Chain as Chain exposing (Chain)
import Data.Hash as Hash exposing (Hash)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode exposing (Value)


type alias Receipt =
    { id : Int
    , chain : Chain
    , address : Address
    , hash : Maybe Hash
    }


decoder : Decoder Receipt
decoder =
    Decode.succeed Receipt
        |> Pipeline.required "id" Decode.int
        |> Pipeline.required "chain" Chain.decoder
        |> Pipeline.required "address" Address.decoder
        |> Pipeline.required "hash" (Hash.decoder |> Decode.nullable)


encode :
    Int
    -> Chain
    -> Address
    -> Value
    -> Value
encode id chain address send =
    [ ( "id", id |> Encode.int )
    , ( "chain", chain |> Chain.encode )
    , ( "address", address |> Address.encode )
    , ( "send", send )
    ]
        |> Encode.object
