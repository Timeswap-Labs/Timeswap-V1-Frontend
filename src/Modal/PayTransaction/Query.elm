module Modal.PayTransaction.Query exposing (Proportion, Sum, decoderProportion, decoderSum, proportion, sum)

import Blockchain.User.Due as Due exposing (Due)
import Blockchain.User.TokenId as TokenId exposing (TokenId)
import Data.Chain as Chain exposing (Chain)
import Data.Pool as Pool exposing (Pool)
import Data.Uint as Uint exposing (Uint)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode exposing (Value)
import Modal.PayTransaction.Error as Error exposing (Error)
import Modal.PayTransaction.Total as Total exposing (Total)


type alias Sum =
    { chain : Chain
    , pool : Pool
    , duesIn : List Total
    }


type alias Proportion =
    { chain : Chain
    , pool : Pool
    , tokenId : TokenId
    , due : Due
    , assetIn : Uint
    }


type alias SumAnswer =
    { chain : Chain
    , pool : Pool
    , duesIn : List Total
    , result : Result Error Total
    }


type alias ProportionAnswer =
    { chain : Chain
    , pool : Pool
    , tokenId : TokenId
    , due : Due
    , assetIn : Uint
    , result : Result Error Uint
    }


sum : Sum -> Value
sum { chain, pool, duesIn } =
    [ ( "chain", chain |> Chain.encode )
    , ( "pool", pool |> Pool.encode )
    , ( "duesInt", duesIn |> Encode.list Total.encode )
    ]
        |> Encode.object


proportion : Proportion -> Value
proportion { chain, pool, tokenId, due, assetIn } =
    [ ( "chain", chain |> Chain.encode )
    , ( "pool", pool |> Pool.encode )
    , ( "tokenId", tokenId |> TokenId.encode )
    , ( "due", due |> Due.encode )
    , ( "assetIn", assetIn |> Uint.encode )
    ]
        |> Encode.object


decoderSum : Decoder SumAnswer
decoderSum =
    Decode.succeed SumAnswer
        |> Pipeline.required "chain" Chain.decoder
        |> Pipeline.required "pool" Pool.decoder
        |> Pipeline.required "duesIn" (Decode.list Total.decoder)
        |> Pipeline.required "result"
            ([ Total.decoder |> Decode.map Ok
             , Error.decoder |> Decode.map Err
             ]
                |> Decode.oneOf
            )


decoderProportion : Decoder ProportionAnswer
decoderProportion =
    Decode.succeed ProportionAnswer
        |> Pipeline.required "chain" Chain.decoder
        |> Pipeline.required "pool" Pool.decoder
        |> Pipeline.required "tokenId" TokenId.decoder
        |> Pipeline.required "due" Due.decoder
        |> Pipeline.required "assetIn" Uint.decoder
        |> Pipeline.required "result"
            ([ Uint.decoder |> Decode.map Ok
             , Error.decoder |> Decode.map Err
             ]
                |> Decode.oneOf
            )
