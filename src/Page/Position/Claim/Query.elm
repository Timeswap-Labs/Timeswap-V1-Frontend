module Page.Position.Claim.Query exposing
    ( AnswerQuery
    , AnswerSum
    , Query
    , Sum
    , decoderReturn
    , decoderSum
    , givenReturn
    , givenSum
    )

import Blockchain.User.Claim as Claim exposing (Claim)
import Blockchain.User.Return as Return exposing (Return)
import Data.Chain as Chain exposing (Chain)
import Data.Pool as Pool exposing (Pool)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode exposing (Value)
import Page.PoolInfo as PoolInfo exposing (PoolInfo)
import Page.Position.Claim.Error as Error exposing (Error)


type alias Sum =
    { chain : Chain
    , pool : Pool
    , claims : Claim
    }


type alias Query =
    { chain : Chain
    , pool : Pool
    , poolInfo : PoolInfo
    , claimsIn : Claim
    }


type alias AnswerSum =
    { chain : Chain
    , pool : Pool
    , claims : Claim
    , result : Result Error Return
    }


type alias AnswerQuery =
    { chain : Chain
    , pool : Pool
    , poolInfo : PoolInfo
    , claimsIn : Claim
    , result : Result Error Return
    }


givenSum : Sum -> Value
givenSum { chain, pool, claims } =
    [ ( "chain", chain |> Chain.encode )
    , ( "pool", pool |> Pool.encode )
    , ( "claims", claims |> Claim.encode )
    ]
        |> Encode.object


givenReturn : Query -> Value
givenReturn { chain, pool, poolInfo, claimsIn } =
    [ ( "chain", chain |> Chain.encode )
    , ( "pool", pool |> Pool.encode )
    , ( "poolInfo", poolInfo |> PoolInfo.encode )
    , ( "claimsIn", claimsIn |> Claim.encode )
    ]
        |> Encode.object


decoderSum : Decoder AnswerSum
decoderSum =
    Decode.succeed AnswerSum
        |> Pipeline.required "chain" Chain.decoder
        |> Pipeline.required "pool" Pool.decoder
        |> Pipeline.required "claims" Claim.decoder
        |> Pipeline.required "result"
            ([ Return.decoder |> Decode.map Ok
             , Error.decoder |> Decode.map Err
             ]
                |> Decode.oneOf
            )


decoderReturn : Decoder AnswerQuery
decoderReturn =
    Decode.succeed AnswerQuery
        |> Pipeline.required "chain" Chain.decoder
        |> Pipeline.required "pool" Pool.decoder
        |> Pipeline.required "poolInfo" PoolInfo.decoder
        |> Pipeline.required "claimsIn" Claim.decoder
        |> Pipeline.required "result"
            ([ Return.decoder |> Decode.map Ok
             , Error.decoder |> Decode.map Err
             ]
                |> Decode.oneOf
            )
