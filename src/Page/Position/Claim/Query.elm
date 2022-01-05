module Page.Position.Claim.Query exposing
    ( Answer
    , Query
    , decoder
    , givenClaim
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


type alias Query =
    { chainId : Chain
    , pool : Pool
    , poolInfo : PoolInfo
    , claimsIn : Claim
    }


type alias Answer =
    { chainId : Chain
    , pool : Pool
    , poolInfo : PoolInfo
    , claimsIn : Claim
    , result : Result Error Return
    }


givenClaim : Query -> Value
givenClaim { chainId, pool, poolInfo, claimsIn } =
    [ ( "chainId", chainId |> Chain.encode )
    , ( "pool", pool |> Pool.encode )
    , ( "poolInfo", poolInfo |> PoolInfo.encode )
    , ( "claimsIn", claimsIn |> Claim.encode )
    ]
        |> Encode.object


decoder : Decoder Answer
decoder =
    Decode.succeed Answer
        |> Pipeline.required "chainId" Chain.decoder
        |> Pipeline.required "pool" Pool.decoder
        |> Pipeline.required "poolInfo" PoolInfo.decoder
        |> Pipeline.required "claimsIn" Claim.decoder
        |> Pipeline.required "result"
            ([ Return.decoder |> Decode.map Ok
             , Error.decoder |> Decode.map Err
             ]
                |> Decode.oneOf
            )
