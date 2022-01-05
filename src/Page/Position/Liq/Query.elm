module Page.Position.Liq.Query exposing
    ( Answer
    , Query
    , decoder
    , givenLiq
    )

import Blockchain.User.Liq as Liq exposing (Liq)
import Blockchain.User.Return as Return exposing (Return)
import Data.Chain as Chain exposing (Chain)
import Data.Maturity as Maturity
import Data.Pool as Pool exposing (Pool)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode exposing (Value)
import Page.PoolInfo as PoolInfo exposing (PoolInfo)
import Page.Position.Liq.Error as Error exposing (Error)


type alias Query =
    { chainId : Chain
    , pool : Pool
    , poolInfo : PoolInfo
    , liquidityIn : Liq
    }


type alias Answer =
    { chainId : Chain
    , pool : Pool
    , poolInfo : PoolInfo
    , liquidityInt : Liq
    , result : Result Error (Maturity.Status Return Float)
    }


givenLiq : Query -> Value
givenLiq { chainId, pool, poolInfo, liquidityIn } =
    [ ( "chainId", chainId |> Chain.encode )
    , ( "pool", pool |> Pool.encode )
    , ( "poolInfo", poolInfo |> PoolInfo.encode )
    , ( "liquidityIn", liquidityIn |> Liq.encode )
    ]
        |> Encode.object


decoder : Decoder Answer
decoder =
    Decode.succeed Answer
        |> Pipeline.required "chainId" Chain.decoder
        |> Pipeline.required "pool" Pool.decoder
        |> Pipeline.required "poolInfo" PoolInfo.decoder
        |> Pipeline.required "liquidityIn" Liq.decoder
        |> Pipeline.required "result"
            ([ Decode.float
                |> Decode.map Maturity.Active
                |> Decode.map Ok
             , Return.decoder
                |> Decode.map Maturity.Matured
                |> Decode.map Ok
             , Error.decoder |> Decode.map Err
             ]
                |> Decode.oneOf
            )
