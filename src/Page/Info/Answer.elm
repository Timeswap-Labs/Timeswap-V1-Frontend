module Page.Info.Answer exposing (..)

import Data.Chain exposing (Chain)
import Data.Chains exposing (Chains)
import Data.Pool as Pool exposing (Pool)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Page.PoolInfo as PoolInfo exposing (PoolInfo)
import Sort.Dict as Dict exposing (Dict)


type alias Answer =
    Dict Pool PoolInfo


decoder : Chain -> Chains -> Decoder Answer
decoder chain chains =
    Decode.succeed Tuple.pair
        |> Pipeline.required "pool" Pool.decoder
        |> Pipeline.required "poolInfo" PoolInfo.decoder
        |> Decode.list
        |> (Decode.map << List.map << Tuple.mapFirst) (Pool.toNative chain chains)
        |> Decode.map (Dict.fromList Pool.sorter)
