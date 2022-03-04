module Page.Info.Answer exposing (..)

import Data.Pool as Pool exposing (Pool)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Page.PoolInfo as PoolInfo exposing (PoolInfo)
import Sort.Dict as Dict exposing (Dict)


type alias Answer =
    Dict Pool PoolInfo


decoder : Decoder Answer
decoder =
    Decode.succeed Tuple.pair
        |> Pipeline.required "pool" Pool.decoder
        |> Pipeline.required "poolInfo" PoolInfo.decoder
        |> Decode.list
        |> Decode.map (Dict.fromList Pool.sorter)
