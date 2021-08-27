module Data.Pools exposing (Pools, getSize, toPairs, whitelist)

import Data.Chain exposing (Chain)
import Data.Maturity exposing (Maturity)
import Data.Pair as Pair exposing (Pair)
import Data.Remote exposing (Remote)
import Sort.Dict as Dict exposing (Dict)


type Pools
    = Pools (Dict Pair (Dict Maturity (Remote Pool))) -- fix in the future


type alias Pool =
    {}


whitelist : Chain -> Pools
whitelist chain =
    Dict.empty (Pair.sorter chain)
        -- fix soon add whitelist
        |> Pools


toPairs : Pools -> List Pair
toPairs (Pools dict) =
    dict
        |> Dict.keys


getSize : Pair -> Pools -> Int
getSize pair (Pools dict) =
    dict
        |> Dict.get pair
        |> Maybe.map Dict.size
        -- fix in the future add time expiration
        |> Maybe.withDefault 0
