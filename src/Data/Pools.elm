module Data.Pools exposing (PoolInfo, Pools, getFirst, getSize, toList, toPairs, whitelist)

import Data.Chain exposing (Chain)
import Data.Maturity exposing (Maturity)
import Data.Pair as Pair exposing (Pair)
import Data.Remote as Remote exposing (Remote)
import Sort.Dict as Dict exposing (Dict)


type Pools
    = Pools (Dict Pair (Dict Maturity (Remote Pool))) -- fix in the future


type alias Pool =
    { assetLiquidity : String
    , collateralLiquidity : String
    , apr : String
    , cf : String
    }


type alias PoolInfo =
    { pair : Pair
    , maturity : Maturity
    , pool : Remote Pool
    }


whitelist : Chain -> Pools
whitelist chain =
    Dict.empty (Pair.sorter chain)
        -- fix soon add whitelist
        |> Pools


toPairs : Pools -> List Pair
toPairs (Pools dict) =
    dict
        |> Dict.keys


toList : Pools -> List (List PoolInfo)
toList (Pools dict) =
    dict
        |> Dict.toList
        |> List.map
            (\( pair, innerDict ) ->
                innerDict
                    |> Dict.toList
                    |> List.map
                        (\( maturity, pool ) ->
                            { pair = pair
                            , maturity = maturity
                            , pool = pool
                            }
                        )
            )


getSize : Pair -> Pools -> Int
getSize pair (Pools dict) =
    dict
        |> Dict.get pair
        |> Maybe.map Dict.size
        -- fix in the future add time expiration
        |> Maybe.withDefault 0


getFirst : Pools -> Maybe Pair
getFirst (Pools dict) =
    dict
        |> Dict.keys
        |> List.head
