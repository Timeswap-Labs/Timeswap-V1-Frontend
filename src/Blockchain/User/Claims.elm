module Blockchain.User.Claims exposing
    ( Claims
    , toList
    )

import Blockchain.User.Claim exposing (Claim)
import Data.Maturity as Maturity
import Data.Pool exposing (Pool)
import Sort.Dict as Dict exposing (Dict)
import Time exposing (Posix)


type alias Claims =
    Dict Pool Claim


toList :
    Posix
    -> Claims
    -> List ( Pool, Claim )
toList posix claims =
    claims
        |> Dict.partition
            (\{ maturity } _ ->
                maturity |> Maturity.isActive posix
            )
        |> Tuple.mapBoth Dict.toList Dict.toList
        |> (\( active, matured ) ->
                [ matured
                , active
                ]
                    |> List.concat
           )
