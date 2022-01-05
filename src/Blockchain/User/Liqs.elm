module Blockchain.User.Liqs exposing (Liqs, toList)

import Blockchain.User.Liq exposing (Liq)
import Data.Maturity as Maturity
import Data.Pool exposing (Pool)
import Sort.Dict as Dict exposing (Dict)
import Time exposing (Posix)


type alias Liqs =
    Dict Pool Liq


toList :
    Posix
    -> Liqs
    -> List ( Pool, Liq )
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
