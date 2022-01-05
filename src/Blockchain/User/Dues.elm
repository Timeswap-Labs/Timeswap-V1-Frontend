module Blockchain.User.Dues exposing (Dues, toList)

import Blockchain.User.Due exposing (Due)
import Blockchain.User.TokenId exposing (TokenId)
import Data.Maturity as Maturity
import Data.Pool exposing (Pool)
import Sort.Dict as Dict exposing (Dict)
import Time exposing (Posix)


type alias Dues =
    Dict Pool (Dict TokenId Due)


toList :
    Posix
    -> Dues
    -> List ( Pool, Dict TokenId Due )
toList posix claims =
    claims
        |> Dict.partition
            (\{ maturity } _ ->
                maturity |> Maturity.isActive posix
            )
        |> Tuple.mapBoth Dict.toList Dict.toList
        |> (\( active, matured ) ->
                [ active
                , matured
                ]
                    |> List.concat
           )
