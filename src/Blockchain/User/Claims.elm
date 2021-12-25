module Blockchain.User.Claims exposing (Claims, subscriptions, update)

import Blockchain.User.Claim exposing (Claim)
import Blockchain.User.Return exposing (Return)
import Data.Pool as Pool exposing (Pool)
import Data.Remote as Remote exposing (Remote)
import Sort.Dict as Dict exposing (Dict)
import Time exposing (Posix)


type alias Claims =
    Dict Pool ( Claim, Maybe (Remote Never Return) )


update : Posix -> Claims -> Claims
update posix claims =
    claims
        |> Dict.toList
        |> (List.map << Tuple.mapSecond << Tuple.mapSecond << Maybe.map)
            (Remote.update posix)
        |> Dict.fromList Pool.sorter


subscriptions : (Posix -> msg) -> Claims -> Sub msg
subscriptions tick claims =
    claims
        |> Dict.values
        |> List.map Tuple.second
        |> List.filterMap identity
        |> List.map (Remote.subscriptions tick)
        |> Sub.batch
