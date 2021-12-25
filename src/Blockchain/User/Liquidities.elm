module Blockchain.User.Liquidities exposing (Liquidities, subscriptions, update)

import Blockchain.User.Liquidity exposing (Liquidity)
import Blockchain.User.Return exposing (Return)
import Data.Pool as Pool exposing (Pool)
import Data.Remote as Remote exposing (Remote)
import Sort.Dict as Dict exposing (Dict)
import Time exposing (Posix)


type alias Liquidities =
    Dict Pool ( Liquidity, Maybe (Remote Never Return) )


update : Posix -> Liquidities -> Liquidities
update posix claims =
    claims
        |> Dict.toList
        |> (List.map << Tuple.mapSecond << Tuple.mapSecond << Maybe.map)
            (Remote.update posix)
        |> Dict.fromList Pool.sorter


subscriptions : (Posix -> msg) -> Liquidities -> Sub msg
subscriptions tick claims =
    claims
        |> Dict.values
        |> List.map Tuple.second
        |> List.filterMap identity
        |> List.map (Remote.subscriptions tick)
        |> Sub.batch
