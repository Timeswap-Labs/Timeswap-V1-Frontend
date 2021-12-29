module Blockchain.User.Claims exposing
    ( Claims
    , subscriptions
    , toList
    , update
    )

import Blockchain.User.Claim exposing (Claim)
import Blockchain.User.Return exposing (Return)
import Data.Maturity as Maturity
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


toList :
    Posix
    -> Claims
    -> List ( Pool, ( Claim, Maybe (Remote Never Return) ) )
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
