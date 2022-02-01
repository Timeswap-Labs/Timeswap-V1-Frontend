module Blockchain.User.Claims exposing
    ( Claims
    , decoder
    , toERC20s
    , toList
    )

import Blockchain.User.Claim as Claim exposing (Claim)
import Data.ERC20 as ERC20
import Data.ERC20s exposing (ERC20s)
import Data.Maturity as Maturity
import Data.Pair as Pair
import Data.Pool as Pool exposing (Pool)
import Data.Token as Token
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Modal.MaturityList.Sorting exposing (Sorting(..))
import Sort.Dict as Dict exposing (Dict)
import Sort.Set as Set
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


decoder : Decoder Claims
decoder =
    Decode.succeed Tuple.pair
        |> Pipeline.required "pool" Pool.decoder
        |> Pipeline.required "claim" Claim.decoder
        |> Decode.list
        |> Decode.map (Dict.fromList Pool.sorter)


toERC20s : Claims -> ERC20s
toERC20s claims =
    claims
        |> Dict.keys
        |> List.concatMap
            (\pool ->
                []
                    |> (++)
                        (pool.pair
                            |> Pair.toAsset
                            |> Token.toERC20
                            |> Maybe.map List.singleton
                            |> Maybe.withDefault []
                        )
                    |> (++)
                        (pool.pair
                            |> Pair.toCollateral
                            |> Token.toERC20
                            |> Maybe.map List.singleton
                            |> Maybe.withDefault []
                        )
            )
        |> Set.fromList ERC20.sorter
