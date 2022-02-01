module Blockchain.User.Liqs exposing (Liqs, decoder, toERC20s, toList)

import Blockchain.User.Liq as Liq exposing (Liq)
import Data.ERC20 as ERC20
import Data.ERC20s exposing (ERC20s)
import Data.Maturity as Maturity
import Data.Pair as Pair
import Data.Pool as Pool exposing (Pool)
import Data.Token as Token
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Sort.Dict as Dict exposing (Dict)
import Sort.Set as Set
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


decoder : Decoder Liqs
decoder =
    Decode.succeed Tuple.pair
        |> Pipeline.required "pool" Pool.decoder
        |> Pipeline.required "liq" Liq.decoder
        |> Decode.list
        |> Decode.map (Dict.fromList Pool.sorter)


toERC20s : Liqs -> ERC20s
toERC20s liqs =
    liqs
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
