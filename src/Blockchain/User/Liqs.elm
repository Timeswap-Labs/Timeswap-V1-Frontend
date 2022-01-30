module Blockchain.User.Liqs exposing (Liqs, decoder, toList)

import Blockchain.User.Liq as Liq exposing (Liq)
import Data.Maturity as Maturity
import Data.Pool as Pool exposing (Pool)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
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


decoder : Decoder Liqs
decoder =
    Decode.succeed Tuple.pair
        |> Pipeline.required "pool" Pool.decoder
        |> Pipeline.required "liq" Liq.decoder
        |> Decode.list
        |> Decode.map (Dict.fromList Pool.sorter)
