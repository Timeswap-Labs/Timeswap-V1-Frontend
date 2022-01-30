module Blockchain.User.Claims exposing
    ( Claims
    , decoder
    , dummy
    , toList
    )

import Blockchain.User.Claim as Claim exposing (Claim)
import Data.Maturity as Maturity
import Data.Pair as Pair
import Data.Pool as Pool exposing (Pool)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Modal.MaturityList.Sorting exposing (Sorting(..))
import Sort.Dict as Dict exposing (Dict)
import Time exposing (Posix)


type alias Claims =
    Dict Pool Claim


dummy : Claims
dummy =
    Dict.fromList Pool.sorter
        [ ( { pair = Pair.dummy
            , maturity = Maturity.dummy
            }
          , Claim.dummy
          )
        , ( { pair = Pair.dummy
            , maturity = Maturity.dummy2
            }
          , Claim.dummy
          )
        ]


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
