module Blockchain.User.Claims exposing
    ( Claims
    , dummy
    , toList
    )

import Blockchain.User.Claim exposing (Claim)
import Data.Maturity as Maturity
import Data.Pair as Pair
import Data.Pool as Pool exposing (Pool)
import Data.Uint as Uint
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
          , { bond = Uint.dummy, insurance = Uint.dummy }
          )
        , ( { pair = Pair.dummy
            , maturity = Maturity.dummy2
            }
          , { bond = Uint.dummy, insurance = Uint.dummy }
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
