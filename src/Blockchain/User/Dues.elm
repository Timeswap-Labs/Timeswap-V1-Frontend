module Blockchain.User.Dues exposing (Dues, dummy, getMultiple, toList)

import Blockchain.User.Due exposing (Due)
import Blockchain.User.TokenId as TokenId exposing (TokenId)
import Data.Maturity as Maturity
import Data.Pair as Pair
import Data.Pool as Pool exposing (Pool)
import Data.Uint as Uint
import Sort.Dict as Dict exposing (Dict)
import Sort.Set as Set exposing (Set)
import Time exposing (Posix)


type alias Dues =
    Dict Pool (Dict TokenId Due)


dummy : Dues
dummy =
    Dict.fromList Pool.sorter
        [ ( { pair = Pair.dummy
            , maturity = Maturity.dummy
            }
          , Dict.fromList TokenId.sorter
                [ ( Uint.dummy
                  , { debt = Uint.dummy, collateral = Uint.dummy }
                  )
                , ( Uint.dummy
                  , { debt = Uint.dummy, collateral = Uint.dummy }
                  )
                ]
          )
        , ( { pair = Pair.dummy2
            , maturity = Maturity.dummy2
            }
          , Dict.fromList TokenId.sorter
                [ ( Uint.dummy
                  , { debt = Uint.dummy, collateral = Uint.dummy }
                  )
                , ( Uint.dummy
                  , { debt = Uint.dummy, collateral = Uint.dummy }
                  )
                ]
          )
        , ( { pair = Pair.dummy
            , maturity = Maturity.dummy
            }
          , Dict.fromList TokenId.sorter
                [ ( Uint.dummy2
                  , { debt = Uint.dummy, collateral = Uint.dummy }
                  )
                , ( Uint.dummy
                  , { debt = Uint.dummy, collateral = Uint.dummy }
                  )
                ]
          )
        ]


toList :
    Posix
    -> Dues
    -> List ( Pool, Dict TokenId Due )
toList posix dues =
    dues
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


getMultiple : Pool -> Set TokenId -> Dues -> Maybe (Dict TokenId Due)
getMultiple pool tokenIds dues =
    dues
        |> Dict.get pool
        |> (Maybe.map << Dict.keepIf)
            (\tokenId _ ->
                tokenId
                    |> Set.memberOf tokenIds
            )
