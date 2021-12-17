module Blockchain.User.Txns.Main exposing
    ( Txns
    , confirm
    , getPending
    , init
    , insert
    , isPending
    , toList
    , toPendingSize
    , updateFailed
    , updateSuccess
    )

import Blockchain.User.Txns.Txn as Txn exposing (Txn)
import Blockchain.User.Txns.TxnWrite as TxnWrite exposing (TxnWrite)
import Data.ERC20 exposing (ERC20)
import Data.Hash as Hash exposing (Hash)
import Sort
import Sort.Dict as Dict exposing (Dict)
import Sort.Set as Set exposing (Set)


type Txns
    = Txns
        { confirmed : Dict Hash Txn
        , uncomfirmed : Dict Int TxnWrite
        }


init : Txns
init =
    { confirmed = Dict.empty Hash.sorter
    , uncomfirmed = Dict.empty Sort.increasing
    }
        |> Txns


insert : TxnWrite -> Txns -> ( Int, Txns )
insert txnWrite (Txns ({ confirmed, uncomfirmed } as txns)) =
    [ confirmed |> Dict.size
    , uncomfirmed |> Dict.size
    ]
        |> List.sum
        |> (\id ->
                ( id
                , { txns
                    | uncomfirmed =
                        uncomfirmed
                            |> Dict.insert id txnWrite
                  }
                    |> Txns
                )
           )


confirm : Int -> Hash -> Txn.State -> Txns -> Txns
confirm id hash state (Txns ({ confirmed, uncomfirmed } as txns)) =
    uncomfirmed
        |> Dict.get id
        |> Maybe.map
            (\txnWrite ->
                { confirmed =
                    confirmed
                        |> Dict.insert hash
                            { id = id
                            , write = txnWrite
                            , state = state
                            }
                , uncomfirmed =
                    uncomfirmed
                        |> Dict.remove id
                }
            )
        |> Maybe.withDefault txns
        |> Txns


updateSuccess : Hash -> Txns -> Txns
updateSuccess hash (Txns ({ confirmed } as txns)) =
    confirmed
        |> Dict.get hash
        |> Maybe.map
            (\txn ->
                { txns
                    | confirmed =
                        confirmed
                            |> Dict.insert hash
                                { txn | state = Txn.Success }
                }
            )
        |> Maybe.withDefault txns
        |> Txns


updateFailed : Hash -> Txns -> Txns
updateFailed hash (Txns ({ confirmed } as txns)) =
    confirmed
        |> Dict.get hash
        |> Maybe.map
            (\txn ->
                { txns
                    | confirmed =
                        confirmed
                            |> Dict.insert hash
                                { txn | state = Txn.Failed }
                }
            )
        |> Maybe.withDefault txns
        |> Txns


getPending : Txns -> Set Hash
getPending (Txns { confirmed }) =
    confirmed
        |> Dict.keepIf
            (\_ { state } ->
                state == Txn.Pending
            )
        |> Dict.keys
        |> Set.fromList Hash.sorter


toPendingSize : Txns -> Int
toPendingSize (Txns { confirmed }) =
    confirmed
        |> Dict.keepIf
            (\_ { state } ->
                state == Txn.Pending
            )
        |> Dict.size


toList : Txns -> List ( Hash, Txn )
toList (Txns { confirmed }) =
    confirmed
        |> Dict.toList
        |> Sort.list
            (Sort.increasing
                |> Sort.reverse
                |> Sort.by .id
                |> Sort.by Tuple.second
            )


isPending : ERC20 -> Txns -> Bool
isPending givenERC20 (Txns { confirmed }) =
    confirmed
        |> Dict.foldl
            (\_ { write, state } accumulator ->
                case ( write, state ) of
                    ( TxnWrite.Approve erc20, Txn.Pending ) ->
                        givenERC20 == erc20

                    _ ->
                        accumulator
            )
            False
