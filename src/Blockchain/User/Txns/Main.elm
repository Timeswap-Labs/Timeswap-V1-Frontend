module Blockchain.User.Txns.Main exposing
    ( Flags
    , Txns
    , confirm
    , decoder
    , encode
    , getPending
    , init
    , initEmpty
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
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode exposing (Value)
import Sort
import Sort.Dict as Dict exposing (Dict)
import Sort.Set as Set exposing (Set)


type Txns
    = Txns
        { confirmed : Dict Hash Txn
        , uncomfirmed : Dict Int TxnWrite
        }


type alias Flags =
    { confirmed : List Txn.Flag
    , uncomfirmed : List TxnWrite.Flag
    }


init : Flags -> Txns
init flags =
    { confirmed =
        flags.confirmed
            |> List.map Txn.initConfirmed
            |> List.filterMap identity
            |> Dict.fromList Hash.sorter
    , uncomfirmed =
        flags.uncomfirmed
            |> List.map TxnWrite.initUnconfirmed
            |> List.filterMap identity
            |> Dict.fromList Sort.increasing
    }
        |> Txns


initEmpty : Txns
initEmpty =
    { confirmed = Dict.empty Hash.sorter
    , uncomfirmed = Dict.empty Sort.increasing
    }
        |> Txns


decoder : Decoder Txns
decoder =
    Decode.succeed
        (\confirmed uncomfirmed ->
            { confirmed = confirmed
            , uncomfirmed = uncomfirmed
            }
                |> Txns
        )
        |> Pipeline.required "confirmed"
            (Txn.decoderConfirmed
                |> Decode.list
                |> Decode.map (Dict.fromList Hash.sorter)
            )
        |> Pipeline.required "uncomfirmed"
            (TxnWrite.decoderUncomfirmed
                |> Decode.list
                |> Decode.map (Dict.fromList Sort.increasing)
            )


encode : Txns -> Value
encode (Txns { confirmed, uncomfirmed }) =
    [ ( "confirmed"
      , confirmed
            |> Dict.toList
            |> Encode.list Txn.encodeConfirmed
      )
    , ( "uncomfirmed"
      , uncomfirmed
            |> Dict.toList
            |> Encode.list TxnWrite.encodeUncomfirmed
      )
    ]
        |> Encode.object


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
