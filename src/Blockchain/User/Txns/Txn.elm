module Blockchain.User.Txns.Txn exposing
    ( State(..)
    , Txn
    )

import Blockchain.User.Txns.TxnWrite exposing (TxnWrite)


type alias Txn =
    { id : Int
    , write : TxnWrite
    , state : State
    }


type State
    = Pending
    | Failed
    | Success
