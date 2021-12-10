module Blockchain.User.Txn exposing
    ( State(..)
    , Txn
    )

import Blockchain.User.TxnWrite exposing (TxnWrite)


type alias Txn =
    { id : Int
    , write : TxnWrite
    , state : State
    }


type State
    = Pending
    | Failed
    | Success
