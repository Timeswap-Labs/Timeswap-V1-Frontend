module Blockchain.User.Txns exposing (Txns)

import Blockchain.User.Txn exposing (Txn)
import Data.Hash exposing (Hash)
import Sort.Dict as Dict exposing (Dict)


type alias Txns =
    Dict Hash Txn
