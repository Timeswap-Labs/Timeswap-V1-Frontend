module Blockchain.User.Dues exposing (Dues)

import Blockchain.User.Due exposing (Due)
import Blockchain.User.TokenId exposing (TokenId)
import Data.Pool exposing (Pool)
import Sort.Dict exposing (Dict)


type alias Dues =
    Dict Pool (Dict TokenId Due)
