module Blockchain.User.Transaction exposing
    ( State(..)
    , Transaction
    , Write(..)
    )

import Data.ERC20 exposing (ERC20)
import Data.Hash exposing (Hash)
import Data.Pool exposing (Pool)
import Data.Uint exposing (Uint)


type alias Transaction =
    { hash : Hash
    , write : Write
    , state : State
    }


type Write
    = Approve ERC20
    | Lend Pool Uint
    | Borrow Pool Uint
    | Liquidity Pool


type State
    = Pending
    | Failed
    | Success
