module Utility.Etherscan exposing (fromHash, fromUser)

import Blockchain.User.Main as User exposing (User)
import Data.Address as Address
import Data.Chain as Chain exposing (Chain)
import Data.Hash as Hash exposing (Hash)
import Url.Builder as Builder


fromUser : Chain -> User -> String
fromUser chain user =
    Builder.crossOrigin
        (chain |> Chain.toEtherscan)
        [ "address"
        , user
            |> User.toName
            |> Maybe.withDefault
                (user
                    |> User.toAddress
                    |> Address.toString
                )
        ]
        []


fromHash : Chain -> Hash -> String
fromHash chain hash =
    Builder.crossOrigin
        (chain |> Chain.toEtherscan)
        [ "tx"
        , hash |> Hash.toString
        ]
        []
