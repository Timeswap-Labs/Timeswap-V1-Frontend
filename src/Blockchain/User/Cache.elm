module Blockchain.User.Cache exposing (encodeTxns)

import Blockchain.User.Txns.Main as Txns exposing (Txns)
import Data.Address as Address exposing (Address)
import Data.Chain as Chain exposing (Chain)
import Json.Encode as Encode exposing (Value)


encodeTxns : Chain -> Address -> Txns -> Value
encodeTxns chain address txns =
    [ ( "chainId", chain |> Chain.encode )
    , ( "address", address |> Address.encode )
    , ( "txns", txns |> Txns.encode )
    ]
        |> Encode.object
