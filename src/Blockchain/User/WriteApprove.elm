module Blockchain.User.WriteApprove exposing (encode)

import Data.Address as Address exposing (Address)
import Data.Chain as Chain exposing (Chain)
import Data.ERC20 as ERC20 exposing (ERC20)
import Json.Encode as Encode exposing (Value)


encode :
    Int
    -> Chain
    -> Address
    -> ERC20
    -> Value
encode id chain address write =
    [ ( "id", id |> Encode.int )
    , ( "chainId", chain |> Chain.encode )
    , ( "address", address |> Address.encode )
    , ( "erc20", write |> ERC20.encode )
    ]
        |> Encode.object
