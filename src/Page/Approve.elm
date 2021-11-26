module Page.Approve exposing (Write, encode)

import Blockchain.Main as Blockchain exposing (Blockchain)
import Blockchain.User.Main as User exposing (User)
import Data.Address as Address
import Data.Chain as Chain
import Data.ERC20 as ERC20 exposing (ERC20)
import Json.Encode as Encode exposing (Value)


type alias Write =
    ERC20


encode :
    Blockchain
    -> User
    -> Write
    -> Value
encode blockchain user write =
    [ ( "chainId"
      , blockchain
            |> Blockchain.toChain
            |> Chain.encode
      )
    , ( "address"
      , user
            |> User.toAddress
            |> Address.encode
      )
    , ( "erc20"
      , write |> ERC20.encode
      )
    ]
        |> Encode.object
