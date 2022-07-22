module Blockchain.User.WriteFlashRepay exposing (WriteFlashRepay, encode, toPool)

import Blockchain.User.TokenId as TokenId exposing (TokenId)
import Data.Address as Address exposing (Address)
import Data.Maturity as Maturity
import Data.Pair as Pair
import Data.Pool exposing (Pool)
import Data.Token as Token
import Json.Encode as Encode exposing (Value)


type alias WriteFlashRepay =
    { pool : Pool
    , cdtAddress : Address
    , tokenIds : List TokenId
    }


toPool : WriteFlashRepay -> Pool
toPool { pool } =
    pool


encode :
    WriteFlashRepay
    -> Value
encode { pool, cdtAddress, tokenIds } =
    [ ( "asset", pool.pair |> Pair.toAsset |> Token.encode )
    , ( "collateral", pool.pair |> Pair.toCollateral |> Token.encode )
    , ( "maturity", pool.maturity |> Maturity.encode )
    , ( "cdtAddress", cdtAddress |> Address.encode )
    , ( "ids", tokenIds |> Encode.list TokenId.encode )
    ]
        |> Encode.object
