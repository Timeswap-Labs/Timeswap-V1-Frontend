module Blockchain.User.WriteWithdraw exposing
    ( WriteWithdraw
    , encode
    , toPool
    )

import Blockchain.User.Claim as Claim exposing (Claim)
import Data.Address as Address exposing (Address)
import Data.Maturity as Maturity
import Data.Pair as Pair
import Data.Pool exposing (Pool)
import Data.Token as Token
import Json.Encode as Encode exposing (Value)


type alias WriteWithdraw =
    { pool : Pool
    , claimsIn : Claim
    }


toPool : WriteWithdraw -> Pool
toPool { pool } =
    pool


encode :
    Address
    -> WriteWithdraw
    -> Value
encode address { pool, claimsIn } =
    [ ( "asset", pool.pair |> Pair.toAsset |> Token.encode )
    , ( "collateral", pool.pair |> Pair.toCollateral |> Token.encode )
    , ( "maturity", pool.maturity |> Maturity.encode )
    , ( "assetTo", address |> Address.encode )
    , ( "collateralTo", address |> Address.encode )
    , ( "claimsIn", claimsIn |> Claim.encode )
    ]
        |> Encode.object
