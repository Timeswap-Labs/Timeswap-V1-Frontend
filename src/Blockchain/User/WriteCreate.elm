module Blockchain.User.WriteCreate exposing (WriteCreate, encode, toPool)

import Data.Address as Address exposing (Address)
import Data.Deadline as Deadline exposing (Deadline)
import Data.Maturity as Maturity
import Data.Pair as Pair
import Data.Pool exposing (Pool)
import Data.Token as Token
import Data.Uint as Uint exposing (Uint)
import Json.Encode as Encode exposing (Value)
import Time exposing (Posix)


type alias WriteCreate =
    { pool : Pool
    , assetIn : Uint
    , debtIn : Uint
    , collateralIn : Uint
    }


toPool : WriteCreate -> Pool
toPool =
    .pool


encode :
    { model | time : Posix, deadline : Deadline }
    -> Address
    -> WriteCreate
    -> Value
encode { time, deadline } address { pool, assetIn, debtIn, collateralIn } =
    [ ( "asset", pool.pair |> Pair.toAsset |> Token.encode )
    , ( "collateral", pool.pair |> Pair.toCollateral |> Token.encode )
    , ( "maturity", pool.maturity |> Maturity.encode )
    , ( "liquidityTo", address |> Address.encode )
    , ( "dueTo", address |> Address.encode )
    , ( "assetIn", assetIn |> Uint.encode )
    , ( "debtIn", debtIn |> Uint.encode )
    , ( "collateralIn", collateralIn |> Uint.encode )
    , ( "deadline", deadline |> Deadline.encodeUnix time )
    ]
        |> Encode.object
