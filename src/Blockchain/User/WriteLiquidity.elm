module Blockchain.User.WriteLiquidity exposing (WriteLiquidity(..), encode, toPool)

import Data.Address as Address exposing (Address)
import Data.Deadline as Deadline exposing (Deadline)
import Data.Maturity as Maturity
import Data.Pair as Pair
import Data.Pool exposing (Pool)
import Data.Token as Token
import Data.Uint as Uint exposing (Uint)
import Json.Encode as Encode exposing (Value)
import Time exposing (Posix)


type WriteLiquidity
    = GivenAsset LiquidityGivenAsset
    | GivenDebt LiquidityGivenDebt
    | GivenCollateral LiquidityGivenCollateral


type alias LiquidityGivenAsset =
    { pool : Pool
    , assetIn : Uint
    , minLiquidity : Uint
    , maxDebt : Uint
    , maxCollateral : Uint
    }


type alias LiquidityGivenDebt =
    { pool : Pool
    , debtIn : Uint
    , minLiquidity : Uint
    , maxAsset : Uint
    , maxCollateral : Uint
    }


type alias LiquidityGivenCollateral =
    { pool : Pool
    , collateralIn : Uint
    , minLiquidity : Uint
    , maxAsset : Uint
    , maxDebt : Uint
    }


toPool : WriteLiquidity -> Pool
toPool writeLiquidity =
    case writeLiquidity of
        GivenAsset { pool } ->
            pool

        GivenDebt { pool } ->
            pool

        GivenCollateral { pool } ->
            pool


encode :
    { model | time : Posix, deadline : Deadline }
    -> Address
    -> WriteLiquidity
    -> Value
encode model address write =
    case write of
        GivenAsset givenAsset ->
            givenAsset
                |> encodeWriteAsset model address

        GivenDebt givenDebt ->
            givenDebt
                |> encodeWriteDebt model address

        GivenCollateral givenCollateral ->
            givenCollateral
                |> encodeWriteCollateral model address


encodeWriteAsset :
    { model | time : Posix, deadline : Deadline }
    -> Address
    -> LiquidityGivenAsset
    -> Value
encodeWriteAsset { time, deadline } address { pool, assetIn, minLiquidity, maxDebt, maxCollateral } =
    [ ( "asset", pool.pair |> Pair.toAsset |> Token.encode )
    , ( "collateral", pool.pair |> Pair.toCollateral |> Token.encode )
    , ( "maturity", pool.maturity |> Maturity.encode )
    , ( "liquidityTo", address |> Address.encode )
    , ( "dueTo", address |> Address.encode )
    , ( "assetIn", assetIn |> Uint.encode )
    , ( "minLiquidity", minLiquidity |> Uint.encode )
    , ( "maxDebt", maxDebt |> Uint.encode )
    , ( "maxCollateral", maxCollateral |> Uint.encode )
    , ( "deadline", deadline |> Deadline.encodeUnix time )
    ]
        |> Encode.object


encodeWriteDebt :
    { model | time : Posix, deadline : Deadline }
    -> Address
    -> LiquidityGivenDebt
    -> Value
encodeWriteDebt { time, deadline } address { pool, debtIn, minLiquidity, maxAsset, maxCollateral } =
    [ ( "asset", pool.pair |> Pair.toAsset |> Token.encode )
    , ( "collateral", pool.pair |> Pair.toCollateral |> Token.encode )
    , ( "maturity", pool.maturity |> Maturity.encode )
    , ( "liquidityTo", address |> Address.encode )
    , ( "dueTo", address |> Address.encode )
    , ( "debtIn", debtIn |> Uint.encode )
    , ( "minLiquidity", minLiquidity |> Uint.encode )
    , ( "maxAsset", maxAsset |> Uint.encode )
    , ( "maxCollateral", maxCollateral |> Uint.encode )
    , ( "deadline", deadline |> Deadline.encodeUnix time )
    ]
        |> Encode.object


encodeWriteCollateral :
    { model | time : Posix, deadline : Deadline }
    -> Address
    -> LiquidityGivenCollateral
    -> Value
encodeWriteCollateral { time, deadline } address { pool, collateralIn, minLiquidity, maxAsset, maxDebt } =
    [ ( "asset", pool.pair |> Pair.toAsset |> Token.encode )
    , ( "collateral", pool.pair |> Pair.toCollateral |> Token.encode )
    , ( "maturity", pool.maturity |> Maturity.encode )
    , ( "liquidityTo", address |> Address.encode )
    , ( "dueTo", address |> Address.encode )
    , ( "collateralIn", collateralIn |> Uint.encode )
    , ( "minLiquidity", minLiquidity |> Uint.encode )
    , ( "maxAsset", maxAsset |> Uint.encode )
    , ( "maxDebt", maxDebt |> Uint.encode )
    , ( "deadline", deadline |> Deadline.encodeUnix time )
    ]
        |> Encode.object
