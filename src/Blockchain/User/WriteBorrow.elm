module Blockchain.User.WriteBorrow exposing (WriteBorrow(..), encode, toPool)

import Data.Address as Address exposing (Address)
import Data.Deadline as Deadline exposing (Deadline)
import Data.Maturity as Maturity
import Data.Pair as Pair
import Data.Percent as Percent exposing (Percent)
import Data.Pool exposing (Pool)
import Data.Token as Token
import Data.Uint as Uint exposing (Uint)
import Json.Encode as Encode exposing (Value)
import Time exposing (Posix)


type WriteBorrow
    = GivenPercent BorrowGivenPercent
    | GivenDebt BorrowGivenDebt
    | GivenCollateral BorrowGivenCollateral


type alias BorrowGivenPercent =
    { pool : Pool
    , assetOut : Uint
    , percent : Percent
    , maxDebt : Uint
    , maxCollateral : Uint
    }


type alias BorrowGivenDebt =
    { pool : Pool
    , assetOut : Uint
    , debtIn : Uint
    , maxCollateral : Uint
    }


type alias BorrowGivenCollateral =
    { pool : Pool
    , assetOut : Uint
    , collateralIn : Uint
    , maxDebt : Uint
    }


toPool : WriteBorrow -> Pool
toPool writeBorrow =
    case writeBorrow of
        GivenPercent { pool } ->
            pool

        GivenDebt { pool } ->
            pool

        GivenCollateral { pool } ->
            pool


encode :
    { model | time : Posix, deadline : Deadline }
    -> Address
    -> WriteBorrow
    -> Value
encode model address write =
    case write of
        GivenPercent givenPercent ->
            givenPercent
                |> encodeWritePercent model address

        GivenDebt givenDebt ->
            givenDebt
                |> encodeWriteDebt model address

        GivenCollateral givenCollateral ->
            givenCollateral
                |> encodeWriteCollateral model address


encodeWritePercent :
    { model | time : Posix, deadline : Deadline }
    -> Address
    -> BorrowGivenPercent
    -> Value
encodeWritePercent { time, deadline } address { pool, assetOut, percent, maxDebt, maxCollateral } =
    [ ( "asset", pool.pair |> Pair.toAsset |> Token.encode )
    , ( "collateral", pool.pair |> Pair.toCollateral |> Token.encode )
    , ( "maturity", pool.maturity |> Maturity.encode )
    , ( "assetTo", address |> Address.encode )
    , ( "dueTo", address |> Address.encode )
    , ( "assetOut", assetOut |> Uint.encode )
    , ( "percent", percent |> Percent.encode )
    , ( "maxDebt", maxDebt |> Uint.encode )
    , ( "maxCollateral", maxCollateral |> Uint.encode )
    , ( "deadline", deadline |> Deadline.encodeUnix time )
    ]
        |> Encode.object


encodeWriteDebt :
    { model | time : Posix, deadline : Deadline }
    -> Address
    -> BorrowGivenDebt
    -> Value
encodeWriteDebt { time, deadline } address { pool, assetOut, debtIn, maxCollateral } =
    [ ( "asset", pool.pair |> Pair.toAsset |> Token.encode )
    , ( "collateral", pool.pair |> Pair.toCollateral |> Token.encode )
    , ( "maturity", pool.maturity |> Maturity.encode )
    , ( "assetTo", address |> Address.encode )
    , ( "dueTo", address |> Address.encode )
    , ( "assetOut", assetOut |> Uint.encode )
    , ( "debtIn", debtIn |> Uint.encode )
    , ( "maxCollateral", maxCollateral |> Uint.encode )
    , ( "deadline", deadline |> Deadline.encodeUnix time )
    ]
        |> Encode.object


encodeWriteCollateral :
    { model | time : Posix, deadline : Deadline }
    -> Address
    -> BorrowGivenCollateral
    -> Value
encodeWriteCollateral { time, deadline } address { pool, assetOut, collateralIn, maxDebt } =
    [ ( "asset", pool.pair |> Pair.toAsset |> Token.encode )
    , ( "collateral", pool.pair |> Pair.toCollateral |> Token.encode )
    , ( "maturity", pool.maturity |> Maturity.encode )
    , ( "assetTo", address |> Address.encode )
    , ( "dueTo", address |> Address.encode )
    , ( "assetOut", assetOut |> Uint.encode )
    , ( "collateralIn", collateralIn |> Uint.encode )
    , ( "maxDebt", maxDebt |> Uint.encode )
    , ( "deadline", deadline |> Deadline.encodeUnix time )
    ]
        |> Encode.object
