module Page.Transaction.Borrow.Borrow.Write exposing (Write(..), encode)

import Blockchain.Main as Blockchain exposing (Blockchain)
import Blockchain.User.Main as User exposing (User)
import Data.Address as Address
import Data.Chain as Chain
import Data.Deadline as Deadline exposing (Deadline)
import Data.Maturity as Maturity
import Data.Pair as Pair
import Data.Percent as Percent exposing (Percent)
import Data.Pool exposing (Pool)
import Data.Token as Token
import Data.Uint as Uint exposing (Uint)
import Json.Encode as Encode exposing (Value)
import Time exposing (Posix)


type Write
    = GivenPercent WritePercent
    | GivenDebt WriteDebt
    | GivenCollateral WriteCollateral


type alias WritePercent =
    { pool : Pool
    , assetOut : Uint
    , percent : Percent
    , maxDebt : Uint
    , maxCollateral : Uint
    }


type alias WriteDebt =
    { pool : Pool
    , assetOut : Uint
    , debtIn : Uint
    , maxCollateral : Uint
    }


type alias WriteCollateral =
    { pool : Pool
    , assetOut : Uint
    , collateralIn : Uint
    , maxDebt : Uint
    }


encode :
    { model | time : Posix, deadline : Deadline }
    -> Blockchain
    -> User
    -> Write
    -> Value
encode model blockchain user write =
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
    , ( "send"
      , write |> encodeWrite model user
      )
    ]
        |> Encode.object


encodeWrite :
    { model | time : Posix, deadline : Deadline }
    -> User
    -> Write
    -> Value
encodeWrite model user write =
    case write of
        GivenPercent givenPercent ->
            givenPercent
                |> encodeWritePercent model user

        GivenDebt givenDebt ->
            givenDebt
                |> encodeWriteDebt model user

        GivenCollateral givenCollateral ->
            givenCollateral
                |> encodeWriteCollateral model user


encodeWritePercent :
    { model | time : Posix, deadline : Deadline }
    -> User
    -> WritePercent
    -> Value
encodeWritePercent { time, deadline } user { pool, assetOut, percent, maxDebt, maxCollateral } =
    [ ( "asset", pool.pair |> Pair.toAsset |> Token.encode )
    , ( "collateral", pool.pair |> Pair.toCollateral |> Token.encode )
    , ( "maturity", pool.maturity |> Maturity.encode )
    , ( "assetTo", user |> User.toAddress |> Address.encode )
    , ( "dueTo", user |> User.toAddress |> Address.encode )
    , ( "assetOut", assetOut |> Uint.encode )
    , ( "percent", percent |> Percent.encode )
    , ( "maxDebt", maxDebt |> Uint.encode )
    , ( "maxCollateral", maxCollateral |> Uint.encode )
    , ( "deadline", deadline |> Deadline.encodeUnix time )
    ]
        |> Encode.object


encodeWriteDebt :
    { model | time : Posix, deadline : Deadline }
    -> User
    -> WriteDebt
    -> Value
encodeWriteDebt { time, deadline } user { pool, assetOut, debtIn, maxCollateral } =
    [ ( "asset", pool.pair |> Pair.toAsset |> Token.encode )
    , ( "collateral", pool.pair |> Pair.toCollateral |> Token.encode )
    , ( "maturity", pool.maturity |> Maturity.encode )
    , ( "assetTo", user |> User.toAddress |> Address.encode )
    , ( "dueTo", user |> User.toAddress |> Address.encode )
    , ( "assetOut", assetOut |> Uint.encode )
    , ( "debtIn", debtIn |> Uint.encode )
    , ( "maxCollateral", maxCollateral |> Uint.encode )
    , ( "deadline", deadline |> Deadline.encodeUnix time )
    ]
        |> Encode.object


encodeWriteCollateral :
    { model | time : Posix, deadline : Deadline }
    -> User
    -> WriteCollateral
    -> Value
encodeWriteCollateral { time, deadline } user { pool, assetOut, collateralIn, maxDebt } =
    [ ( "asset", pool.pair |> Pair.toAsset |> Token.encode )
    , ( "collateral", pool.pair |> Pair.toCollateral |> Token.encode )
    , ( "maturity", pool.maturity |> Maturity.encode )
    , ( "assetTo", user |> User.toAddress |> Address.encode )
    , ( "dueTo", user |> User.toAddress |> Address.encode )
    , ( "assetOut", assetOut |> Uint.encode )
    , ( "collateralIn", collateralIn |> Uint.encode )
    , ( "maxDebt", maxDebt |> Uint.encode )
    , ( "deadline", deadline |> Deadline.encodeUnix time )
    ]
        |> Encode.object
