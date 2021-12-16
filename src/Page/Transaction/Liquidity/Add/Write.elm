module Page.Transaction.Liquidity.Add.Write exposing (Write(..), encode)

import Blockchain.Main as Blockchain exposing (Blockchain)
import Blockchain.User.Main as User exposing (User)
import Data.Address as Address
import Data.Chain as Chain
import Data.Deadline as Deadline exposing (Deadline)
import Data.Maturity as Maturity
import Data.Pair as Pair
import Data.Pool exposing (Pool)
import Data.Token as Token
import Data.Uint as Uint exposing (Uint)
import Json.Encode as Encode exposing (Value)
import Time exposing (Posix)


type Write
    = GivenAsset WriteAsset
    | GivenDebt WriteDebt
    | GivenCollateral WriteCollateral


type alias WriteAsset =
    { pool : Pool
    , assetIn : Uint
    , minLiquidity : Uint
    , maxDebt : Uint
    , maxCollateral : Uint
    }


type alias WriteDebt =
    { pool : Pool
    , debtIn : Uint
    , minLiquidity : Uint
    , maxAsset : Uint
    , maxCollateral : Uint
    }


type alias WriteCollateral =
    { pool : Pool
    , collateralIn : Uint
    , minLiquidity : Uint
    , maxAsset : Uint
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
        GivenAsset givenAsset ->
            givenAsset
                |> encodeWriteAsset model user

        GivenDebt givenDebt ->
            givenDebt
                |> encodeWriteDebt model user

        GivenCollateral givenCollateral ->
            givenCollateral
                |> encodeWriteCollateral model user


encodeWriteAsset :
    { model | time : Posix, deadline : Deadline }
    -> User
    -> WriteAsset
    -> Value
encodeWriteAsset { time, deadline } user { pool, assetIn, minLiquidity, maxDebt, maxCollateral } =
    [ ( "asset", pool.pair |> Pair.toAsset |> Token.encode )
    , ( "collateral", pool.pair |> Pair.toCollateral |> Token.encode )
    , ( "maturity", pool.maturity |> Maturity.encode )
    , ( "liquidityTo", user |> User.toAddress |> Address.encode )
    , ( "dueTo", user |> User.toAddress |> Address.encode )
    , ( "assetIn", assetIn |> Uint.encode )
    , ( "minLiquidity", minLiquidity |> Uint.encode )
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
encodeWriteDebt { time, deadline } user { pool, debtIn, minLiquidity, maxAsset, maxCollateral } =
    [ ( "asset", pool.pair |> Pair.toAsset |> Token.encode )
    , ( "collateral", pool.pair |> Pair.toCollateral |> Token.encode )
    , ( "maturity", pool.maturity |> Maturity.encode )
    , ( "liquidityTo", user |> User.toAddress |> Address.encode )
    , ( "dueTo", user |> User.toAddress |> Address.encode )
    , ( "debtIn", debtIn |> Uint.encode )
    , ( "minLiquidity", minLiquidity |> Uint.encode )
    , ( "maxAsset", maxAsset |> Uint.encode )
    , ( "maxCollateral", maxCollateral |> Uint.encode )
    , ( "deadline", deadline |> Deadline.encodeUnix time )
    ]
        |> Encode.object


encodeWriteCollateral :
    { model | time : Posix, deadline : Deadline }
    -> User
    -> WriteCollateral
    -> Value
encodeWriteCollateral { time, deadline } user { pool, collateralIn, minLiquidity, maxAsset, maxDebt } =
    [ ( "asset", pool.pair |> Pair.toAsset |> Token.encode )
    , ( "collateral", pool.pair |> Pair.toCollateral |> Token.encode )
    , ( "maturity", pool.maturity |> Maturity.encode )
    , ( "liquidityTo", user |> User.toAddress |> Address.encode )
    , ( "dueTo", user |> User.toAddress |> Address.encode )
    , ( "collateralIn", collateralIn |> Uint.encode )
    , ( "minLiquidity", minLiquidity |> Uint.encode )
    , ( "maxAsset", maxAsset |> Uint.encode )
    , ( "maxDebt", maxDebt |> Uint.encode )
    , ( "deadline", deadline |> Deadline.encodeUnix time )
    ]
        |> Encode.object
