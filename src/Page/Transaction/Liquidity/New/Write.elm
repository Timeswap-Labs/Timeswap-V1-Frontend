module Page.Transaction.Liquidity.New.Write exposing (encode)

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


type alias Write =
    { pool : Pool
    , assetIn : Uint
    , debtIn : Uint
    , collateralIn : Uint
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
encodeWrite { time, deadline } user { pool, assetIn, debtIn, collateralIn } =
    [ ( "asset", pool.pair |> Pair.toAsset |> Token.encode )
    , ( "collateral", pool.pair |> Pair.toCollateral |> Token.encode )
    , ( "maturity", pool.maturity |> Maturity.encode )
    , ( "liquidityTo", user |> User.toAddress |> Address.encode )
    , ( "dueTo", user |> User.toAddress |> Address.encode )
    , ( "assetIn", assetIn |> Uint.encode )
    , ( "debtIn", debtIn |> Uint.encode )
    , ( "collateralIn", collateralIn |> Uint.encode )
    , ( "deadline", deadline |> Deadline.encodeUnix time )
    ]
        |> Encode.object
