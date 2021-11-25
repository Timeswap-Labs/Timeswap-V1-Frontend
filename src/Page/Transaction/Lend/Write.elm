module Page.Transaction.Lend.Write exposing (Write(..), encode)

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
    | GivenBond WriteBond
    | GivenInsurance WriteInsurance


type alias WritePercent =
    { pool : Pool
    , assetIn : Uint
    , percent : Percent
    , minBond : Uint
    , minInsurance : Uint
    }


type alias WriteBond =
    { pool : Pool
    , assetIn : Uint
    , bondOut : Uint
    , minInsurance : Uint
    }


type alias WriteInsurance =
    { pool : Pool
    , assetIn : Uint
    , insuranceOut : Uint
    , minBond : Uint
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

        GivenBond givenBond ->
            givenBond
                |> encodeWriteBond model user

        GivenInsurance givenInsurance ->
            givenInsurance
                |> encodeWriteInsurance model user


encodeWritePercent :
    { model | time : Posix, deadline : Deadline }
    -> User
    -> WritePercent
    -> Value
encodeWritePercent { time, deadline } user { pool, assetIn, percent, minBond, minInsurance } =
    [ ( "asset", pool.pair |> Pair.toAsset |> Token.encode )
    , ( "collateral", pool.pair |> Pair.toCollateral |> Token.encode )
    , ( "maturity", pool.maturity |> Maturity.encode )
    , ( "bondTo", user |> User.toAddress |> Address.encode )
    , ( "insuranceTo", user |> User.toAddress |> Address.encode )
    , ( "assetIn", assetIn |> Uint.encode )
    , ( "percent", percent |> Percent.encode )
    , ( "minBond", minBond |> Uint.encode )
    , ( "minInsurance", minInsurance |> Uint.encode )
    , ( "deadline", deadline |> Deadline.encodeUnix time )
    ]
        |> Encode.object


encodeWriteBond :
    { model | time : Posix, deadline : Deadline }
    -> User
    -> WriteBond
    -> Value
encodeWriteBond { time, deadline } user { pool, assetIn, bondOut, minInsurance } =
    [ ( "asset", pool.pair |> Pair.toAsset |> Token.encode )
    , ( "collateral", pool.pair |> Pair.toCollateral |> Token.encode )
    , ( "maturity", pool.maturity |> Maturity.encode )
    , ( "bondTo", user |> User.toAddress |> Address.encode )
    , ( "insuranceTo", user |> User.toAddress |> Address.encode )
    , ( "assetIn", assetIn |> Uint.encode )
    , ( "bondOut", bondOut |> Uint.encode )
    , ( "minInsurance", minInsurance |> Uint.encode )
    , ( "deadline", deadline |> Deadline.encodeUnix time )
    ]
        |> Encode.object


encodeWriteInsurance :
    { model | time : Posix, deadline : Deadline }
    -> User
    -> WriteInsurance
    -> Value
encodeWriteInsurance { time, deadline } user { pool, assetIn, insuranceOut, minBond } =
    [ ( "asset", pool.pair |> Pair.toAsset |> Token.encode )
    , ( "collateral", pool.pair |> Pair.toCollateral |> Token.encode )
    , ( "maturity", pool.maturity |> Maturity.encode )
    , ( "bondTo", user |> User.toAddress |> Address.encode )
    , ( "insuranceTo", user |> User.toAddress |> Address.encode )
    , ( "assetIn", assetIn |> Uint.encode )
    , ( "insuranceOut", insuranceOut |> Uint.encode )
    , ( "minBond", minBond |> Uint.encode )
    , ( "deadline", deadline |> Deadline.encodeUnix time )
    ]
        |> Encode.object
