module Blockchain.User.WriteLend exposing (WriteLend(..), encode, toPool)

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


type WriteLend
    = GivenPercent LendGivenPercent
    | GivenBond LendGivenBond
    | GivenInsurance LendGivenInsurance


type alias LendGivenPercent =
    { pool : Pool
    , assetIn : Uint
    , percent : Percent
    , minBond : Uint
    , minInsurance : Uint
    }


type alias LendGivenBond =
    { pool : Pool
    , assetIn : Uint
    , bondOut : Uint
    , minInsurance : Uint
    }


type alias LendGivenInsurance =
    { pool : Pool
    , assetIn : Uint
    , insuranceOut : Uint
    , minBond : Uint
    }


toPool : WriteLend -> Pool
toPool writeLend =
    case writeLend of
        GivenPercent { pool } ->
            pool

        GivenBond { pool } ->
            pool

        GivenInsurance { pool } ->
            pool


encode :
    { model | time : Posix, deadline : Deadline }
    -> Address
    -> WriteLend
    -> Value
encode model address write =
    case write of
        GivenPercent givenPercent ->
            givenPercent
                |> encodeWritePercent model address

        GivenBond givenBond ->
            givenBond
                |> encodeWriteBond model address

        GivenInsurance givenInsurance ->
            givenInsurance
                |> encodeWriteInsurance model address


encodeWritePercent :
    { model | time : Posix, deadline : Deadline }
    -> Address
    -> LendGivenPercent
    -> Value
encodeWritePercent { time, deadline } address { pool, assetIn, percent, minBond, minInsurance } =
    [ ( "asset", pool.pair |> Pair.toAsset |> Token.encode )
    , ( "collateral", pool.pair |> Pair.toCollateral |> Token.encode )
    , ( "maturity", pool.maturity |> Maturity.encode )
    , ( "bondTo", address |> Address.encode )
    , ( "insuranceTo", address |> Address.encode )
    , ( "assetIn", assetIn |> Uint.encode )
    , ( "percent", percent |> Percent.encode )
    , ( "minBond", minBond |> Uint.encode )
    , ( "minInsurance", minInsurance |> Uint.encode )
    , ( "deadline", deadline |> Deadline.encodeUnix time )
    ]
        |> Encode.object


encodeWriteBond :
    { model | time : Posix, deadline : Deadline }
    -> Address
    -> LendGivenBond
    -> Value
encodeWriteBond { time, deadline } address { pool, assetIn, bondOut, minInsurance } =
    [ ( "asset", pool.pair |> Pair.toAsset |> Token.encode )
    , ( "collateral", pool.pair |> Pair.toCollateral |> Token.encode )
    , ( "maturity", pool.maturity |> Maturity.encode )
    , ( "bondTo", address |> Address.encode )
    , ( "insuranceTo", address |> Address.encode )
    , ( "assetIn", assetIn |> Uint.encode )
    , ( "bondOut", bondOut |> Uint.encode )
    , ( "minInsurance", minInsurance |> Uint.encode )
    , ( "deadline", deadline |> Deadline.encodeUnix time )
    ]
        |> Encode.object


encodeWriteInsurance :
    { model | time : Posix, deadline : Deadline }
    -> Address
    -> LendGivenInsurance
    -> Value
encodeWriteInsurance { time, deadline } address { pool, assetIn, insuranceOut, minBond } =
    [ ( "asset", pool.pair |> Pair.toAsset |> Token.encode )
    , ( "collateral", pool.pair |> Pair.toCollateral |> Token.encode )
    , ( "maturity", pool.maturity |> Maturity.encode )
    , ( "bondTo", address |> Address.encode )
    , ( "insuranceTo", address |> Address.encode )
    , ( "assetIn", assetIn |> Uint.encode )
    , ( "insuranceOut", insuranceOut |> Uint.encode )
    , ( "minBond", minBond |> Uint.encode )
    , ( "deadline", deadline |> Deadline.encodeUnix time )
    ]
        |> Encode.object
