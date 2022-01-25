module Blockchain.User.WritePay exposing (WritePay, encode, toPool)

import Blockchain.User.TokenId as TokenId exposing (TokenId)
import Data.Address as Address exposing (Address)
import Data.Deadline as Deadline exposing (Deadline)
import Data.Maturity as Maturity
import Data.Pair as Pair
import Data.Pool exposing (Pool)
import Data.Token as Token
import Data.Uint as Uint exposing (Uint)
import Json.Encode as Encode exposing (Value)
import Sort.Dict as Dict exposing (Dict)
import Time exposing (Posix)


type alias WritePay =
    { pool : Pool
    , assetsIn : Dict TokenId Uint
    }


toPool : WritePay -> Pool
toPool { pool } =
    pool


encode :
    { model | time : Posix, deadline : Deadline }
    -> Address
    -> WritePay
    -> Value
encode { time, deadline } address { pool, assetsIn } =
    [ ( "asset", pool.pair |> Pair.toAsset |> Token.encode )
    , ( "collateral", pool.pair |> Pair.toCollateral |> Token.encode )
    , ( "maturity", pool.maturity |> Maturity.encode )
    , ( "collateralTo", address |> Address.encode )
    , ( "ids", assetsIn |> Dict.keys |> Encode.list TokenId.encode )
    , ( "maxAssetsIn", assetsIn |> Dict.values |> Encode.list Uint.encode )
    , ( "deadline", deadline |> Deadline.encodeUnix time )
    ]
        |> Encode.object
