module Blockchain.User.WriteBurn exposing
    ( WriteBurn
    , encode
    , toPool
    )

import Blockchain.User.Liq as Liq exposing (Liq)
import Data.Address as Address exposing (Address)
import Data.Maturity as Maturity
import Data.Pair as Pair
import Data.Pool exposing (Pool)
import Data.Token as Token
import Json.Encode as Encode exposing (Value)


type alias WriteBurn =
    { pool : Pool
    , liquidityIn : Liq
    }


toPool : WriteBurn -> Pool
toPool { pool } =
    pool


encode :
    Address
    -> WriteBurn
    -> Value
encode address { pool, liquidityIn } =
    [ ( "asset", pool.pair |> Pair.toAsset |> Token.encode )
    , ( "collateral", pool.pair |> Pair.toCollateral |> Token.encode )
    , ( "maturity", pool.maturity |> Maturity.encode )
    , ( "assetTo", address |> Address.encode )
    , ( "collateralTo", address |> Address.encode )
    , ( "liquidityIn", liquidityIn |> Liq.encode )
    ]
        |> Encode.object
