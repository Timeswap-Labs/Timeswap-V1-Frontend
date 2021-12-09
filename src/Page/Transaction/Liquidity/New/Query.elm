module Page.Transaction.Liquidity.New.Query exposing (givenNew)

import Data.Chain as Chain exposing (Chain)
import Data.Pool as Pool exposing (Pool)
import Data.Uint as Uint exposing (Uint)
import Json.Encode as Encode exposing (Value)


type alias QueryNew =
    { chainId : Chain
    , pool : Pool
    , assetIn : Uint
    , debtOut : Uint
    , collateralOut : Uint
    }


givenNew : QueryNew -> Value
givenNew { chainId, pool, assetIn, debtOut, collateralOut } =
    [ ( "chainId", chainId |> Chain.encode )
    , ( "pool", pool |> Pool.encode )
    , ( "assetIn", assetIn |> Uint.encode )
    , ( "debtOut", debtOut |> Uint.encode )
    , ( "collateralOut", collateralOut |> Uint.encode )
    ]
        |> Encode.object
