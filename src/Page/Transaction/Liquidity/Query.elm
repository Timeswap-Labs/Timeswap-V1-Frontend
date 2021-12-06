module Page.Transaction.Liquidity.Query exposing
    ( givenAsset
    , givenCollateral
    , givenDebt
    , givenNew
    )

import Data.Chain as Chain exposing (Chain)
import Data.Pool as Pool exposing (Pool)
import Data.Slippage as Slippage exposing (Slippage)
import Data.Uint as Uint exposing (Uint)
import Json.Encode as Encode exposing (Value)
import Page.Transaction.PoolInfo as PoolInfo exposing (PoolInfo)


type alias QueryAsset =
    { chainId : Chain
    , pool : Pool
    , poolInfo : PoolInfo
    , assetIn : Uint
    , slippage : Slippage
    }


type alias QueryDebt =
    { chainId : Chain
    , pool : Pool
    , poolInfo : PoolInfo
    , debtOut : Uint
    , slippage : Slippage
    }


type alias QueryCollateral =
    { chainId : Chain
    , pool : Pool
    , poolInfo : PoolInfo
    , collateralOut : Uint
    , slippage : Slippage
    }


type alias QueryNew =
    { chainId : Chain
    , pool : Pool
    , assetIn : Uint
    , debtOut : Uint
    , collateralOut : Uint
    }


givenAsset : QueryAsset -> Value
givenAsset { chainId, pool, poolInfo, assetIn, slippage } =
    [ ( "chainId", chainId |> Chain.encode )
    , ( "pool", pool |> Pool.encode )
    , ( "poolInfo", poolInfo |> PoolInfo.encode )
    , ( "assetIn", assetIn |> Uint.encode )
    , ( "slippage", slippage |> Slippage.encodeGivenPercent )
    ]
        |> Encode.object


givenDebt : QueryDebt -> Value
givenDebt { chainId, pool, poolInfo, debtOut, slippage } =
    [ ( "chainId", chainId |> Chain.encode )
    , ( "pool", pool |> Pool.encode )
    , ( "poolInfo", poolInfo |> PoolInfo.encode )
    , ( "debtOut", debtOut |> Uint.encode )
    , ( "slippage", slippage |> Slippage.encodeGivenPercent )
    ]
        |> Encode.object


givenCollateral : QueryCollateral -> Value
givenCollateral { chainId, pool, poolInfo, collateralOut, slippage } =
    [ ( "chainId", chainId |> Chain.encode )
    , ( "pool", pool |> Pool.encode )
    , ( "poolInfo", poolInfo |> PoolInfo.encode )
    , ( "collateralOut", collateralOut |> Uint.encode )
    , ( "slippage", slippage |> Slippage.encodeGivenPercent )
    ]
        |> Encode.object


givenNew : QueryNew -> Value
givenNew { chainId, pool, assetIn, debtOut, collateralOut } =
    [ ( "chainId", chainId |> Chain.encode )
    , ( "pool", pool |> Pool.encode )
    , ( "assetIn", assetIn |> Uint.encode )
    , ( "debtOut", debtOut |> Uint.encode )
    , ( "collateralOut", collateralOut |> Uint.encode )
    ]
        |> Encode.object
