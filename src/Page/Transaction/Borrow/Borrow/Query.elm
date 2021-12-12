module Page.Transaction.Borrow.Borrow.Query exposing
    ( givenCollateral
    , givenDebt
    , givenMax
    , givenPercent
    )

import Data.Chain as Chain exposing (Chain)
import Data.Percent as Percent exposing (Percent)
import Data.Pool as Pool exposing (Pool)
import Data.Slippage as Slippage exposing (Slippage)
import Data.Uint as Uint exposing (Uint)
import Json.Encode as Encode exposing (Value)
import Page.Transaction.PoolInfo as PoolInfo exposing (PoolInfo)


type alias QueryPercent =
    { chainId : Chain
    , pool : Pool
    , poolInfo : PoolInfo
    , assetOut : Uint
    , percent : Percent
    , slippage : Slippage
    }


type alias QueryMax =
    { chainId : Chain
    , pool : Pool
    , poolInfo : PoolInfo
    , collateralOut : Uint
    , percent : Percent
    , slippage : Slippage
    }


type alias QueryDebt =
    { chainId : Chain
    , pool : Pool
    , poolInfo : PoolInfo
    , assetOut : Uint
    , debtOut : Uint
    , slippage : Slippage
    }


type alias QueryCollateral =
    { chainId : Chain
    , pool : Pool
    , poolInfo : PoolInfo
    , assetOut : Uint
    , collateralOut : Uint
    , slippage : Slippage
    }


givenPercent : QueryPercent -> Value
givenPercent { chainId, pool, poolInfo, assetOut, percent, slippage } =
    [ ( "chainId", chainId |> Chain.encode )
    , ( "pool", pool |> Pool.encode )
    , ( "poolInfo", poolInfo |> PoolInfo.encode )
    , ( "assetOut", assetOut |> Uint.encode )
    , ( "percent", percent |> Percent.encode )
    , ( "slippage", slippage |> Slippage.encodeGivenPercent )
    ]
        |> Encode.object


givenMax : QueryMax -> Value
givenMax { chainId, pool, poolInfo, collateralOut, percent, slippage } =
    [ ( "chainId", chainId |> Chain.encode )
    , ( "pool", pool |> Pool.encode )
    , ( "poolInfo", poolInfo |> PoolInfo.encode )
    , ( "collateralOut", collateralOut |> Uint.encode )
    , ( "percent", percent |> Percent.encode )
    , ( "slippage", slippage |> Slippage.encodeGivenPercent )
    ]
        |> Encode.object


givenDebt : QueryDebt -> Value
givenDebt { chainId, pool, poolInfo, assetOut, debtOut, slippage } =
    [ ( "chainId", chainId |> Chain.encode )
    , ( "pool", pool |> Pool.encode )
    , ( "poolInfo", poolInfo |> PoolInfo.encode )
    , ( "assetOut", assetOut |> Uint.encode )
    , ( "debtOut", debtOut |> Uint.encode )
    , ( "slippage", slippage |> Slippage.encode )
    ]
        |> Encode.object


givenCollateral : QueryCollateral -> Value
givenCollateral { chainId, pool, poolInfo, assetOut, collateralOut, slippage } =
    [ ( "chainId", chainId |> Chain.encode )
    , ( "pool", pool |> Pool.encode )
    , ( "poolInfo", poolInfo |> PoolInfo.encode )
    , ( "assetOut", assetOut |> Uint.encode )
    , ( "collateralOut", collateralOut |> Uint.encode )
    , ( "slippage", slippage |> Slippage.encode )
    ]
        |> Encode.object
