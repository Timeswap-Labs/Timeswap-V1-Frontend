module Page.Transaction.Lend.Query exposing (givenBond, givenInsurance, givenPercent)

import Data.Chain as Chain exposing (Chain)
import Data.Percent as Percent exposing (Percent)
import Data.Pool as Pool exposing (Pool)
import Data.Slippage as Slippage exposing (Slippage)
import Data.Uint as Uint exposing (Uint)
import Json.Encode as Encode exposing (Value)
import Page.Transaction.PoolInfo as PoolInfo exposing (PoolInfo)


type Query
    = Query


type alias QueryPercent =
    { chainId : Chain
    , pool : Pool
    , poolInfo : PoolInfo
    , assetIn : Uint
    , percent : Percent
    , slippage : Slippage
    }


type alias QueryBond =
    { chainId : Chain
    , pool : Pool
    , poolInfo : PoolInfo
    , assetIn : Uint
    , bondOut : Uint
    , slippage : Slippage
    }


type alias QueryInsurance =
    { chainId : Chain
    , pool : Pool
    , poolInfo : PoolInfo
    , assetIn : Uint
    , insuranceOut : Uint
    , slippage : Slippage
    }


givenPercent : QueryPercent -> Value
givenPercent { chainId, pool, poolInfo, assetIn, percent, slippage } =
    [ ( "chainId", chainId |> Chain.encode )
    , ( "pool", pool |> Pool.encode )
    , ( "poolInfo", poolInfo |> PoolInfo.encode )
    , ( "assetIn", assetIn |> Uint.encode )
    , ( "percent", percent |> Percent.encode )
    , ( "slippage", slippage |> Slippage.encodeGivenPercent )
    ]
        |> Encode.object


givenBond : QueryBond -> Value
givenBond { chainId, pool, poolInfo, assetIn, bondOut, slippage } =
    [ ( "chainId", chainId |> Chain.encode )
    , ( "pool", pool |> Pool.encode )
    , ( "poolInfo", poolInfo |> PoolInfo.encode )
    , ( "assetIn", assetIn |> Uint.encode )
    , ( "bondOut", bondOut |> Uint.encode )
    , ( "slippage", slippage |> Slippage.encode )
    ]
        |> Encode.object


givenInsurance : QueryInsurance -> Value
givenInsurance { chainId, pool, poolInfo, assetIn, insuranceOut, slippage } =
    [ ( "chainId", chainId |> Chain.encode )
    , ( "pool", pool |> Pool.encode )
    , ( "poolInfo", poolInfo |> PoolInfo.encode )
    , ( "assetIn", assetIn |> Uint.encode )
    , ( "insuranceOut", insuranceOut |> Uint.encode )
    , ( "slippage", slippage |> Slippage.encode )
    ]
        |> Encode.object
