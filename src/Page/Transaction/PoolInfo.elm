module Page.Transaction.PoolInfo exposing (PoolInfo)

import Data.Uint exposing (Uint)


type alias PoolInfo =
    { x : Uint
    , y : Uint
    , z : Uint
    , assetReserve : Uint
    , collateralReserve : Uint
    , totalBond : Uint
    , totalInsurance : Uint
    , totalDebtCreated : Uint
    , totalLiquidity : Uint
    , spot : Maybe Uint
    }
