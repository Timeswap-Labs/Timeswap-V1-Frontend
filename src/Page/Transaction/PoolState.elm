module Page.Transaction.PoolState exposing (PoolState(..))

import Page.Transaction.PoolInfo exposing (PoolInfo)


type PoolState transaction create
    = Active
        { poolInfo : PoolInfo
        , transaction : transaction
        }
    | DoesNotExist create
    | Matured
