module Page.Transaction.State exposing (State(..))

import Page.Transaction.PoolInfo exposing (PoolInfo)


type State transaction create
    = Active
        { poolInfo : PoolInfo
        , transaction : transaction
        }
    | DoesNotExist create
    | Matured
