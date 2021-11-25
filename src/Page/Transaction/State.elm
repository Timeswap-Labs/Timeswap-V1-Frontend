module Page.Transaction.State exposing (State(..))

import Page.Transaction.PoolInfo exposing (PoolInfo)


type State transaction mint
    = Active
        { poolInfo : PoolInfo
        , transaction : transaction
        }
    | DoesNotExist mint
    | Matured
