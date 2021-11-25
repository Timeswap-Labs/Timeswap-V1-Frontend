module Page.Transaction.State exposing (State(..))

import Page.Transaction.PoolInfo exposing (PoolInfo)


type State transaction mint
    = Active PoolInfo transaction
    | DoesNotExist mint
    | Matured
