module Page.Transaction.Error exposing (Error, State(..))

import Http


type alias Error transaction create =
    { error : Http.Error
    , state : State transaction create
    }


type State transaction create
    = Active transaction
    | DoesNotExist create
