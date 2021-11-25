module Page.Transaction.Error exposing (Error)

import Http


type alias Error transaction =
    { error : Http.Error
    , transaction : transaction
    }
