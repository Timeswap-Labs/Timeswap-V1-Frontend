module Modal.PayTransaction.Error exposing (Error(..), toString)


type Error
    = Invalid
    | RepayOverflow
    | SumOverflow


toString : Error -> String
toString error =
    case error of
        Invalid ->
            "Invalid"

        RepayOverflow ->
            "Repay amount higher than Debt"

        SumOverflow ->
            "Sum overflow"
