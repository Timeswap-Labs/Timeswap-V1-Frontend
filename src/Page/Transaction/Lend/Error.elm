module Page.Transaction.Lend.Error exposing (Error)


type Error
    = PrincipalOverflow
    | BondUnderflow
    | BondOverflow
    | InsuranceUnderflow
    | InsuranceOverflow
    | Invalid
