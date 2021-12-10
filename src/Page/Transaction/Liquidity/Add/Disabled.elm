module Page.Transaction.Liquidity.Add.Disabled exposing (Transaction(..), init)


type Transaction
    = Asset String
    | Debt String
    | Collateral String


init : Transaction
init =
    Asset ""
