module Page.Transaction.Liquidity.AddError exposing (Transaction(..), init)


type Transaction
    = Asset String
    | Debt String
    | Collateral String


init : Transaction
init =
    Asset ""
