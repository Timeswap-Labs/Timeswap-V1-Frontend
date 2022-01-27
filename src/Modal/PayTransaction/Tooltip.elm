module Modal.PayTransaction.Tooltip exposing (Tooltip(..))

import Blockchain.User.TokenId exposing (TokenId)


type Tooltip
    = Debt TokenId
    | Collateral TokenId
    | TextboxToken TokenId
    | Balance TokenId
