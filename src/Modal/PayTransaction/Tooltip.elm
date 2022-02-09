module Modal.PayTransaction.Tooltip exposing (Tooltip(..))

import Blockchain.User.TokenId exposing (TokenId)


type Tooltip
    = DebtAmount TokenId
    | DebtSymbol TokenId
    | CollateralAmount TokenId
    | CollateralSymbol TokenId
    | TextboxToken TokenId
    | Balance TokenId
    | TotalDebt
    | TotalCollateral
    | TotalDebtSymbol
    | TotalCollateralSymbol
