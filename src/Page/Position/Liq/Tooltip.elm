module Page.Position.Liq.Tooltip exposing (Tooltip(..))

import Blockchain.User.TokenId exposing (TokenId)
import Data.TokenParam exposing (TokenParam)


type Tooltip
    = Pair
    | Maturity
    | Symbol TokenParam
    | Amount TokenParam
    | TotalDebtSymbol TokenParam
    | TotalCollateralSymbol TokenParam
    | DebtAmount TokenId TokenParam
    | CollateralAmount TokenId TokenParam
    | BorrowPositionInfo
    | FlashRepayDisabled
    | LPShare
    | FlashRepayCaution
