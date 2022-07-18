module Page.Transaction.Tooltip exposing (Tooltip(..))

import Data.TokenParam exposing (TokenParam)


type Tooltip
    = Token TokenParam
    | Maturity
    | LiquidityMedium
