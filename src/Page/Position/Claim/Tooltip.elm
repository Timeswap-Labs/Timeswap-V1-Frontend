module Page.Position.Claim.Tooltip exposing (Tooltip(..))

import Data.TokenParam exposing (TokenParam)


type Tooltip
    = Pair
    | Maturity
    | Symbol TokenParam
    | Amount TokenParam
