module Page.Position.Due.Tooltip exposing (Tooltip(..))

import Blockchain.User.TokenId exposing (TokenId)
import Data.TokenParam exposing (TokenParam)


type Tooltip
    = Pair
    | Maturity
    | Symbol TokenId TokenParam
    | Amount TokenId TokenParam
