module Modal.TokenList.Tooltip exposing (Tooltip(..))

import Data.Token exposing (Token)


type Tooltip
    = Symbol Token
    | Name Token
    | Amount Token
