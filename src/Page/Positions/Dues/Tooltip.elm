module Page.Positions.Dues.Tooltip exposing (Tooltip(..))

import Data.Pool exposing (Pool)


type Tooltip
    = Symbol Pool
    | Maturity Pool
