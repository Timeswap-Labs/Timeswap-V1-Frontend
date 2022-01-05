module Page.Positions.Liqs.Tooltip exposing (Tooltip(..))

import Data.Pool exposing (Pool)


type Tooltip
    = Symbol Pool
    | Maturity Pool
