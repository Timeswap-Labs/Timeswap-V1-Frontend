module Page.Info.Tooltip exposing (Tooltip(..))

import Data.Pair exposing (Pair)
import Data.Pool exposing (Pool)


type Tooltip
    = Maturity Pool
    | CDP Pool
    | CDPSymbol Pool
    | PairSymbol Pair
    | AssetLiquidity Pool
    | CollateralLiquidity Pool
