module Modal.MaturityList.Tooltip exposing (Tooltip(..))

import Data.Maturity exposing (Maturity)


type Tooltip
    = Maturity Maturity
    | CDP
