module Modal.MaturityList.Summary exposing (Summary)

import Data.CDP exposing (CDP)


type alias Summary =
    { rank : Int
    , apr : Float
    , cdp : CDP
    }
