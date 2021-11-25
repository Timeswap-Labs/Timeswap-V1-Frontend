module Modal.MaturityList.Pools exposing (Pools)

import Data.Maturity exposing (Maturity)
import Modal.MaturityList.Summary exposing (Summary)
import Sort.Dict exposing (Dict)


type alias Pools =
    Dict Maturity Summary
