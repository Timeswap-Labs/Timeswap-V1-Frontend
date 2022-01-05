module Blockchain.User.Positions exposing
    ( Positions
    , dummy
    )

import Blockchain.User.Claims exposing (Claims)
import Blockchain.User.Dues exposing (Dues)
import Blockchain.User.Liqs exposing (Liqs)
import Data.Pool as Pool
import Sort.Dict as Dict


type alias Positions =
    { claims : Claims
    , dues : Dues
    , liquidities : Liqs
    }


dummy : Positions
dummy =
    { claims = Dict.empty Pool.sorter
    , dues = Dict.empty Pool.sorter
    , liquidities = Dict.empty Pool.sorter
    }
