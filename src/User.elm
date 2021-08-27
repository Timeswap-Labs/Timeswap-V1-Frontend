module User exposing (User)

import Data.Address exposing (Address)
import Data.Chain exposing (Chain)


type alias User =
    { chain : Chain
    , address : Address
    }
