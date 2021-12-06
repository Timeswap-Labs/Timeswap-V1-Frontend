module Modal.ChooseMaturity.Main exposing (Modal, init)

import Data.Pair exposing (Pair)


type Modal
    = Modal { pair : Pair }


init :
    Pair
    -> Modal
init pair =
    { pair = pair }
        |> Modal
