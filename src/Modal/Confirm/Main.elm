module Modal.Confirm.Main exposing (Modal, Msg, init, update, view)

import Data.Backdrop exposing (Backdrop)
import Element
    exposing
        ( Element
        , none
        )
import Utility.Glass as Glass
import Utility.IconButton as IconButton


type Modal
    = Confirming
    | Reject


type Msg
    = Exit


init : Modal
init =
    Confirming


update : Msg -> Modal -> Maybe Modal
update msg modal =
    case ( msg, modal ) of
        ( Exit, _ ) ->
            Nothing


view : { model | backdrop : Backdrop } -> Element Msg
view model =
    Glass.outsideModal model
        { onClick = Exit
        , modal = none
        }
