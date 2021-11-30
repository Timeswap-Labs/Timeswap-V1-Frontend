module Modal.Confirm.Main exposing (Modal, Msg, init, update, view)

import Data.Backdrop exposing (Backdrop)
import Element
    exposing
        ( Element
        , none
        )
import Utility.Glass as Glass


type Modal
    = Confirm
    | Reject


type Msg
    = Exit


init : Modal
init =
    Confirm


update : Msg -> Modal -> Maybe Modal
update msg modal =
    case ( msg, modal ) of
        ( Exit, _ ) ->
            Nothing


view : { model | backdrop : Backdrop } -> Element Msg
view { backdrop } =
    Glass.outsideModal backdrop
        Exit
        none
