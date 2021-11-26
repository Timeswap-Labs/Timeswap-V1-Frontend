module Modal.Confirm.Main exposing (Modal, Msg, init, update, view)

import Element
    exposing
        ( Element
        , none
        )


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


view : Element Never
view =
    none
