module Modal.Confirm.Main exposing (Modal, Msg, confirm, init, reject, update, view)

import Blockchain.User.Txns.TxnWrite exposing (TxnWrite)
import Data.Backdrop exposing (Backdrop)
import Data.Theme exposing (Theme)
import Element
    exposing
        ( Element
        , none
        )
import Modal.Outside as Outside


type Modal
    = Modal
        { write : TxnWrite
        , state : State
        }


type State
    = Confirming
    | Rejected


type Msg
    = Exit


init : TxnWrite -> Modal
init txnWrite =
    { write = txnWrite
    , state = Confirming
    }
        |> Modal


update : Msg -> Maybe Never
update msg =
    case msg of
        Exit ->
            Nothing


confirm : Maybe Never
confirm =
    Nothing


reject : Modal -> Modal
reject (Modal modal) =
    { modal | state = Rejected }
        |> Modal


view : { model | backdrop : Backdrop, theme : Theme } -> Element Msg
view model =
    Outside.view model
        { onClick = Exit
        , modal = none
        }
