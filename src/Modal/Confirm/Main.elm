module Modal.Confirm.Main exposing (Modal, Msg, confirm, init, reject, update, view)

import Blockchain.User.Txns.TxnWrite exposing (TxnWrite)
import Data.Backdrop exposing (Backdrop)
import Data.Hash exposing (Hash)
import Data.Images exposing (Images)
import Data.Theme exposing (Theme)
import Element
    exposing
        ( Element
        , centerX
        , centerY
        , column
        , el
        , fill
        , height
        , padding
        , paddingXY
        , px
        , row
        , shrink
        , spacing
        , text
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Modal.Outside as Outside
import Utility.Glass as Glass
import Utility.IconButton as IconButton
import Utility.ThemeColor as ThemeColor


type Modal
    = Modal
        { write : TxnWrite
        , state : State
        }


type State
    = Confirming Int
    | Rejected
    | Confirmed Hash


type Msg
    = Exit


init : Int -> TxnWrite -> Modal
init id txnWrite =
    { write = txnWrite
    , state = Confirming id
    }
        |> Modal


update : Msg -> Maybe Never
update msg =
    case msg of
        Exit ->
            Nothing


confirm : Int -> Hash -> Modal -> Modal
confirm id hash (Modal modal) =
    { modal
        | state =
            case modal.state of
                Confirming currentId ->
                    if id == currentId then
                        Confirmed hash

                    else
                        modal.state

                _ ->
                    modal.state
    }
        |> Modal


reject : Int -> Modal -> Modal
reject id (Modal modal) =
    { modal
        | state =
            case modal.state of
                Confirming currentId ->
                    if id == currentId then
                        Rejected

                    else
                        modal.state

                _ ->
                    modal.state
    }
        |> Modal


view :
    { model | images : Images, backdrop : Backdrop, theme : Theme }
    -> Modal
    -> Element Msg
view ({ backdrop, theme } as model) modal =
    Outside.view model
        { onClick = Exit
        , modal =
            column
                ([ width <| px 375
                 , height shrink
                 , padding 24
                 , centerX
                 , centerY
                 , spacing 16
                 , Border.rounded 8
                 , theme |> ThemeColor.border |> Border.color
                 , Border.width 1
                 ]
                    ++ Glass.background backdrop theme
                )
                [ row
                    [ width fill
                    , height shrink
                    , spacing 16
                    ]
                    [ el
                        [ width shrink
                        , height shrink
                        , centerY
                        , Font.size 18
                        , Font.bold
                        , paddingXY 0 3
                        , theme |> ThemeColor.text |> Font.color
                        ]
                        (text "Transaction Submitted")
                    , IconButton.exit model Exit
                    ]
                ]
        }
