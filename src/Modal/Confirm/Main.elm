module Modal.Confirm.Main exposing (Modal, Msg, confirmed, init, reject, submit, update, view)

import Blockchain.Main as Blockchain exposing (Blockchain)
import Blockchain.User.Txns.TxnWrite exposing (TxnWrite(..))
import Data.Backdrop exposing (Backdrop)
import Data.Hash as Hash exposing (Hash)
import Data.Images exposing (Images)
import Data.Theme as Theme exposing (Theme)
import Element
    exposing
        ( Element
        , centerX
        , centerY
        , column
        , el
        , fill
        , height
        , newTabLink
        , none
        , padding
        , paddingXY
        , paragraph
        , px
        , row
        , shrink
        , spacing
        , text
        , width
        )
import Element.Border as Border
import Element.Font as Font
import Modal.Outside as Outside
import Utility.Glass as Glass
import Utility.IconButton as IconButton
import Utility.Image as Image
import Utility.ThemeColor as ThemeColor


type Modal
    = Modal
        { write : TxnWrite
        , state : State
        }


type State
    = Initiating Int
    | Rejected
    | Submitted Hash
    | Confirmed Hash


type Msg
    = Exit


init : Int -> TxnWrite -> Modal
init id txnWrite =
    { write = txnWrite
    , state = Initiating id
    }
        |> Modal


update : Msg -> Maybe Never
update msg =
    case msg of
        Exit ->
            Nothing


submit : Int -> Hash -> Modal -> Modal
submit id hash (Modal modal) =
    { modal
        | state =
            case modal.state of
                Initiating currentId ->
                    if id == currentId then
                        Submitted hash

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
                Initiating currentId ->
                    if id == currentId then
                        Rejected

                    else
                        modal.state

                _ ->
                    modal.state
    }
        |> Modal


confirmed : Hash -> Modal -> Modal
confirmed hash (Modal modal) =
    { modal
        | state =
            case modal.state of
                Submitted currentHash ->
                    if hash == currentHash then
                        Confirmed hash

                    else
                        modal.state

                _ ->
                    modal.state
    }
        |> Modal


view :
    { model | images : Images, backdrop : Backdrop, theme : Theme }
    -> Blockchain
    -> Modal
    -> Element Msg
view ({ backdrop, theme } as model) blockchain modal =
    Outside.view model
        { onClick = Exit
        , modal =
            column
                ([ width <| px 375
                 , height shrink
                 , centerX
                 , centerY
                 , spacing 16
                 , Border.rounded 8
                 , theme |> ThemeColor.border |> Border.color
                 , Border.width 1
                 ]
                    ++ Glass.background backdrop theme
                )
                [ header model modal
                , body model blockchain modal
                ]
        }


header :
    { model | images : Images, backdrop : Backdrop, theme : Theme }
    -> Modal
    -> Element Msg
header ({ theme } as model) (Modal modal) =
    row
        [ width fill
        , height shrink
        , spacing 16
        , padding 24
        , Border.widthEach
            { top = 0
            , right = 0
            , bottom = 1
            , left = 0
            }
        , theme |> ThemeColor.textboxBorder |> Border.color
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
            (text
                (case modal.state of
                    Initiating _ ->
                        "Awaiting Submission"

                    Rejected ->
                        "Transaction Rejected"

                    Submitted _ ->
                        "Transaction Submitted"

                    Confirmed _ ->
                        "Transaction Confirmed"
                )
            )
        , IconButton.exit model Exit
        ]


body :
    { model | images : Images, backdrop : Backdrop, theme : Theme }
    -> Blockchain
    -> Modal
    -> Element Msg
body ({ images, theme } as model) blockchain (Modal { state, write }) =
    column
        [ width fill
        , centerX
        , centerY
        , Font.center
        , padding 30
        , spacing 24
        ]
        [ images
            |> (case state of
                    Initiating _ ->
                        Image.loadingAnimation

                    Submitted _ ->
                        Image.loading

                    Rejected ->
                        Image.loadingFailed

                    Confirmed _ ->
                        Image.loadingSuccess
               )
                [ width <| px 80, height <| px 80, centerX, centerY, Font.center ]
        , column
            [ width fill
            , centerX
            , Font.center
            , spacing 16
            ]
            [ paragraph
                [ centerX
                , theme |> ThemeColor.text |> Font.color
                , Font.center
                , Font.size 14
                ]
                [ text
                    (case state of
                        Initiating _ ->
                            case write of
                                ApproveAndLend _ ->
                                    "Approve transaction is being initiated"

                                Lend _ ->
                                    "Lend transaction is being initiated"

                                ApproveAndBorrow _ ->
                                    "Approve transaction is being initiated"

                                Borrow _ ->
                                    "Borrow transaction is being initiated"

                                ApproveAndFlashRepay _ ->
                                    "Approve transaction is being initiated"

                                FlashRepay _ ->
                                    "Flash-Repay transaction is being initiated"

                                _ ->
                                    "Transaction is being initiated"

                        Rejected ->
                            case write of
                                ApproveAndLend _ ->
                                    "Approve transaction was not submitted"

                                Lend _ ->
                                    "Lend transaction was not submitted"

                                ApproveAndBorrow _ ->
                                    "Approve transaction was not submitted"

                                Borrow _ ->
                                    "Borrow transaction was not submitted"

                                ApproveAndFlashRepay _ ->
                                    "Approve transaction was not submitted"

                                FlashRepay _ ->
                                    "Flash-Repay transaction was not submitted"

                                _ ->
                                    "Transaction was not submitted"

                        Submitted _ ->
                            case write of
                                ApproveAndLend _ ->
                                    "Approve transaction submitted, waiting for confirmation"

                                Lend _ ->
                                    "Lend transaction submitted, waiting for confirmation"

                                ApproveAndBorrow _ ->
                                    "Approve transaction submitted, waiting for confirmation"

                                Borrow _ ->
                                    "Borrow transaction submitted, waiting for confirmation"

                                ApproveAndFlashRepay _ ->
                                    "Approve transaction submitted, waiting for confirmation"

                                FlashRepay _ ->
                                    "Flash-Repay transaction submitted, waiting for confirmation"

                                _ ->
                                    "Transaction submitted, waiting for confirmation"

                        Confirmed _ ->
                            "Your transaction has been confirmed"
                    )
                ]
            , case state of
                Submitted hash ->
                    txnExplorerLink model blockchain hash

                Confirmed hash ->
                    txnExplorerLink model blockchain hash

                _ ->
                    none
            ]
        ]


txnExplorerLink :
    { model | images : Images, backdrop : Backdrop, theme : Theme }
    -> Blockchain
    -> Hash
    -> Element Msg
txnExplorerLink { images, theme } blockchain hash =
    newTabLink [ width fill, centerX, Font.center ]
        { url = Hash.toUrlString (blockchain |> Blockchain.toChain) hash
        , label =
            row [ spacing 8, centerX, centerY, Font.center ]
                [ el
                    [ Font.size 12
                    , Font.center
                    , centerX
                    , theme |> ThemeColor.actionElemLabel |> Font.color
                    ]
                    (text "View on Explorer")
                , images
                    |> (case theme of
                            Theme.Dark ->
                                Image.link

                            Theme.Light ->
                                Image.linkSecondary
                       )
                        [ width <| px 16, height <| px 16 ]
                ]
        }
