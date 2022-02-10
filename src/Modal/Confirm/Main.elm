module Modal.Confirm.Main exposing (Modal, Msg, confirm, init, reject, update, view)

import Blockchain.Main as Blockchain exposing (Blockchain)
import Blockchain.User.Txns.TxnWrite exposing (TxnWrite)
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
        , inFront
        , newTabLink
        , none
        , padding
        , paddingXY
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
                    Confirming _ ->
                        "Awaiting Submission"

                    Rejected ->
                        "Transaction Rejected"

                    Confirmed _ ->
                        "Transaction Submitted"
                )
            )
        , IconButton.exit model Exit
        ]


body :
    { model | images : Images, backdrop : Backdrop, theme : Theme }
    -> Blockchain
    -> Modal
    -> Element Msg
body { images, theme } blockchain (Modal { state }) =
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
                    Rejected ->
                        Image.semiCircleRed

                    _ ->
                        Image.semiCircleGreen
               )
                [ width <| px 80
                , height <| px 80
                , centerX
                , Font.center
                , (images
                    |> (case state of
                            Confirmed hash ->
                                Image.matured

                            Confirming int ->
                                Image.hourglassPrimary

                            Rejected ->
                                Image.error
                       )
                        [ width <| px 36, height <| px 36, centerX, centerY, Font.center ]
                  )
                    |> inFront
                ]
        , column
            [ width fill
            , centerX
            , Font.center
            , spacing 16
            ]
            [ el
                [ centerX
                , theme |> ThemeColor.text |> Font.color
                , Font.center
                , Font.size 14
                ]
                (text
                    (case state of
                        Confirming _ ->
                            "Transaction is being initiated"

                        Rejected ->
                            "Transaction not submitted"

                        Confirmed _ ->
                            "Transaction submitted"
                    )
                )
            , case state of
                Confirmed hash ->
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

                _ ->
                    none
            ]
        ]
