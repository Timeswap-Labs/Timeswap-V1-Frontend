port module Modal.Connect.Main exposing
    ( Modal
    , Msg
    , init
    , receiveUser
    , subscriptions
    , update
    , view
    )

import Data.Address as Address exposing (Address)
import Data.Backdrop exposing (Backdrop)
import Data.Images exposing (Images)
import Data.Remote exposing (Remote(..))
import Data.Wallet as Wallet exposing (Wallet)
import Data.Wallets exposing (Wallets)
import Element
    exposing
        ( Element
        , alignRight
        , behindContent
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
        , px
        , rotate
        , row
        , shrink
        , spacing
        , text
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Json.Decode as Decode
import Json.Encode exposing (Value)
import Modal.Connect.Error as Error exposing (Error)
import Modal.Connect.Terms as Terms
import Sort.Set as Set
import Utility.Color as Color
import Utility.Glass as Glass
import Utility.Image as Image


type Modal
    = Wallets
    | Waiting
        { wallet : Wallet
        , error : Remote Error Never
        }
    | Connected


type Msg
    = GoToWallets
    | Connect Wallet
    | TryAgain
    | CopyToClipboard Address
    | ReceiveNoConnect Value
    | Exit


init : Modal
init =
    Wallets


update : Msg -> Modal -> ( Maybe Modal, Cmd Msg )
update msg modal =
    case ( msg, modal ) of
        ( GoToWallets, Waiting _ ) ->
            ( Wallets |> Just
            , Cmd.none
            )

        ( GoToWallets, Connected ) ->
            ( Wallets |> Just
            , Cmd.none
            )

        ( Connect wallet, Wallets ) ->
            ( { wallet = wallet
              , error = Loading
              }
                |> Waiting
                |> Just
            , wallet
                |> Wallet.encode
                |> connect
            )

        ( TryAgain, Waiting waiting ) ->
            ( { waiting | error = Loading }
                |> Waiting
                |> Just
            , waiting.wallet
                |> Wallet.encode
                |> connect
            )

        ( CopyToClipboard address, _ ) ->
            ( modal |> Just
            , address
                |> Address.encode
                |> copyToClipboard
            )

        ( ReceiveNoConnect value, Waiting waiting ) ->
            ( (case
                value
                    |> Decode.decodeValue Wallet.decoder
               of
                Ok wallet ->
                    if wallet == waiting.wallet then
                        { waiting | error = Error.NoConnect |> Failure }
                            |> Waiting

                    else
                        modal

                Err _ ->
                    modal
              )
                |> Just
            , Cmd.none
            )

        ( Exit, _ ) ->
            ( Nothing
            , Cmd.none
            )

        _ ->
            ( modal |> Just
            , Cmd.none
            )


receiveUser : Modal -> Maybe Modal
receiveUser modal =
    case modal of
        Connected ->
            Just modal

        _ ->
            Nothing


port connect : Value -> Cmd msg


port copyToClipboard : Value -> Cmd msg


port receiveNoConnect : (Value -> msg) -> Sub msg


subscriptions : Modal -> Sub Msg
subscriptions modal =
    case modal of
        Waiting { error } ->
            case error of
                Loading ->
                    receiveNoConnect ReceiveNoConnect

                _ ->
                    Sub.none

        _ ->
            Sub.none


view :
    { model
        | backdrop : Backdrop
        , images : Images
        , wallets : Wallets
    }
    -> Modal
    -> Element Msg
view ({ backdrop } as model) modal =
    Glass.outsideModal backdrop
        Exit
        (el
            [ width <| px 375
            , height shrink
            , padding 24
            , centerX
            , centerY
            , Glass.background backdrop
            , Border.rounded 8
            , Border.color Color.transparent100
            , Border.width 1
            ]
            (case modal of
                Wallets ->
                    viewWallets model

                Waiting waiting ->
                    viewWaiting model waiting

                _ ->
                    none |> Debug.log "later"
            )
        )


viewWallets : { model | images : Images, wallets : Wallets } -> Element Msg
viewWallets ({ images } as model) =
    column
        [ width fill
        , height shrink
        , spacing 16
        ]
        [ row
            [ width fill
            , height shrink
            ]
            [ el
                [ width shrink
                , height shrink
                , centerY
                , Font.size 18
                , paddingXY 0 3
                , Font.color Color.light100
                ]
                (text "Connect Wallet")
            , Input.button
                [ width shrink
                , height shrink
                , alignRight
                , centerY
                ]
                { onPress = Just Exit
                , label =
                    images
                        |> Image.close
                            [ width <| px 24
                            , height <| px 24
                            ]
                }
            ]
        , metamaskButton model
        , Terms.view
        ]


metamaskButton : { model | images : Images, wallets : Wallets } -> Element Msg
metamaskButton { images, wallets } =
    if Wallet.Metamask |> Set.memberOf wallets then
        Input.button
            [ width fill
            , height <| px 54
            , paddingXY 18 0
            , Background.color Color.primary100
            , Border.rounded 8
            ]
            { onPress = Connect Wallet.Metamask |> Just
            , label =
                row
                    [ width fill
                    , height fill
                    , spacing 8
                    ]
                    [ images
                        |> Image.viewWallet
                            [ width <| px 24
                            , height <| px 24
                            , centerY
                            ]
                            Wallet.Metamask
                    , el
                        [ width shrink
                        , height shrink
                        , centerY
                        , Font.size 16
                        , paddingXY 0 4
                        , Font.color Color.light100
                        ]
                        (Wallet.Metamask
                            |> Wallet.toString
                            |> text
                        )
                    , images
                        |> Image.arrow
                            [ width <| px 24
                            , height <| px 24
                            , alignRight
                            , centerY
                            ]
                    ]
            }

    else
        newTabLink
            [ width fill
            , height <| px 54
            , paddingXY 18 0
            , Background.color Color.primary100
            , Border.rounded 8
            ]
            { url = Wallet.Metamask |> Wallet.toUrlString
            , label =
                row
                    [ width fill
                    , height fill
                    , spacing 8
                    ]
                    [ images
                        |> Image.viewWallet
                            [ width <| px 24
                            , height <| px 24
                            , centerY
                            ]
                            Wallet.Metamask
                    , el
                        [ width shrink
                        , height shrink
                        , centerY
                        , Font.size 16
                        , paddingXY 0 4
                        , Font.color Color.light100
                        ]
                        ([ "Install"
                         , Wallet.Metamask
                            |> Wallet.toString
                         ]
                            |> String.join " "
                            |> text
                        )
                    , images
                        |> Image.link
                            [ width <| px 18
                            , height <| px 18
                            , alignRight
                            , centerY
                            ]
                    ]
            }


viewWaiting :
    { model | images : Images }
    ->
        { wallet : Wallet
        , error : Remote Error Never
        }
    -> Element Msg
viewWaiting ({ images } as model) waiting =
    column
        [ width fill
        , height shrink
        , spacing 16
        ]
        [ row
            [ width fill
            , height shrink
            , spacing 16
            ]
            [ Input.button
                [ width shrink
                , height shrink
                , centerY
                ]
                { onPress = Just GoToWallets
                , label =
                    images
                        |> Image.arrowDown
                            [ width <| px 18
                            , height <| px 18
                            , rotate (pi / 2)
                            ]
                }
            , Input.button
                [ width shrink
                , height shrink
                , alignRight
                , centerY
                ]
                { onPress = Just Exit
                , label =
                    images
                        |> Image.close
                            [ width <| px 24
                            , height <| px 24
                            ]
                }
            ]
        , walletWaiting model waiting
        , case waiting.error of
            Loading ->
                initializing

            _ ->
                noConnectError
        , Terms.view
        ]


walletWaiting :
    { model | images : Images }
    -> { waiting | wallet : Wallet }
    -> Element msg
walletWaiting { images } { wallet } =
    row
        [ width fill
        , height <| px 54
        , paddingXY 18 0
        , spacing 8
        , Background.color Color.primary100
        , Border.rounded 8
        ]
        [ images
            |> Image.viewWallet
                [ width <| px 24
                , height <| px 24
                , centerY
                ]
                wallet
        , el
            [ width shrink
            , height shrink
            , centerY
            , Font.size 16
            , paddingXY 0 4
            , Font.color Color.light100
            ]
            (wallet
                |> Wallet.toString
                |> text
            )
        ]


initializing : Element msg
initializing =
    row
        [ width fill
        , height <| px 54
        , paddingXY 18 0
        , spacing 8
        , Border.width 1
        , Border.color Color.primary100
        , Border.rounded 8
        ]
        [ el
            [ width shrink
            , height shrink
            , centerY
            , Font.size 16
            , paddingXY 0 4
            , Font.color Color.light100
            ]
            (text "Initializing...")
        ]


noConnectError : Element Msg
noConnectError =
    row
        [ width fill
        , height <| px 54
        , paddingXY 18 0
        , spacing 8
        , Border.width 1
        , Border.color Color.negative500
        , Border.rounded 8
        ]
        [ el
            [ width shrink
            , height shrink
            , centerY
            , Font.size 16
            , paddingXY 0 4
            , Font.color Color.negative500
            ]
            (text "Error connecting")
        , Input.button
            [ width shrink
            , height <| px 44
            , paddingXY 12 0
            , Background.color Color.primary100
            , Border.rounded 4
            ]
            { onPress = Just TryAgain
            , label =
                el
                    [ width shrink
                    , height shrink
                    , centerY
                    , Font.size 14
                    , paddingXY 0 3
                    , Font.color Color.light100
                    ]
                    (text "Try Again")
            }
        ]
