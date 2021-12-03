port module Modal.Connect.Main exposing
    ( Modal
    , Msg
    , init
    , receiveUser
    , subscriptions
    , update
    , view
    )

import Blockchain.Main as Blockchain exposing (Blockchain)
import Blockchain.User.Main as User exposing (User)
import Data.Address as Address exposing (Address)
import Data.Backdrop exposing (Backdrop)
import Data.Images exposing (Images)
import Data.Remote exposing (Remote(..))
import Data.Support exposing (Support(..))
import Data.Wallet as Wallet exposing (Wallet)
import Data.Wallets exposing (Wallets)
import Element
    exposing
        ( Element
        , alignRight
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
import Element.Font as Font
import Element.Input as Input
import Json.Decode as Decode
import Json.Encode exposing (Value)
import Modal.Connect.Error as Error exposing (Error)
import Modal.Connect.Etherscan as Etherscan
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
    | InstallMetamask
    | CopyAddress Address
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

        ( InstallMetamask, Wallets ) ->
            ( modal |> Just
            , installMetamask ()
            )

        ( CopyAddress address, _ ) ->
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


port installMetamask : () -> Cmd msg


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
        , blockchain : Support User.NotSupported Blockchain
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
            (case
                ( case model.blockchain of
                    Supported blockchain ->
                        blockchain
                            |> Ok

                    NotSupported notSupported ->
                        notSupported
                            |> Err
                , modal
                )
             of
                ( _, Wallets ) ->
                    viewWallets model

                ( _, Waiting ({ error } as waiting) ) ->
                    case error of
                        Loading ->
                            viewInitializing model waiting

                        _ ->
                            viewError model waiting

                ( Ok blockchain, Connected ) ->
                    blockchain
                        |> Blockchain.toUser
                        |> Maybe.map
                            (viewConnected model blockchain)
                        |> Maybe.withDefault
                            (viewWallets model)

                ( Err notSupported, Connected ) ->
                    viewNotSupported model notSupported
            )
        )


viewWallets :
    { model
        | images : Images
        , wallets : Wallets
        , blockchain : Support User.NotSupported Blockchain
    }
    -> Element Msg
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
            (((case model.blockchain of
                Supported blockchain ->
                    blockchain
                        |> Blockchain.toUser
                        |> Maybe.map User.toWallet

                NotSupported notSupported ->
                    notSupported
                        |> User.toWalletNotSupported
                        |> Just
              )
                |> Maybe.map
                    (\_ ->
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
                        , el
                            [ width shrink
                            , height shrink
                            , centerX
                            , centerY
                            , Font.size 18
                            , paddingXY 0 3
                            , Font.color Color.light100
                            ]
                            (text "Change Wallet")
                        ]
                    )
                |> Maybe.withDefault
                    [ el
                        [ width shrink
                        , height shrink
                        , centerY
                        , Font.size 18
                        , paddingXY 0 3
                        , Font.color Color.light100
                        ]
                        (text "Connect Wallet")
                    ]
             )
                ++ [ Input.button
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
            )
        , metamaskButton model
        , Terms.view
        ]


metamaskButton :
    { model
        | images : Images
        , wallets : Wallets
        , blockchain : Support User.NotSupported Blockchain
    }
    -> Element Msg
metamaskButton ({ images, wallets } as model) =
    (case model.blockchain of
        Supported blockchain ->
            blockchain
                |> Blockchain.toUser
                |> Maybe.map User.toWallet

        NotSupported notSupported ->
            notSupported
                |> User.toWalletNotSupported
                |> Just
    )
        |> (\wallet ->
                case
                    ( wallet
                    , Wallet.Metamask |> Set.memberOf wallets
                    )
                of
                    ( Just Wallet.Metamask, True ) ->
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
                            , el
                                [ width <| px 8
                                , height <| px 8
                                , alignRight
                                , centerY
                                , Background.color Color.positive500
                                , Border.rounded 4
                                ]
                                none
                            ]

                    ( _, True ) ->
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

                    ( _, False ) ->
                        Input.button
                            [ width fill
                            , height <| px 54
                            , paddingXY 18 0
                            , Background.color Color.primary100
                            , Border.rounded 8
                            ]
                            { onPress = Just InstallMetamask
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
           )


viewInitializing :
    { model | images : Images }
    -> { waiting | wallet : Wallet }
    -> Element Msg
viewInitializing ({ images } as model) waiting =
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
            , el
                [ width shrink
                , height shrink
                , centerX
                , centerY
                , Font.size 18
                , paddingXY 0 3
                , Font.color Color.light100
                ]
                (text "Initializing")
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
        , Terms.view
        ]


viewError :
    { model | images : Images }
    -> { waiting | wallet : Wallet }
    -> Element Msg
viewError ({ images } as model) waiting =
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
            , el
                [ width shrink
                , height shrink
                , centerX
                , centerY
                , Font.size 18
                , paddingXY 0 3
                , Font.color Color.negative500
                ]
                (text "Error")
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
        , walletError model waiting
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
        , el
            [ width shrink
            , height shrink
            , alignRight
            , centerY
            , Font.color Color.light100
            , Font.size 16
            ]
            (text "Loading")
            |> Debug.log "loading symbol"
        ]


walletError :
    { model | images : Images }
    -> { waiting | wallet : Wallet }
    -> Element Msg
walletError { images } { wallet } =
    row
        [ width fill
        , height <| px 54
        , paddingXY 18 0
        , spacing 8
        , Background.color Color.primary100
        , Border.width 1
        , Border.color Color.negative500
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
        , Input.button
            [ width shrink
            , height <| px 44
            , paddingXY 12 0
            , alignRight
            , centerY
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


viewConnected :
    { model | images : Images }
    -> Blockchain
    -> User
    -> Element Msg
viewConnected ({ images } as model) blockchain user =
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
                (text "Account")
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
        , walletConnected model blockchain user
        ]


walletConnected :
    { model | images : Images }
    -> Blockchain
    -> User
    -> Element Msg
walletConnected { images } blockchain user =
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
                (user |> User.toWallet)
        , el
            [ width shrink
            , height shrink
            , centerY
            , Font.size 16
            , paddingXY 0 4
            , Font.color Color.primary500
            ]
            (user
                |> User.toName
                |> Maybe.withDefault
                    (user
                        |> User.toAddress
                        |> Address.toStringShort
                    )
                |> text
            )
        , newTabLink
            [ width shrink
            , height shrink
            , centerY
            ]
            { url =
                user
                    |> User.toAddress
                    |> Etherscan.toUrlString
                        (blockchain |> Blockchain.toChain)
                        (user |> User.toName)
            , label =
                images
                    |> Image.link
                        [ width <| px 16
                        , height <| px 16
                        ]
            }
        , Input.button
            [ width shrink
            , height shrink
            , alignRight
            , centerY
            , Font.color Color.primary500
            , Font.size 14
            , paddingXY 0 3
            ]
            { onPress = Just GoToWallets
            , label = text "Change"
            }
        ]


viewNotSupported :
    { model | images : Images }
    -> User.NotSupported
    -> Element Msg
viewNotSupported ({ images } as model) notSupported =
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
                (text "Not Supported")
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
        , walletNotSupported model notSupported
        ]


walletNotSupported :
    { model | images : Images }
    -> User.NotSupported
    -> Element Msg
walletNotSupported { images } notSupported =
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
                (notSupported
                    |> User.toWalletNotSupported
                )
        , el
            [ width shrink
            , height shrink
            , centerY
            , Font.size 16
            , paddingXY 0 4
            , Font.color Color.primary500
            ]
            (notSupported
                |> User.toAddressNotSupported
                |> Address.toStringShort
                |> text
            )
        , Input.button
            [ width shrink
            , height shrink
            , alignRight
            , centerY
            , Font.color Color.primary500
            , Font.size 14
            , paddingXY 0 3
            ]
            { onPress = Just GoToWallets
            , label = text "Change"
            }
        ]
