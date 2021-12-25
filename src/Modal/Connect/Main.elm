port module Modal.Connect.Main exposing
    ( Effect(..)
    , Modal
    , Msg
    , init
    , receiveUser
    , subscriptions
    , update
    , view
    )

import Animator exposing (Timeline)
import Blockchain.Main as Blockchain exposing (Blockchain)
import Blockchain.User.Main as User exposing (User)
import Blockchain.User.Txns.Txn as Txn exposing (Txn)
import Blockchain.User.Txns.TxnWrite as TxnWrite
import Data.Address as Address exposing (Address)
import Data.Backdrop exposing (Backdrop)
import Data.ERC20 as ERC20
import Data.Hash as Hash exposing (Hash)
import Data.Images exposing (Images)
import Data.Pool as Pool
import Data.Remote as Remote exposing (Remote(..))
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
import Element.Keyed as Keyed
import Json.Decode as Decode
import Json.Encode exposing (Value)
import Modal.Connect.Error as Error exposing (Error)
import Modal.Connect.Terms as Terms
import Modal.Outside as Outside
import Sort.Set as Set
import Time exposing (Posix)
import Utility.Color as Color
import Utility.Etherscan as Etherscan
import Utility.Glass as Glass
import Utility.IconButton as IconButton
import Utility.Image as Image
import Utility.Loading as Loading


type Modal
    = Wallets
    | Waiting
        { wallet : Wallet
        , error : Remote Error Never
        }
    | Connected


type Msg
    = GoToWallets
    | GoToConnected
    | Connect Wallet
    | TryAgain
    | InstallMetamask
    | CopyAddress Address
    | ClearAll
    | ReceiveNoConnect Value
    | Tick Posix
    | Exit


type Effect
    = ClearTxns


init : Support User.NotSupported Blockchain -> Modal
init param =
    case param of
        Supported blockchain ->
            blockchain
                |> Blockchain.toUser
                |> Maybe.map (\_ -> Connected)
                |> Maybe.withDefault Wallets

        NotSupported _ ->
            Connected


update : Msg -> Modal -> ( Maybe Modal, Cmd Msg, Maybe Effect )
update msg modal =
    case ( msg, modal ) of
        ( GoToWallets, Waiting _ ) ->
            ( Wallets |> Just
            , Cmd.none
            , Nothing
            )

        ( GoToWallets, Connected ) ->
            ( Wallets |> Just
            , Cmd.none
            , Nothing
            )

        ( GoToConnected, Wallets ) ->
            ( Connected |> Just
            , Cmd.none
            , Nothing
            )

        ( Connect wallet, Wallets ) ->
            ( { wallet = wallet
              , error = Remote.loading
              }
                |> Waiting
                |> Just
            , wallet
                |> Wallet.encode
                |> connect
            , Nothing
            )

        ( TryAgain, Waiting waiting ) ->
            ( { waiting | error = Remote.loading }
                |> Waiting
                |> Just
            , waiting.wallet
                |> Wallet.encode
                |> connect
            , Nothing
            )

        ( InstallMetamask, Wallets ) ->
            ( modal |> Just
            , installMetamask ()
            , Nothing
            )

        ( CopyAddress address, _ ) ->
            ( modal |> Just
            , address
                |> Address.toString
                |> copyToClipboard
            , Nothing
            )

        ( ClearAll, Connected ) ->
            ( modal |> Just
            , Cmd.none
            , ClearTxns |> Just
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
            , Nothing
            )

        ( Tick posix, Waiting waiting ) ->
            ( { waiting
                | error = waiting.error |> Remote.update posix
              }
                |> Waiting
                |> Just
            , Cmd.none
            , Nothing
            )

        ( Exit, _ ) ->
            ( Nothing
            , Cmd.none
            , Nothing
            )

        _ ->
            ( modal |> Just
            , Cmd.none
            , Nothing
            )


receiveUser : Modal -> Maybe Modal
receiveUser modal =
    case modal of
        Waiting _ ->
            Nothing

        _ ->
            Just modal


port connect : Value -> Cmd msg


port installMetamask : () -> Cmd msg


port copyToClipboard : String -> Cmd msg



-- NOTE: Wallet exist check along with user denied check


port receiveNoConnect : (Value -> msg) -> Sub msg


subscriptions : Modal -> Sub Msg
subscriptions modal =
    case modal of
        Waiting { error } ->
            [ case error of
                Loading _ ->
                    receiveNoConnect ReceiveNoConnect

                _ ->
                    Sub.none
            , error |> Remote.subscriptions Tick
            ]
                |> Sub.batch

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
    Outside.view model
        { onClick = Exit
        , modal =
            el
                ([ width <| px 375
                 , height shrink
                 , padding 24
                 , centerX
                 , centerY
                 , Border.rounded 8
                 , Border.color Color.transparent100
                 , Border.width 1
                 ]
                    ++ Glass.background backdrop
                )
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
                            Loading timeline ->
                                viewInitializing model waiting timeline

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
        }


viewWallets :
    { model
        | images : Images
        , wallets : Wallets
        , blockchain : Support User.NotSupported Blockchain
    }
    -> Element Msg
viewWallets model =
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
                        [ IconButton.back model GoToConnected
                        , el
                            [ width shrink
                            , height shrink
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
                ++ [ IconButton.exit model Exit
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
    -> Timeline ()
    -> Element Msg
viewInitializing model waiting timeline =
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
            [ IconButton.back model GoToWallets
            , el
                [ width shrink
                , height shrink
                , centerY
                , Font.size 18
                , paddingXY 0 3
                , Font.color Color.light100
                ]
                (text "Connect Wallet")
            , IconButton.exit model Exit
            ]
        , walletWaiting model waiting timeline
        , Terms.view
        ]


viewError :
    { model | images : Images }
    -> { waiting | wallet : Wallet }
    -> Element Msg
viewError model waiting =
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
            [ IconButton.back model GoToWallets
            , el
                [ width shrink
                , height shrink
                , centerY
                , Font.size 18
                , paddingXY 0 3
                , Font.color Color.light100
                ]
                (text "Connect Wallet")
            , IconButton.exit model Exit
            ]
        , walletError model waiting
        , Terms.view
        ]


walletWaiting :
    { model | images : Images }
    -> { waiting | wallet : Wallet }
    -> Timeline ()
    -> Element msg
walletWaiting { images } { wallet } timeline =
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
            (Loading.view timeline)
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
        , Background.color Color.negative100
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
            , height <| px 32
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
        , viewTxns model blockchain user
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
                        |> User.toName
                        |> Maybe.withDefault
                            (user
                                |> User.toAddress
                                |> Address.toStringShort
                            )
                    )
                |> text
            )
        , row
            [ width shrink
            , height shrink
            , centerY
            , spacing 4
            ]
            [ newTabLink
                [ width shrink
                , height shrink
                , centerY
                ]
                { url =
                    user
                        |> Etherscan.fromUser
                            (blockchain |> Blockchain.toChain)
                , label =
                    el
                        [ width shrink
                        , height shrink
                        , padding 4
                        ]
                        (images
                            |> Image.link
                                [ width <| px 16
                                , height <| px 16
                                , centerX
                                , centerY
                                ]
                        )
                }
            , Input.button
                [ width shrink
                , height shrink
                , centerY
                ]
                { onPress =
                    user
                        |> User.toAddress
                        |> CopyAddress
                        |> Just
                , label =
                    images
                        |> Image.copy
                            [ width <| px 20
                            , height <| px 20
                            ]
                }
            ]
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


viewTxns :
    { model | images : Images }
    -> Blockchain
    -> User
    -> Element Msg
viewTxns model blockchain user =
    user
        |> User.toTxnsList
        |> (\list ->
                if list |> List.isEmpty then
                    el
                        [ width shrink
                        , height shrink
                        , Font.size 14
                        , paddingXY 0 3
                        , Font.color Color.transparent300
                        ]
                        (text "No recent transactions...")

                else
                    column
                        [ width fill
                        , height shrink
                        , spacing 16
                        ]
                        [ recentTransactions
                        , Keyed.column
                            [ width fill
                            , height shrink
                            , spacing 16
                            ]
                            (list
                                |> List.map
                                    (\tuple ->
                                        ( tuple
                                            |> Tuple.first
                                            |> Hash.toString
                                        , tuple |> viewTxn model blockchain
                                        )
                                    )
                            )
                        ]
           )


recentTransactions : Element Msg
recentTransactions =
    row
        [ width fill
        , height shrink
        ]
        [ el
            [ width shrink
            , height shrink
            , Font.size 14
            , paddingXY 0 3
            , Font.color Color.transparent300
            ]
            (text "Recent transactions")
        , Input.button
            [ width shrink
            , height shrink
            , alignRight
            ]
            { onPress = Just ClearAll
            , label =
                el
                    [ width shrink
                    , height shrink
                    , Font.size 14
                    , paddingXY 0 3
                    , Font.color Color.warning400
                    ]
                    (text "clear all")
            }
        ]


viewTxn :
    { model | images : Images }
    -> Blockchain
    -> ( Hash, Txn )
    -> Element msg
viewTxn { images } blockchain ( hash, txn ) =
    row
        [ width fill
        , height shrink
        , spacing 14
        ]
        [ case txn.state of
            Txn.Pending ->
                el
                    [ width <| px 40
                    , height <| px 40
                    , Border.rounded 999
                    , Background.color Color.warning100
                    ]
                    none

            Txn.Failed ->
                el
                    [ width <| px 40
                    , height <| px 40
                    , Border.rounded 999
                    , Background.color Color.negative100
                    ]
                    none

            Txn.Success ->
                el
                    [ width <| px 40
                    , height <| px 40
                    , Border.rounded 999
                    , Background.color Color.positive100
                    ]
                    none
        , el
            [ width shrink
            , height shrink
            , centerY
            , Font.size 14
            , paddingXY 0 3
            ]
            ((case txn.write of
                TxnWrite.Approve erc20 ->
                    [ "Approve"
                    , erc20
                        |> ERC20.toSymbol
                        |> String.left 5
                    ]
                        |> String.join " "

                TxnWrite.Lend pool ->
                    [ "Lend to"
                    , pool |> Pool.toString
                    , "pool"
                    ]
                        |> String.join " "

                TxnWrite.Borrow pool ->
                    [ "Borrow from"
                    , pool |> Pool.toString
                    , "pool"
                    ]
                        |> String.join " "

                TxnWrite.Liquidity pool ->
                    [ "Add liquidity to"
                    , pool |> Pool.toString
                    , "pool"
                    ]
                        |> String.join " "

                TxnWrite.Create pool ->
                    [ "Create new"
                    , pool |> Pool.toString
                    , "pool"
                    ]
                        |> String.join " "
             )
                |> text
            )
        , newTabLink
            [ width shrink
            , height shrink
            , alignRight
            , centerY
            ]
            { url =
                hash
                    |> Etherscan.fromHash
                        (blockchain |> Blockchain.toChain)
            , label =
                images
                    |> Image.link
                        [ width <| px 16
                        , height <| px 16
                        ]
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
