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
import Data.Pair as Pair
import Data.Pool as Pool
import Data.Remote as Remote exposing (Remote(..))
import Data.Support exposing (Support(..))
import Data.Theme as Theme exposing (Theme)
import Data.Token as Token
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
        , inFront
        , link
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
import Utility.BlockExplorer as BlockExplorer
import Utility.Color as Color
import Utility.Glass as Glass
import Utility.IconButton as IconButton
import Utility.Image as Image
import Utility.Loading as Loading
import Utility.ThemeColor as ThemeColor


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


port copyToClipboard : String -> Cmd msg


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
        , theme : Theme
    }
    -> Modal
    -> Element Msg
view ({ backdrop, theme } as model) modal =
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
                 , theme |> ThemeColor.border |> Border.color
                 , Border.width 1
                 ]
                    ++ Glass.background backdrop theme
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
        , theme : Theme
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
                            , Font.bold
                            , paddingXY 0 3
                            , model.theme |> ThemeColor.text |> Font.color
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
                        , Font.bold
                        , paddingXY 0 3
                        , model.theme |> ThemeColor.text |> Font.color
                        ]
                        (text "Connect Wallet")
                    ]
             )
                ++ [ IconButton.exit model Exit
                   ]
            )
        , walletList model
        , Terms.view model.theme
        ]


walletList :
    { model
        | images : Images
        , wallets : Wallets
        , blockchain : Support User.NotSupported Blockchain
        , theme : Theme
    }
    -> Element Msg
walletList ({ images, wallets, theme } as model) =
    column [ width fill, spacing 12 ]
        ((case model.blockchain of
            Supported blockchain ->
                blockchain
                    |> Blockchain.toUser
                    |> Maybe.map User.toWallet

            NotSupported notSupported ->
                notSupported
                    |> User.toWalletNotSupported
                    |> Just
         )
            |> (\maybeUserWallet ->
                    (if Wallet.Metamask |> Set.memberOf wallets |> not then
                        [ link
                            [ width fill
                            , height <| px 54
                            , paddingXY 18 0
                            , Background.color Color.primary100
                            , Border.rounded 8
                            ]
                            { url = "https://metamask.io/download/"
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
                                        , theme |> ThemeColor.text |> Font.color
                                        ]
                                        ([ "Install"
                                         , Wallet.Metamask
                                            |> Wallet.toString
                                         ]
                                            |> String.join " "
                                            |> text
                                        )
                                    , images
                                        |> (case theme of
                                                Theme.Dark ->
                                                    Image.link

                                                Theme.Light ->
                                                    Image.linkSecondary
                                           )
                                            [ width <| px 18
                                            , height <| px 18
                                            , alignRight
                                            , centerY
                                            ]
                                    ]
                            }
                        ]

                     else
                        [ none ]
                    )
                        ++ (wallets
                                |> Set.toList
                                |> List.map
                                    (\eachWallet ->
                                        case maybeUserWallet of
                                            Just connectedWallet ->
                                                if eachWallet == connectedWallet then
                                                    row
                                                        [ width fill
                                                        , height <| px 54
                                                        , paddingXY 18 0
                                                        , spacing 8
                                                        , theme |> ThemeColor.btnBackground |> Background.color
                                                        , Border.rounded 8
                                                        ]
                                                        [ images
                                                            |> Image.viewWallet
                                                                [ width <| px 24
                                                                , height <| px 24
                                                                , centerY
                                                                ]
                                                                connectedWallet
                                                        , el
                                                            [ width shrink
                                                            , height shrink
                                                            , centerY
                                                            , Font.size 16
                                                            , paddingXY 0 4
                                                            , theme |> ThemeColor.text |> Font.color
                                                            ]
                                                            (connectedWallet
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

                                                else
                                                    Input.button
                                                        [ width fill
                                                        , height <| px 54
                                                        , paddingXY 18 0
                                                        , Background.color Color.primary100
                                                        , Border.rounded 8
                                                        ]
                                                        { onPress = Connect eachWallet |> Just
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
                                                                        eachWallet
                                                                , el
                                                                    [ width shrink
                                                                    , height shrink
                                                                    , centerY
                                                                    , Font.size 16
                                                                    , paddingXY 0 4
                                                                    , theme |> ThemeColor.text |> Font.color
                                                                    ]
                                                                    (eachWallet
                                                                        |> Wallet.toString
                                                                        |> text
                                                                    )
                                                                , images
                                                                    |> (case theme of
                                                                            Theme.Dark ->
                                                                                Image.arrow

                                                                            Theme.Light ->
                                                                                Image.arrowSecondary
                                                                       )
                                                                        [ width <| px 24
                                                                        , height <| px 24
                                                                        , alignRight
                                                                        , centerY
                                                                        ]
                                                                ]
                                                        }

                                            Nothing ->
                                                Input.button
                                                    [ width fill
                                                    , height <| px 54
                                                    , paddingXY 18 0
                                                    , Background.color Color.primary100
                                                    , Border.rounded 8
                                                    ]
                                                    { onPress = Connect eachWallet |> Just
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
                                                                    eachWallet
                                                            , el
                                                                [ width shrink
                                                                , height shrink
                                                                , centerY
                                                                , Font.size 16
                                                                , paddingXY 0 4
                                                                , theme |> ThemeColor.text |> Font.color
                                                                ]
                                                                (eachWallet
                                                                    |> Wallet.toString
                                                                    |> text
                                                                )
                                                            , images
                                                                |> (case theme of
                                                                        Theme.Dark ->
                                                                            Image.arrow

                                                                        Theme.Light ->
                                                                            Image.arrowSecondary
                                                                   )
                                                                    [ width <| px 24
                                                                    , height <| px 24
                                                                    , alignRight
                                                                    , centerY
                                                                    ]
                                                            ]
                                                    }
                                    )
                           )
               )
        )


viewInitializing :
    { model | images : Images, theme : Theme }
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
                , Font.bold
                , paddingXY 0 3
                , model.theme |> ThemeColor.text |> Font.color
                ]
                (text "Connect Wallet")
            , IconButton.exit model Exit
            ]
        , walletWaiting model waiting timeline
        , Terms.view model.theme
        ]


viewError :
    { model | images : Images, theme : Theme }
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
                , Font.bold
                , paddingXY 0 3
                , model.theme |> ThemeColor.text |> Font.color
                ]
                (text "Connect Wallet")
            , IconButton.exit model Exit
            ]
        , walletError model waiting
        , Terms.view model.theme
        ]


walletWaiting :
    { model | images : Images, theme : Theme }
    -> { waiting | wallet : Wallet }
    -> Timeline ()
    -> Element msg
walletWaiting { images, theme } { wallet } timeline =
    row
        [ width fill
        , height <| px 54
        , paddingXY 18 0
        , spacing 8
        , theme |> ThemeColor.btnBackground |> Background.color
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
            , theme |> ThemeColor.text |> Font.color
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
            , theme |> ThemeColor.text |> Font.color
            , Font.size 16
            ]
            (Loading.view timeline theme)
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
    { model | images : Images, theme : Theme }
    -> Blockchain
    -> User
    -> Element Msg
viewConnected ({ theme } as model) blockchain user =
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
                , Font.bold
                , paddingXY 0 3
                , theme |> ThemeColor.text |> Font.color
                ]
                (text "Account")
            , IconButton.exit model Exit
            ]
        , walletConnected model blockchain user
        , viewTxns model blockchain user
        ]


walletConnected :
    { model | images : Images, theme : Theme }
    -> Blockchain
    -> User
    -> Element Msg
walletConnected { images, theme } blockchain user =
    row
        [ width fill
        , height <| px 54
        , paddingXY 18 0
        , spacing 8
        , theme |> ThemeColor.btnBackground |> Background.color
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
            , theme |> ThemeColor.primaryBtn |> Font.color
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
                        |> BlockExplorer.fromUser
                            (blockchain |> Blockchain.toChain)
                , label =
                    el
                        [ width shrink
                        , height shrink
                        , padding 4
                        ]
                        (images
                            |> (case theme of
                                    Theme.Dark ->
                                        Image.link

                                    Theme.Light ->
                                        Image.linkSecondary
                               )
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
            , theme |> ThemeColor.primaryBtn |> Font.color
            , Font.size 14
            , paddingXY 0 3
            ]
            { onPress = Just GoToWallets
            , label = text "Change"
            }
        ]


viewTxns :
    { model | images : Images, theme : Theme }
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
                        , model.theme |> ThemeColor.textLight |> Font.color
                        ]
                        (text "No recent transactions...")

                else
                    column
                        [ width fill
                        , height shrink
                        , spacing 16
                        ]
                        [ recentTransactions model.theme
                        , Keyed.column
                            [ width fill
                            , height shrink
                            , spacing 16
                            ]
                            (list
                                |> List.take 5
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


recentTransactions : Theme -> Element Msg
recentTransactions theme =
    row
        [ width fill
        , height shrink
        ]
        [ el
            [ width shrink
            , height shrink
            , Font.size 14
            , paddingXY 0 3
            , theme |> ThemeColor.textLight |> Font.color
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
                    , Font.color Color.warning500
                    ]
                    (text "Clear all")
            }
        ]


viewTxn :
    { model | images : Images, theme : Theme }
    -> Blockchain
    -> ( Hash, Txn )
    -> Element msg
viewTxn { images, theme } blockchain ( hash, txn ) =
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
                    , (images
                        |> Image.semiCircleYellow
                            [ width <| px 20
                            , height <| px 20
                            , centerX
                            , centerY
                            , Font.center
                            ]
                      )
                        |> inFront
                    ]
                    none

            Txn.Failed ->
                el
                    [ width <| px 40
                    , height <| px 40
                    , Border.rounded 999
                    , Background.color Color.negative100
                    , (images
                        |> Image.error
                            [ width <| px 20
                            , height <| px 20
                            , centerX
                            , centerY
                            , Font.center
                            ]
                      )
                        |> inFront
                    ]
                    none

            Txn.Success ->
                el
                    [ width <| px 40
                    , height <| px 40
                    , Border.rounded 999
                    , Background.color Color.positive100
                    , (images
                        |> Image.matured
                            [ width <| px 20
                            , height <| px 20
                            , centerX
                            , centerY
                            , Font.center
                            ]
                      )
                        |> inFront
                    ]
                    none
        , paragraph
            [ width fill
            , height shrink
            , centerY
            , Font.size 14
            , theme |> ThemeColor.text |> Font.color
            , paddingXY 0 3
            ]
            [ (case txn.write of
                TxnWrite.Approve erc20 ->
                    [ "Approve"
                    , erc20
                        |> ERC20.toSymbol
                        |> String.left 5
                    ]
                        |> String.join " "

                TxnWrite.Lend pool ->
                    [ "Lend :"
                    , pool |> Pool.toString
                    , "pool"
                    ]
                        |> String.join " "

                TxnWrite.ApproveAndLend pool ->
                    [ "Approve"
                    , pool.pair
                        |> Pair.toAsset
                        |> Token.toSymbol
                        |> String.left 5
                    ]
                        |> String.join " "

                TxnWrite.Borrow pool ->
                    [ "Borrow :"
                    , pool |> Pool.toString
                    , "pool"
                    ]
                        |> String.join " "

                TxnWrite.ApproveAndBorrow pool ->
                    [ "Approve"
                    , pool.pair
                        |> Pair.toCollateral
                        |> Token.toSymbol
                        |> String.left 5
                    ]
                        |> String.join " "

                TxnWrite.Liquidity pool ->
                    [ "Add liq :"
                    , pool |> Pool.toString
                    , "pool"
                    ]
                        |> String.join " "

                TxnWrite.Create pool ->
                    [ "Create :"
                    , pool |> Pool.toString
                    , "pool"
                    ]
                        |> String.join " "

                TxnWrite.Withdraw pool ->
                    [ "Withdraw :"
                    , pool |> Pool.toString
                    , "pool"
                    ]
                        |> String.join " "

                TxnWrite.Pay pool ->
                    [ "Pay :"
                    , pool |> Pool.toString
                    , "pool"
                    ]
                        |> String.join " "

                TxnWrite.Burn pool ->
                    [ "Burn :"
                    , pool |> Pool.toString
                    , "pool"
                    ]
                        |> String.join " "

                TxnWrite.ApproveAndFlashRepay _ ->
                    [ "Approve Flash-Repay" ]
                        |> String.join " "

                TxnWrite.FlashRepay pool ->
                    [ "FlashRepay :"
                    , pool |> Pool.toString
                    , "pool"
                    ]
                        |> String.join " "
              )
                |> text
            ]
        , newTabLink
            [ width shrink
            , height shrink
            , alignRight
            , centerY
            ]
            { url =
                hash
                    |> BlockExplorer.fromHash
                        (blockchain |> Blockchain.toChain)
            , label =
                images
                    |> (case theme of
                            Theme.Dark ->
                                Image.link

                            Theme.Light ->
                                Image.linkSecondary
                       )
                        [ width <| px 16
                        , height <| px 16
                        ]
            }
        ]


viewNotSupported :
    { model | images : Images, theme : Theme }
    -> User.NotSupported
    -> Element Msg
viewNotSupported ({ theme } as model) notSupported =
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
                , Font.bold
                , paddingXY 0 3
                , theme |> ThemeColor.text |> Font.color
                ]
                (text "Not Supported")
            , IconButton.exit model Exit
            ]
        , walletNotSupported model notSupported
        ]


walletNotSupported :
    { model | images : Images, theme : Theme }
    -> User.NotSupported
    -> Element Msg
walletNotSupported { images, theme } notSupported =
    row
        [ width fill
        , height <| px 54
        , paddingXY 18 0
        , spacing 8
        , theme |> ThemeColor.btnBackground |> Background.color
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
            , theme |> ThemeColor.primaryBtn |> Font.color
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
            , theme |> ThemeColor.primaryBtn |> Font.color
            , Font.size 14
            , paddingXY 0 3
            ]
            { onPress = Just GoToWallets
            , label = text "Change"
            }
        ]
