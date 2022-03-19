port module Main exposing (main)

import Animator exposing (Animator, Timeline)
import Animator.Css
import Blockchain.Main as Blockchain exposing (Blockchain)
import Blockchain.User.Main as User
import Blockchain.User.Txns.TxnWrite as TxnWrite
import Blockchain.User.WriteBorrow as WriteBorrow
import Blockchain.User.WriteBurn as WriteBurn
import Blockchain.User.WriteCreate as WriteCreate
import Blockchain.User.WriteLend as WriteLend
import Blockchain.User.WriteLiquidity as WriteLiquidity
import Blockchain.User.WritePay as WritePay
import Blockchain.User.WriteWithdraw as WriteWithdraw
import Browser exposing (Document, UrlRequest(..))
import Browser.Events exposing (Visibility)
import Browser.Navigation as Navigation exposing (Key)
import Data.Address as Address
import Data.Backdrop as Backdrop exposing (Backdrop)
import Data.Chain as Chain
import Data.Chains as Chains exposing (Chains)
import Data.ChosenZone as ChosenZone exposing (ChosenZone)
import Data.Deadline as Deadline exposing (Deadline)
import Data.Device as Device exposing (Device(..))
import Data.Images as Images exposing (Images)
import Data.Offset as Offset exposing (Offset)
import Data.Parameter as Parameter
import Data.PriceFeed as PriceFeed exposing (PriceFeed)
import Data.Slippage as Slippage exposing (Slippage)
import Data.Support exposing (Support(..))
import Data.Tab as Tab exposing (Tab)
import Data.Theme as Theme exposing (Theme)
import Data.Token as Token
import Data.Wallet as Wallet
import Data.Wallets as Wallets exposing (Wallets)
import Data.ZoneName as ZoneName exposing (ZoneName)
import Element
    exposing
        ( Element
        , Option
        , alignBottom
        , alignLeft
        , alignRight
        , behindContent
        , below
        , centerX
        , centerY
        , column
        , el
        , fill
        , focusStyle
        , height
        , html
        , inFront
        , layoutWith
        , link
        , map
        , minimum
        , mouseDown
        , mouseOver
        , moveDown
        , moveLeft
        , moveUp
        , newTabLink
        , noStaticStyleSheet
        , none
        , padding
        , paddingEach
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
import Element.Region as Region
import Html exposing (Html)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode exposing (Value)
import Modal.Main as Modal exposing (Modal)
import Page.Main as Page exposing (Page)
import Page.Route as Route
import Sort.Set as Set
import Task
import Time exposing (Posix)
import Url exposing (Url)
import Utility.Blur as Blur
import Utility.Color as Color
import Utility.Glass as Glass
import Utility.Id as Id
import Utility.Image as Image
import Utility.Length as Length
import Utility.Pointer as Pointer
import Utility.Scroll as Scroll
import Utility.ThemeColor as ThemeColor


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = RequestUrl
        , onUrlChange = ChangeUrl
        }


type alias Model =
    { key : Key
    , url : Url
    , time : Posix
    , offset : Offset
    , zoneName : Maybe ZoneName
    , chosenZone : ChosenZone
    , zoneDropdown : Maybe ()
    , device : Device
    , headerGlass : Timeline Visibility
    , scrollToPositions : Timeline Visibility
    , visibility : Visibility
    , backdrop : Backdrop
    , theme : Theme
    , images : Images
    , slippage : Slippage
    , deadline : Deadline
    , priceFeed : PriceFeed
    , wallets : Wallets
    , chains : Chains
    , blockchain : Support User.NotSupported Blockchain
    , page : Page
    , modal : Timeline (Maybe Modal)
    }


type alias Flags =
    { time : Int
    , offset : Offset.Flag
    , zoneName : ZoneName.Flag
    , chosenZone : ChosenZone.Flag
    , width : Int
    , hasBackdropSupport : Backdrop.Flag
    , theme : Theme.Flag
    , images : Images.Flags
    , tokenImages : Images.Flags
    , chainImages : Images.Flags
    , walletImages : Images.Flags
    , slippage : Slippage.Flag
    , deadline : Deadline.Flag
    , priceFeed : PriceFeed.Flag
    , wallets : Wallets.Flags
    , chains : Chains.Flags
    , user : Maybe User.Flag
    }


type Msg
    = RequestUrl UrlRequest
    | ChangeUrl Url
    | ReceiveTime Posix
    | SwitchZone ChosenZone
    | OpenZoneDropdown
    | CloseZoneDropdown
    | ResizeWindow Int Int
    | Scroll ()
    | ReceiveVisibility Scroll.Visibility
    | ClickScroll
    | ScrollToPositions ()
    | VisibilityChange Visibility
    | SwitchTheme
    | OpenConnect
    | OpenChainList
    | ReceiveMetamaskInstalled ()
    | ReceiveUser Value
    | BlockchainMsg Blockchain.Msg
    | PageMsg Page.Msg
    | ModalMsg Modal.Msg
    | Tick Posix


init : Flags -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    (case ( flags.chains |> Chains.init, flags.user ) of
        ( chains, Just user ) ->
            (case
                ( user |> Blockchain.init chains
                , Blockchain.initDefault chains
                , User.initNotSupported user
                )
             of
                ( Just tuple, _, _ ) ->
                    tuple
                        |> Tuple.mapBoth
                            Supported
                            (Cmd.map BlockchainMsg)

                ( Nothing, _, Just notSupported ) ->
                    ( notSupported |> NotSupported
                    , Cmd.none
                    )

                ( Nothing, tuple, Nothing ) ->
                    tuple
                        |> Tuple.mapBoth
                            Supported
                            (Cmd.map BlockchainMsg)
            )
                |> (\( blockchain, cmd ) ->
                        ( chains
                        , blockchain
                        , cmd
                        )
                   )

        ( chains, Nothing ) ->
            Blockchain.initDefault chains
                |> Tuple.mapBoth
                    Supported
                    (Cmd.map BlockchainMsg)
                |> (\( blockchain, cmd ) ->
                        ( chains
                        , blockchain
                        , cmd
                        )
                   )
    )
        |> (\( chains, blockchain, cmd ) ->
                url
                    |> Page.init
                        { time = flags.time |> Time.millisToPosix
                        , chains = chains
                        , blockchain = blockchain
                        }
                    |> (\( page, pageCmd ) ->
                            { chains = chains
                            , blockchain = blockchain
                            , page = page
                            , cmd =
                                [ cmd
                                , pageCmd |> Cmd.map PageMsg
                                ]
                                    |> Cmd.batch
                            }
                       )
           )
        |> (\{ chains, blockchain, page, cmd } ->
                ( { key = key
                  , url = url
                  , time = flags.time |> Time.millisToPosix
                  , offset = flags.offset |> Offset.init
                  , zoneName = flags.zoneName |> ZoneName.init
                  , chosenZone = flags.chosenZone |> ChosenZone.init
                  , zoneDropdown = Nothing
                  , device = flags.width |> Device.fromWidth
                  , headerGlass = Animator.init Browser.Events.Hidden
                  , scrollToPositions =
                        Animator.init Browser.Events.Hidden

                  -- |> Debug.log "edit"
                  , visibility = Browser.Events.Visible
                  , backdrop = flags.hasBackdropSupport |> Backdrop.init
                  , theme = flags.theme |> Theme.init
                  , images =
                        Images.init
                            { images = flags.images
                            , tokenImages = flags.tokenImages
                            , chainImages = flags.chainImages
                            , walletImages = flags.walletImages
                            }
                  , slippage = flags.slippage |> Slippage.init
                  , deadline = flags.deadline |> Deadline.init
                  , priceFeed = flags.priceFeed |> PriceFeed.init
                  , wallets = flags.wallets |> Wallets.init
                  , chains = chains
                  , blockchain = blockchain
                  , page = page
                  , modal = Animator.init Nothing
                  }
                , [ Time.now |> Task.perform ReceiveTime
                  , Scroll.visibility ReceiveVisibility
                  , cmd
                  ]
                    |> Cmd.batch
                )
           )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RequestUrl (Browser.Internal url) ->
            ( model
            , url
                |> Url.toString
                |> Navigation.pushUrl model.key
            )

        RequestUrl (Browser.External url) ->
            ( model
            , Navigation.load url
            )

        ChangeUrl url ->
            model.page
                |> Page.change model url
                |> Tuple.mapBoth
                    (\page ->
                        { model
                            | url = url
                            , page = page
                            , modal =
                                model.modal
                                    |> Animator.go Animator.quickly Nothing
                        }
                    )
                    (Cmd.map PageMsg)

        ReceiveTime posix ->
            ( { model | time = posix }
            , Cmd.none
            )

        SwitchZone newZone ->
            ( { model | chosenZone = newZone }
            , newZone
                |> ChosenZone.encode
                |> cacheChosenZone
            )

        OpenZoneDropdown ->
            ( { model | zoneDropdown = Just () }
            , Cmd.none
            )

        CloseZoneDropdown ->
            ( { model | zoneDropdown = Nothing }
            , Cmd.none
            )

        ResizeWindow width _ ->
            ( { model | device = width |> Device.fromWidth }
            , Scroll.visibility ReceiveVisibility
            )

        Scroll () ->
            ( model
            , Scroll.visibility ReceiveVisibility
            )

        ReceiveVisibility { headerGlass, scrollToPositions } ->
            ( { model
                | headerGlass =
                    model.headerGlass
                        |> Animator.go Animator.quickly headerGlass
                , scrollToPositions =
                    model.scrollToPositions
                        |> Animator.go Animator.quickly scrollToPositions
              }
            , Cmd.none
            )

        ClickScroll ->
            ( model
            , Scroll.toPositions ScrollToPositions
            )

        ScrollToPositions () ->
            ( model
            , Cmd.none
            )

        VisibilityChange visibility ->
            ( { model | visibility = visibility }
            , Cmd.none
            )

        SwitchTheme ->
            ( { model | theme = model.theme |> Theme.switch }
            , model.theme
                |> Theme.switch
                |> Theme.encode
                |> cacheTheme
            )

        OpenConnect ->
            ( { model
                | modal =
                    model.modal
                        |> Animator.go Animator.quickly
                            (Modal.initConnect model.blockchain |> Just)
              }
            , Cmd.none
            )

        OpenChainList ->
            ( { model
                | modal =
                    model.modal
                        |> Animator.go Animator.quickly
                            (Modal.initChainList |> Just)
              }
            , Cmd.none
            )

        ReceiveMetamaskInstalled () ->
            ( { model | wallets = model.wallets |> Set.insert Wallet.Metamask }
            , Cmd.none
            )

        ReceiveUser value ->
            case model.blockchain of
                Supported blockchain ->
                    case
                        ( blockchain
                            |> Blockchain.receiveUser model value
                        , User.receiveNotSupported value
                        )
                    of
                        ( Just ( block, cmd ), _ ) ->
                            ( { model
                                | blockchain =
                                    block |> Supported
                                , modal =
                                    model.modal
                                        |> Animator.go Animator.quickly
                                            (model.modal
                                                |> Animator.current
                                                |> Maybe.andThen Modal.receiveUser
                                            )
                              }
                            , cmd |> Cmd.map BlockchainMsg
                            )

                        ( Nothing, Just userNotSupported ) ->
                            ( { model
                                | blockchain =
                                    userNotSupported |> NotSupported
                                , modal =
                                    model.modal
                                        |> Animator.go Animator.quickly
                                            (model.modal
                                                |> Animator.current
                                                |> Maybe.andThen Modal.receiveUser
                                            )
                              }
                            , Cmd.none
                            )

                        _ ->
                            ( model
                            , Cmd.none
                            )

                NotSupported _ ->
                    case
                        ( Blockchain.receiveUserInit model value
                        , User.receiveNotSupported value
                        )
                    of
                        ( Just ( block, cmd ), _ ) ->
                            ( { model
                                | blockchain =
                                    block |> Supported
                                , modal =
                                    model.modal
                                        |> Animator.go Animator.quickly
                                            (model.modal
                                                |> Animator.current
                                                |> Maybe.andThen Modal.receiveUser
                                            )
                              }
                            , cmd |> Cmd.map BlockchainMsg
                            )

                        ( Nothing, Just userNotSupported ) ->
                            ( { model
                                | blockchain =
                                    userNotSupported |> NotSupported
                                , modal =
                                    model.modal
                                        |> Animator.go Animator.quickly
                                            (model.modal
                                                |> Animator.current
                                                |> Maybe.andThen Modal.receiveUser
                                            )
                              }
                            , Cmd.none
                            )

                        _ ->
                            ( model
                            , Cmd.none
                            )

        BlockchainMsg blockchainMsg ->
            case model.blockchain of
                Supported blockchain ->
                    blockchain
                        |> Blockchain.update model blockchainMsg
                        |> (\( updated, cmd, maybeEffect ) ->
                                maybeEffect
                                    |> Maybe.map
                                        (\effect ->
                                            { model | blockchain = updated |> Supported }
                                                |> blockchainEffect effect
                                                |> Tuple.mapSecond List.singleton
                                                |> Tuple.mapSecond
                                                    ((::) (cmd |> Cmd.map BlockchainMsg))
                                                |> Tuple.mapSecond Cmd.batch
                                        )
                                    |> Maybe.withDefault
                                        ( { model | blockchain = updated |> Supported }
                                        , cmd |> Cmd.map BlockchainMsg
                                        )
                           )

                _ ->
                    ( model
                    , Cmd.none
                    )

        PageMsg pageMsg ->
            case model.blockchain of
                Supported blockchain ->
                    model.page
                        |> Page.update model blockchain pageMsg
                        |> (\( updated, cmd, maybeEffect ) ->
                                maybeEffect
                                    |> Maybe.map
                                        (\effect ->
                                            { model | page = updated }
                                                |> pageEffects blockchain effect
                                                |> Tuple.mapSecond List.singleton
                                                |> Tuple.mapSecond
                                                    ((::) (cmd |> Cmd.map PageMsg))
                                                |> Tuple.mapSecond Cmd.batch
                                        )
                                    |> Maybe.withDefault
                                        ( { model | page = updated }
                                        , [ cmd |> Cmd.map PageMsg
                                          , Scroll.visibility ReceiveVisibility
                                          ]
                                            |> Cmd.batch
                                        )
                           )

                _ ->
                    ( model
                    , Cmd.none
                    )

        ModalMsg modalMsg ->
            model.modal
                |> Animator.current
                |> Maybe.map (Modal.update model modalMsg)
                |> Maybe.map
                    (\( updated, cmd, maybeEffect ) ->
                        maybeEffect
                            |> Maybe.map
                                (\effect ->
                                    { model
                                        | modal =
                                            model.modal
                                                |> Animator.go Animator.quickly updated
                                    }
                                        |> modalEffects effect
                                        |> Tuple.mapSecond List.singleton
                                        |> Tuple.mapSecond
                                            ((::) (cmd |> Cmd.map ModalMsg))
                                        |> Tuple.mapSecond Cmd.batch
                                )
                            |> Maybe.withDefault
                                ( { model
                                    | modal =
                                        model.modal
                                            |> Animator.go Animator.quickly updated
                                  }
                                , cmd |> Cmd.map ModalMsg
                                )
                    )
                |> Maybe.withDefault
                    ( model
                    , Cmd.none
                    )

        Tick posix ->
            ( model |> Animator.update posix animator
            , Cmd.none
            )


blockchainEffect :
    Blockchain.Effect
    -> Model
    -> ( Model, Cmd Msg )
blockchainEffect effect model =
    case ( effect, model.blockchain ) of
        ( Blockchain.AddERC20s erc20s, Supported blockchain ) ->
            model.chains
                |> Chains.insertAll (blockchain |> Blockchain.toChain) erc20s
                |> (\chains ->
                        ( { model | chains = chains }
                        , chains
                            |> Chains.encodeCustom
                            |> cacheCustom
                        )
                   )

        ( Blockchain.OpenConfirm id txnWrite, Supported _ ) ->
            ( { model
                | modal =
                    model.modal
                        |> Animator.go Animator.quickly
                            (txnWrite
                                |> Modal.initConfirm id
                                |> Just
                            )
              }
            , Cmd.none
            )

        ( Blockchain.ConfirmTxn id hash, Supported _ ) ->
            ( { model
                | modal =
                    model.modal
                        |> Animator.go Animator.quickly
                            (model.modal
                                |> Animator.current
                                |> Maybe.map (Modal.confirm id hash)
                            )
              }
            , Cmd.none
            )

        ( Blockchain.RejectTxn id, Supported _ ) ->
            ( { model
                | modal =
                    model.modal
                        |> Animator.go Animator.quickly
                            (model.modal
                                |> Animator.current
                                |> Maybe.map (Modal.reject id)
                            )
              }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


pageEffects :
    Blockchain
    -> Page.Effect
    -> Model
    -> ( Model, Cmd Msg )
pageEffects blockchain effect model =
    case effect of
        Page.OpenTokenList tokenParam ->
            ( { model
                | modal =
                    model.modal
                        |> Animator.go Animator.quickly
                            (tokenParam
                                |> Modal.initTokenList
                                |> Just
                            )
              }
            , Cmd.none
            )

        Page.OpenMaturityList pair ->
            Modal.initMaturityList blockchain pair
                |> Tuple.mapBoth
                    (\maturityList ->
                        { model
                            | modal =
                                model.modal
                                    |> Animator.go Animator.quickly
                                        (maturityList |> Just)
                        }
                    )
                    (Cmd.map ModalMsg)

        Page.OpenInputMaturity pair ->
            ( { model
                | modal =
                    model.modal
                        |> Animator.go Animator.quickly
                            (Modal.initInputMaturity pair |> Just)
              }
            , Cmd.none
            )

        Page.OpenSettings ->
            ( { model
                | modal =
                    model.modal
                        |> Animator.go Animator.quickly
                            (Modal.initSettings model |> Just)
              }
            , Cmd.none
            )

        Page.OpenConnect ->
            ( { model
                | modal =
                    model.modal
                        |> Animator.go Animator.quickly
                            (Supported blockchain
                                |> Modal.initConnect
                                |> Just
                            )
              }
            , Cmd.none
            )

        Page.InputPool pool ->
            ( model
            , Route.fromTab
                (model.page |> Page.toTab)
                (Parameter.Pool pool |> Just)
                |> Route.toUrlString
                |> Navigation.pushUrl model.key
            )

        Page.OpenPayTransaction pool set ->
            blockchain
                |> Blockchain.toUser
                |> Maybe.map
                    (\user ->
                        Modal.initPayTransaction blockchain user pool set
                    )
                |> Maybe.map
                    (\( newModal, cmd ) ->
                        ( { model
                            | modal =
                                model.modal
                                    |> Animator.go Animator.quickly
                                        (Just newModal)
                          }
                        , cmd |> Cmd.map ModalMsg
                        )
                    )
                |> Maybe.withDefault
                    ( model
                    , Cmd.none
                    )

        Page.Approve erc20 ->
            blockchain
                |> Blockchain.updateApprove erc20
                |> (\( updated, cmd, maybeEffect ) ->
                        maybeEffect
                            |> Maybe.map
                                (\pageEffect ->
                                    { model | blockchain = Supported updated }
                                        |> blockchainEffect pageEffect
                                        |> Tuple.mapSecond List.singleton
                                        |> Tuple.mapSecond
                                            ((::) (cmd |> Cmd.map BlockchainMsg))
                                        |> Tuple.mapSecond Cmd.batch
                                )
                            |> Maybe.withDefault
                                ( { model | blockchain = Supported updated }
                                , cmd |> Cmd.map BlockchainMsg
                                )
                   )

        Page.Lend writeLend ->
            blockchain
                |> Blockchain.updateLend model writeLend
                |> (\( updated, cmd, maybeEffect ) ->
                        maybeEffect
                            |> Maybe.map
                                (\pageEffect ->
                                    { model | blockchain = Supported updated }
                                        |> blockchainEffect pageEffect
                                        |> Tuple.mapSecond List.singleton
                                        |> Tuple.mapSecond
                                            ((::) (cmd |> Cmd.map BlockchainMsg))
                                        |> Tuple.mapSecond Cmd.batch
                                )
                            |> Maybe.withDefault
                                ( { model | blockchain = Supported updated }
                                , cmd |> Cmd.map BlockchainMsg
                                )
                   )

        Page.Borrow writeBorrow ->
            blockchain
                |> Blockchain.updateBorrow model writeBorrow
                |> (\( updated, cmd, maybeEffect ) ->
                        maybeEffect
                            |> Maybe.map
                                (\pageEffect ->
                                    { model | blockchain = Supported updated }
                                        |> blockchainEffect pageEffect
                                        |> Tuple.mapSecond List.singleton
                                        |> Tuple.mapSecond
                                            ((::) (cmd |> Cmd.map BlockchainMsg))
                                        |> Tuple.mapSecond Cmd.batch
                                )
                            |> Maybe.withDefault
                                ( { model | blockchain = Supported updated }
                                , cmd |> Cmd.map BlockchainMsg
                                )
                   )

        Page.Liquidity writeLiquidity ->
            blockchain
                |> Blockchain.updateLiquidity model writeLiquidity
                |> (\( updated, cmd, maybeEffect ) ->
                        maybeEffect
                            |> Maybe.map
                                (\pageEffect ->
                                    { model | blockchain = Supported updated }
                                        |> blockchainEffect pageEffect
                                        |> Tuple.mapSecond List.singleton
                                        |> Tuple.mapSecond
                                            ((::) (cmd |> Cmd.map BlockchainMsg))
                                        |> Tuple.mapSecond Cmd.batch
                                )
                            |> Maybe.withDefault
                                ( { model | blockchain = Supported updated }
                                , cmd |> Cmd.map BlockchainMsg
                                )
                   )

        Page.Create writeCreate ->
            blockchain
                |> Blockchain.updateCreate model writeCreate
                |> (\( updated, cmd, maybeEffect ) ->
                        maybeEffect
                            |> Maybe.map
                                (\pageEffect ->
                                    { model | blockchain = Supported updated }
                                        |> blockchainEffect pageEffect
                                        |> Tuple.mapSecond List.singleton
                                        |> Tuple.mapSecond
                                            ((::) (cmd |> Cmd.map BlockchainMsg))
                                        |> Tuple.mapSecond Cmd.batch
                                )
                            |> Maybe.withDefault
                                ( { model | blockchain = Supported updated }
                                , cmd |> Cmd.map BlockchainMsg
                                )
                   )

        Page.Withdraw writeWithdraw ->
            blockchain
                |> Blockchain.updateWithdraw writeWithdraw
                |> (\( updated, cmd, maybeEffect ) ->
                        maybeEffect
                            |> Maybe.map
                                (\pageEffect ->
                                    { model | blockchain = Supported updated }
                                        |> blockchainEffect pageEffect
                                        |> Tuple.mapSecond List.singleton
                                        |> Tuple.mapSecond
                                            ((::) (cmd |> Cmd.map BlockchainMsg))
                                        |> Tuple.mapSecond Cmd.batch
                                )
                            |> Maybe.withDefault
                                ( { model | blockchain = Supported updated }
                                , cmd |> Cmd.map BlockchainMsg
                                )
                   )

        Page.Burn writeBurn ->
            blockchain
                |> Blockchain.updateBurn writeBurn
                |> (\( updated, cmd, maybeEffect ) ->
                        maybeEffect
                            |> Maybe.map
                                (\pageEffect ->
                                    { model | blockchain = Supported updated }
                                        |> blockchainEffect pageEffect
                                        |> Tuple.mapSecond List.singleton
                                        |> Tuple.mapSecond
                                            ((::) (cmd |> Cmd.map BlockchainMsg))
                                        |> Tuple.mapSecond Cmd.batch
                                )
                            |> Maybe.withDefault
                                ( { model | blockchain = Supported updated }
                                , cmd |> Cmd.map BlockchainMsg
                                )
                   )


modalEffects :
    Modal.Effect
    -> Model
    -> ( Model, Cmd Msg )
modalEffects effect model =
    case effect of
        Modal.ClearTxns ->
            case model.blockchain of
                Supported blockchain ->
                    blockchain
                        |> Blockchain.updateClearTxns
                        |> Tuple.mapBoth
                            (\updated ->
                                { model
                                    | blockchain = Supported updated
                                }
                            )
                            (Cmd.map BlockchainMsg)

                NotSupported _ ->
                    ( model
                    , Cmd.none
                    )

        Modal.UpdateSlippage slippage ->
            ( { model | slippage = slippage }
            , Cmd.none
            )

        Modal.UpdateDeadline deadline ->
            ( { model | deadline = deadline }
            , Cmd.none
            )

        Modal.UpdatePriceFeed priceFeed ->
            ( { model | priceFeed = priceFeed }
            , Cmd.none
            )

        Modal.InputToken tokenParam token ->
            ( model
            , Route.fromTab
                (model.page |> Page.toTab)
                (model.page
                    |> Page.toParameter
                    |> Parameter.inputToken tokenParam token
                    |> Just
                )
                |> Route.toUrlString
                |> Navigation.pushUrl model.key
            )

        Modal.AddERC20 tokenParam erc20 ->
            case model.blockchain of
                Supported blockchain ->
                    ( blockchain |> Blockchain.toChain
                    , blockchain |> Blockchain.updateAddERC20 erc20
                    )
                        |> (\( chain, ( updated, cmd ) ) ->
                                model.chains
                                    |> Chains.insert chain erc20
                                    |> (\chains ->
                                            ( { model
                                                | chains = chains
                                                , blockchain = updated |> Supported
                                              }
                                            , [ cmd |> Cmd.map BlockchainMsg
                                              , Route.fromTab
                                                    (model.page |> Page.toTab)
                                                    (model.page
                                                        |> Page.toParameter
                                                        |> Parameter.inputToken tokenParam
                                                            (erc20 |> Token.ERC20)
                                                        |> Just
                                                    )
                                                    |> Route.toUrlString
                                                    |> Navigation.pushUrl model.key
                                              , chains
                                                    |> Chains.encodeCustom
                                                    |> cacheCustom
                                              ]
                                                |> Cmd.batch
                                            )
                                       )
                           )

                _ ->
                    ( model, Cmd.none )

        Modal.RemoveERC20 erc20 ->
            case model.blockchain of
                Supported blockchain ->
                    blockchain
                        |> Blockchain.toChain
                        |> (\chain ->
                                model.chains
                                    |> Chains.remove chain erc20
                                    |> (\chains ->
                                            ( { model | chains = chains }
                                            , chains
                                                |> Chains.encodeCustom
                                                |> cacheCustom
                                            )
                                       )
                           )

                _ ->
                    ( model, Cmd.none )

        Modal.RemoveAll ->
            case model.blockchain of
                Supported blockchain ->
                    blockchain
                        |> Blockchain.toChain
                        |> (\chain ->
                                model.chains
                                    |> Chains.removeAll chain
                                    |> (\chains ->
                                            ( { model | chains = chains }
                                            , chains
                                                |> Chains.encodeCustom
                                                |> cacheCustom
                                            )
                                       )
                           )

                _ ->
                    ( model, Cmd.none )

        Modal.InputPool pool ->
            ( model
            , Route.fromTab
                (model.page |> Page.toTab)
                (Parameter.Pool pool |> Just)
                |> Route.toUrlString
                |> Navigation.pushUrl model.key
            )

        Modal.ChangeChain chain ->
            Blockchain.initGivenChain chain
                |> Tuple.mapBoth
                    (\blockchain ->
                        { model | blockchain = Supported blockchain }
                    )
                    (Cmd.map BlockchainMsg)

        Modal.Approve erc20 ->
            case model.blockchain of
                Supported blockchain ->
                    blockchain
                        |> Blockchain.updateApprove erc20
                        |> (\( updated, cmd, maybeEffect ) ->
                                maybeEffect
                                    |> Maybe.map
                                        (\pageEffect ->
                                            { model | blockchain = Supported updated }
                                                |> blockchainEffect pageEffect
                                                |> Tuple.mapSecond List.singleton
                                                |> Tuple.mapSecond
                                                    ((::) (cmd |> Cmd.map BlockchainMsg))
                                                |> Tuple.mapSecond Cmd.batch
                                        )
                                    |> Maybe.withDefault
                                        ( { model | blockchain = Supported updated }
                                        , cmd |> Cmd.map BlockchainMsg
                                        )
                           )

                NotSupported _ ->
                    ( model
                    , Cmd.none
                    )

        Modal.Pay writePay ->
            case model.blockchain of
                Supported blockchain ->
                    blockchain
                        |> Blockchain.updatePay model writePay
                        |> (\( updated, cmd, maybeEffect ) ->
                                maybeEffect
                                    |> Maybe.map
                                        (\pageEffect ->
                                            { model | blockchain = Supported updated }
                                                |> blockchainEffect pageEffect
                                                |> Tuple.mapSecond List.singleton
                                                |> Tuple.mapSecond
                                                    ((::) (cmd |> Cmd.map BlockchainMsg))
                                                |> Tuple.mapSecond Cmd.batch
                                        )
                                    |> Maybe.withDefault
                                        ( { model | blockchain = Supported updated }
                                        , cmd |> Cmd.map BlockchainMsg
                                        )
                           )

                NotSupported _ ->
                    ( model
                    , Cmd.none
                    )


animator : Animator Model
animator =
    Animator.animator
        |> Animator.Css.watching .modal
            (\updated model ->
                { model | modal = updated }
            )
        |> Animator.Css.watching .headerGlass
            (\updated model ->
                { model | headerGlass = updated }
            )
        |> Animator.Css.watching .scrollToPositions
            (\updated model ->
                { model | scrollToPositions = updated }
            )


port cacheChosenZone : Value -> Cmd msg


port cacheTheme : Value -> Cmd msg


port cacheCustom : Value -> Cmd msg


port cacheSettings : Value -> Cmd msg


port scroll : (() -> msg) -> Sub msg


port receiveMetamaskInstalled : (() -> msg) -> Sub msg


port receiveUser : (Value -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    [ Browser.Events.onResize ResizeWindow
    , scroll Scroll
    , Browser.Events.onVisibilityChange VisibilityChange
    , Time.every 1000 ReceiveTime
    , receiveMetamaskInstalled ReceiveMetamaskInstalled
    , receiveUser ReceiveUser
    , case model.blockchain of
        Supported _ ->
            model.page
                |> Page.subscriptions
                |> Sub.map PageMsg

        NotSupported _ ->
            Sub.none
    , model.modal
        |> Animator.current
        |> Maybe.map Modal.subscriptions
        |> (Maybe.map << Sub.map) ModalMsg
        |> Maybe.withDefault Sub.none
    , Animator.toSubscription Tick model animator
    , onClickOutsideDropdown model
    , case model.blockchain of
        Supported blockchain ->
            blockchain
                |> Blockchain.subscriptions
                |> Sub.map BlockchainMsg

        _ ->
            Sub.none
    ]
        |> Sub.batch


onClickOutsideDropdown : { model | zoneDropdown : Maybe () } -> Sub Msg
onClickOutsideDropdown { zoneDropdown } =
    zoneDropdown
        |> Maybe.map
            (\_ ->
                Browser.Events.onClick (Decode.at [ "target", "id" ] decoderOutsideDropdown)
            )
        |> Maybe.withDefault Sub.none


decoderOutsideDropdown : Decoder Msg
decoderOutsideDropdown =
    Decode.string
        |> Decode.andThen
            (\string ->
                if string /= "zone-dropdown" then
                    Decode.succeed CloseZoneDropdown

                else
                    Decode.fail "Its the zone dropdown"
            )


view : Model -> Document Msg
view model =
    { title = "Timeswap"
    , body = [ viewHtml model ]
    }


viewHtml : Model -> Html Msg
viewHtml model =
    layoutWith
        { options = options }
        [ width <| minimum 375 fill
        , height shrink
        , header model |> inFront
        , scrollButton model |> inFront
        , model.modal
            |> fading model
            |> inFront
        , Font.family [ Font.typeface "Supreme" ]
        , (case model.theme of
            Theme.Light ->
                Color.light200

            Theme.Dark ->
                Color.dark500
          )
            |> Background.color
        ]
        (body model)


options : List Option
options =
    [ { borderColor = Nothing
      , backgroundColor = Nothing
      , shadow = Nothing
      }
        |> focusStyle
    ]


fading :
    { model
        | backdrop : Backdrop
        , blockchain : Support User.NotSupported Blockchain
        , chains : Chains
        , chosenZone : ChosenZone
        , device : Device
        , images : Images
        , offset : Offset
        , priceFeed : PriceFeed
        , time : Posix
        , wallets : Wallets
        , theme : Theme
    }
    -> Timeline (Maybe Modal)
    -> Element Msg
fading ({ backdrop } as model) timeline =
    Animator.Css.div timeline
        [ Animator.Css.opacity
            (Maybe.map
                (\_ -> Animator.at 1)
                >> Maybe.withDefault
                    (Animator.at 0)
            )
        ]
        ([ Length.widthFill
         , Length.heightFill
         ]
            ++ (case backdrop of
                    Backdrop.Supported ->
                        Blur.tenHtml

                    Backdrop.NotSupported ->
                        []
               )
            ++ (timeline
                    |> Animator.current
                    |> Maybe.map (\_ -> [])
                    |> Maybe.withDefault
                        [ Pointer.offHtml ]
               )
        )
        [ layoutWith { options = noStaticStyleSheet :: options }
            [ width fill
            , height fill
            , Font.family [ Font.typeface "Supreme" ]
            ]
            (timeline
                |> Animator.current
                |> Maybe.map (Modal.view model)
                |> (Maybe.map << map) ModalMsg
                |> Maybe.withDefault none
            )
        ]
        |> html


header :
    { model
        | offset : Offset
        , zoneName : Maybe ZoneName
        , chosenZone : ChosenZone
        , zoneDropdown : Maybe ()
        , device : Device
        , headerGlass : Timeline Visibility
        , backdrop : Backdrop
        , theme : Theme
        , images : Images
        , blockchain : Support User.NotSupported Blockchain
        , page : Page
    }
    -> Element Msg
header ({ device, backdrop, theme } as model) =
    column
        [ width fill
        , height shrink
        , Pointer.on
        , Animator.Css.div model.headerGlass
            [ Animator.Css.opacity
                (\headerGlass ->
                    case headerGlass of
                        Browser.Events.Visible ->
                            Animator.at 1

                        Browser.Events.Hidden ->
                            Animator.at 0
                )
            ]
            ([ Length.widthFill
             , Length.heightFill
             ]
                ++ (case backdrop of
                        Backdrop.Supported ->
                            Blur.tenHtml

                        Backdrop.NotSupported ->
                            []
                   )
            )
            [ layoutWith { options = noStaticStyleSheet :: options }
                [ width fill
                , height fill
                , Border.widthEach
                    { top = 0
                    , right = 0
                    , bottom = 1
                    , left = 0
                    }
                , theme |> ThemeColor.border |> Border.color
                , (case backdrop of
                    Backdrop.Supported ->
                        theme |> ThemeColor.background

                    Backdrop.NotSupported ->
                        Color.solid
                  )
                    |> Background.color
                ]
                none
            ]
            |> html
            |> behindContent
        ]
        [ warning model
        , row
            [ Region.navigation
            , width fill
            , height <| px 80
            , (case device of
                Desktop ->
                    76

                _ ->
                    12
              )
                |> spacing
            , paddingXY 16 0
            ]
            [ logo model |> map never
            , case device of
                Phone ->
                    none

                _ ->
                    tabs model |> map never
            , row
                [ width shrink
                , height shrink
                , spacing 10
                , alignRight
                , centerY
                ]
                [ chainListButton model
                , connectButton model
                , zoneButton model
                , themeButton model
                ]
            ]
        ]


warning : { model | images : Images, theme : Theme } -> Element msg
warning { images, theme } =
    row
        [ width fill
        , height shrink
        , paddingXY 12 7
        , Background.color Color.negative200
        , spacing 6
        ]
        [ images
            |> (case theme of
                    Theme.Dark ->
                        Image.info

                    Theme.Light ->
                        Image.error
               )
                [ width <| px 20
                , height <| px 20
                , centerX
                , centerY
                ]
        , el
            [ width shrink
            , height shrink
            , centerX
            , centerY
            , Font.size 14
            , paddingXY 0 3
            , theme |> ThemeColor.textError |> Font.color
            ]
            (text "We are in alpha, please exercise caution while using the app.")
        ]


scrollButton :
    { model
        | device : Device
        , backdrop : Backdrop
        , scrollToPositions : Timeline Visibility
        , images : Images
        , theme : Theme
    }
    -> Element Msg
scrollButton ({ backdrop, images, theme } as model) =
    el
        [ width shrink
        , height shrink
        , alignRight
        , alignBottom
        , moveUp 16
        , moveLeft 16
        ]
        (Animator.Css.div model.scrollToPositions
            [ Animator.Css.opacity
                (\scrollToPositions ->
                    case scrollToPositions of
                        Browser.Events.Visible ->
                            Animator.at 1

                        Browser.Events.Hidden ->
                            Animator.at 0
                )
            ]
            (Length.heightFill
                :: (case backdrop of
                        Backdrop.Supported ->
                            Blur.tenHtml

                        Backdrop.NotSupported ->
                            []
                   )
                ++ (case model.scrollToPositions |> Animator.current of
                        Browser.Events.Visible ->
                            []

                        Browser.Events.Hidden ->
                            [ Pointer.offHtml ]
                   )
            )
            [ layoutWith { options = noStaticStyleSheet :: options }
                [ width shrink
                , height shrink
                , Font.family [ Font.typeface "Supreme" ]
                ]
                (Input.button
                    [ width shrink
                    , height <| px 44
                    , paddingXY 12 0
                    , Border.width 1
                    , theme |> ThemeColor.border |> Border.color
                    , Border.rounded 8
                    , (case backdrop of
                        Backdrop.Supported ->
                            theme |> ThemeColor.btnBackground

                        Backdrop.NotSupported ->
                            Color.solid
                      )
                        |> Background.color
                    ]
                    { onPress = Just ClickScroll
                    , label =
                        row
                            [ width shrink
                            , height shrink
                            , centerX
                            , centerY
                            , spacing 8
                            ]
                            [ el
                                [ width shrink
                                , height shrink
                                , centerX
                                , centerY
                                , theme |> ThemeColor.text |> Font.color
                                , Font.size 16
                                ]
                                (text "Your Positions")
                            , images
                                |> (case theme of
                                        Theme.Dark ->
                                            Image.discloser

                                        Theme.Light ->
                                            Image.arrowDownDark
                                   )
                                    [ width <| px 11
                                    , height <| px 7
                                    ]
                            ]
                    }
                )
            ]
            |> html
        )


logo : { model | device : Device, images : Images, theme : Theme } -> Element Never
logo { device, images, theme } =
    newTabLink [ width shrink ]
        { url = "https://timeswap.io/"
        , label =
            row
                [ Region.description "logo"
                , spacing 12
                , centerY
                ]
                [ case device of
                    Desktop ->
                        images
                            |> (case theme of
                                    Theme.Dark ->
                                        Image.logoText

                                    Theme.Light ->
                                        Image.logoTextDark
                               )
                                [ height <| px 32 ]

                    _ ->
                        images
                            |> Image.logoPure
                                [ height <| px 32 ]
                , el
                    [ width shrink
                    , height <| px 22
                    , padding 5
                    , alignLeft
                    , centerY
                    , theme |> ThemeColor.primaryBtn |> Background.color
                    , Border.rounded 4
                    ]
                    (el
                        [ width shrink
                        , height shrink
                        , centerY
                        , Font.bold
                        , Font.size 12
                        , Font.color Color.light100
                        , Font.letterSpacing 1.28
                        ]
                        (text "ALPHA")
                    )
                ]
        }


tabs :
    { model
        | device : Device
        , backdrop : Backdrop
        , page : Page
        , theme : Theme
    }
    -> Element Never
tabs ({ device, backdrop, theme } as model) =
    row
        ([ Region.description "tabs"
         , width shrink
         , height <| px 44
         , case device of
            Phone ->
                centerX

            _ ->
                alignLeft
         , centerY
         , padding 4
         , spacing 4
         , theme |> ThemeColor.border |> Background.color
         , Border.rounded 8
         , Pointer.on
         ]
            ++ (case ( device, backdrop ) of
                    ( Phone, Backdrop.Supported ) ->
                        [ Border.width 1
                        , Border.color Color.transparent100
                        ]
                            ++ Blur.ten

                    _ ->
                        []
               )
        )
        [ tab model Tab.Lend
        , tab model Tab.Borrow
        , tab model Tab.Info

        --, tab model Tab.Liquidity -- Remove liquidity
        ]


tab : { model | device : Device, page : Page, theme : Theme } -> Tab -> Element Never
tab { page, theme } givenTab =
    if (page |> Page.toTab) == givenTab then
        el
            [ width <| px 84
            , height fill
            , theme |> ThemeColor.primaryBtn |> Background.color
            , Border.rounded 4
            ]
            (el
                [ centerX
                , centerY
                , Font.color Color.light100
                , Font.size 16
                , Font.bold
                ]
                (givenTab
                    |> Tab.toString
                    |> text
                )
            )

    else
        link
            [ width <| px 84
            , height fill
            ]
            { url =
                page
                    |> Page.toParameter
                    |> Route.fromTab givenTab
                    |> Route.toUrlString
            , label =
                el
                    [ centerX
                    , centerY
                    , theme |> ThemeColor.textLight |> Font.color
                    , Font.size 16
                    , Font.bold
                    ]
                    (givenTab
                        |> Tab.toString
                        |> text
                    )
            }


chainListButton :
    { model
        | device : Device
        , backdrop : Backdrop
        , images : Images
        , blockchain : Support User.NotSupported Blockchain
        , theme : Theme
    }
    -> Element Msg
chainListButton ({ device, images, theme } as model) =
    Input.button
        [ Region.description "chains button"
        , width shrink
        , height <| px 44
        , paddingXY 12 0
        , theme |> ThemeColor.btnBackground |> Background.color
        , Border.rounded 4
        , mouseDown [ theme |> ThemeColor.btnPressBG |> Background.color ]
        , mouseOver [ theme |> ThemeColor.btnHoverBG |> Background.color ]
        ]
        { onPress = Just OpenChainList
        , label =
            row
                [ centerX
                , centerY
                , paddingXY 0 3
                , spacing 6
                , Font.size 16
                , theme |> ThemeColor.primaryBtn |> Font.color
                ]
                (case model.blockchain of
                    Supported blockchain ->
                        [ images
                            |> Image.viewChain
                                [ width <| px 24
                                , height <| px 24
                                , centerY
                                ]
                                (blockchain
                                    |> Blockchain.toChain
                                )
                        , case device of
                            Desktop ->
                                blockchain
                                    |> Blockchain.toChain
                                    |> Chain.toString
                                    |> text

                            _ ->
                                none
                        ]

                    NotSupported _ ->
                        [ images
                            |> Image.default
                                [ width <| px 24
                                , height <| px 24
                                , centerY
                                ]
                        , case device of
                            Desktop ->
                                "Not Supported" |> text

                            _ ->
                                none
                        ]
                )
        }


connectButton :
    { model
        | device : Device
        , backdrop : Backdrop
        , images : Images
        , blockchain : Support User.NotSupported Blockchain
        , theme : Theme
    }
    -> Element Msg
connectButton ({ device, images, theme } as model) =
    Input.button
        ([ width shrink
         , height <| px 44
         , paddingXY 12 0
         , Border.rounded 4
         ]
            ++ (case model.blockchain of
                    Supported blockchain ->
                        blockchain
                            |> Blockchain.toUser
                            |> Maybe.map
                                (\_ ->
                                    [ Region.description "account button"
                                    , theme |> ThemeColor.btnBackground |> Background.color
                                    , mouseDown [ theme |> ThemeColor.btnPressBG |> Background.color ]
                                    , mouseOver [ theme |> ThemeColor.btnHoverBG |> Background.color ]
                                    ]
                                )
                            |> Maybe.withDefault
                                [ Region.description "connect button"
                                , theme |> ThemeColor.primaryBtn |> Background.color
                                , mouseDown [ theme |> ThemeColor.btnPressBG |> Background.color ]
                                , mouseOver [ Background.color Color.primary400 ]
                                ]

                    NotSupported _ ->
                        [ Region.description "account button"
                        , theme |> ThemeColor.btnBackground |> Background.color
                        , mouseDown [ theme |> ThemeColor.btnPressBG |> Background.color ]
                        , mouseOver [ theme |> ThemeColor.btnHoverBG |> Background.color ]
                        ]
               )
        )
        { onPress = Just OpenConnect
        , label =
            el
                [ centerX
                , centerY
                ]
                (case model.blockchain of
                    Supported blockchain ->
                        blockchain
                            |> Blockchain.toUser
                            |> Maybe.map
                                (\user ->
                                    row
                                        [ centerX
                                        , centerY
                                        , spacing 6
                                        ]
                                        [ images
                                            |> Image.viewWallet
                                                [ width <| px 24
                                                , height <| px 24
                                                , centerY
                                                ]
                                                (user |> User.toWallet)
                                        , case device of
                                            Desktop ->
                                                el
                                                    [ width shrink
                                                    , height shrink
                                                    , Font.size 16
                                                    , theme |> ThemeColor.primaryBtn |> Font.color
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

                                            _ ->
                                                none
                                        , user
                                            |> User.toPendingSize
                                            |> (\size ->
                                                    if size > 0 then
                                                        el
                                                            [ width <| px 24
                                                            , height <| px 24
                                                            , Background.color Color.warning500
                                                            , Border.rounded 999
                                                            ]
                                                            (el
                                                                [ width shrink
                                                                , height shrink
                                                                , centerX
                                                                , centerY
                                                                , Font.color Color.light100
                                                                , Font.size 16
                                                                , paddingXY 0 4
                                                                ]
                                                                (size
                                                                    |> String.fromInt
                                                                    |> text
                                                                )
                                                            )

                                                    else
                                                        none
                                               )
                                        ]
                                )
                            |> Maybe.withDefault
                                (row
                                    [ width shrink
                                    , height shrink
                                    , spacing 6
                                    ]
                                    [ images
                                        |> Image.wallet
                                            [ width <| px 18
                                            , height <| px 18
                                            , centerY
                                            ]
                                    , case device of
                                        Desktop ->
                                            el
                                                [ width shrink
                                                , height shrink
                                                , Font.size 16
                                                , Font.color Color.transparent400
                                                ]
                                                (text "Connect Wallet")

                                        _ ->
                                            none
                                    ]
                                )

                    NotSupported user ->
                        row
                            [ centerX
                            , centerY
                            , spacing 6
                            ]
                            [ images
                                |> Image.viewWallet
                                    [ width <| px 24
                                    , height <| px 24
                                    , centerY
                                    ]
                                    (user |> User.toWalletNotSupported)
                            , case device of
                                Desktop ->
                                    el
                                        [ width shrink
                                        , height shrink
                                        , Font.size 16
                                        , theme |> ThemeColor.primaryBtn |> Font.color
                                        ]
                                        (user
                                            |> User.toAddressNotSupported
                                            |> Address.toStringShort
                                            |> text
                                        )

                                _ ->
                                    none
                            ]
                )
        }


zoneButton :
    { model
        | images : Images
        , offset : Offset
        , zoneName : Maybe ZoneName
        , chosenZone : ChosenZone
        , backdrop : Backdrop
        , zoneDropdown : Maybe ()
        , theme : Theme
    }
    -> Element Msg
zoneButton ({ images, offset, zoneName, chosenZone, zoneDropdown, theme } as model) =
    Input.button
        [ Region.description "zone button"
        , width shrink
        , height <| px 44
        , paddingXY 12 0
        , theme |> ThemeColor.btnBackground |> Background.color
        , Border.rounded 4
        , mouseDown [ theme |> ThemeColor.btnPressBG |> Background.color ]
        , mouseOver [ theme |> ThemeColor.btnHoverBG |> Background.color ]
        , el
            [ width fill
            , height fill
            , Id.is "zone-dropdown"
            ]
            none
            |> inFront
        , (if zoneDropdown == Just () then
            model |> zoneDropdownOptions

           else
            none
          )
            |> below
        ]
        { onPress =
            case zoneDropdown of
                Just () ->
                    Just CloseZoneDropdown

                Nothing ->
                    Just OpenZoneDropdown
        , label =
            row
                [ centerX
                , centerY
                , spacing 8
                , Font.size 16
                , theme |> ThemeColor.primaryBtn |> Font.color
                ]
                [ ChosenZone.toString chosenZone zoneName offset
                    |> text
                , images
                    |> (case theme of
                            Theme.Dark ->
                                Image.discloser

                            Theme.Light ->
                                Image.arrowDownDark
                       )
                        [ width <| px 11, height <| px 7 ]
                ]
        }


zoneDropdownOptions :
    { model
        | offset : Offset
        , zoneName : Maybe ZoneName
        , chosenZone : ChosenZone
        , backdrop : Backdrop
        , theme : Theme
    }
    -> Element Msg
zoneDropdownOptions { offset, zoneName, chosenZone, theme } =
    column
        [ width shrink
        , height shrink
        , moveDown 10
        , paddingXY 0 5
        , theme |> ThemeColor.dropdownBG |> Background.color
        , Border.rounded 4
        , Border.width 1
        , theme |> ThemeColor.border |> Border.color
        , alignRight
        ]
        ([ ChosenZone.Here, ChosenZone.UTC, ChosenZone.Unix ]
            |> List.map
                (\zoneOption ->
                    Input.button
                        [ width fill
                        , height shrink
                        , paddingXY 16 8
                        , theme |> ThemeColor.text |> Font.color
                        , Font.size 14
                        , mouseOver [ theme |> ThemeColor.btnBackground |> Background.color ]
                        ]
                        { onPress =
                            if chosenZone == zoneOption then
                                Nothing

                            else
                                Just (SwitchZone zoneOption)
                        , label = ChosenZone.toString zoneOption zoneName offset |> text
                        }
                )
            |> List.append
                [ el
                    [ width fill
                    , paddingXY 16 8
                    , Font.size 12
                    , theme |> ThemeColor.textLight |> Font.color
                    ]
                    ("Timezone" |> text)
                ]
        )


themeButton :
    { model
        | theme : Theme
        , images : Images
    }
    -> Element Msg
themeButton { theme, images } =
    Input.button
        [ Region.description "theme button"
        , width <| px 44
        , height <| px 44
        , theme |> ThemeColor.btnBackground |> Background.color
        , Border.rounded 8
        , mouseDown [ theme |> ThemeColor.btnPressBG |> Background.color ]
        , mouseOver [ theme |> ThemeColor.btnHoverBG |> Background.color ]
        ]
        { onPress = Just SwitchTheme
        , label =
            el
                [ centerX
                , centerY
                ]
                (case theme of
                    Theme.Light ->
                        images
                            |> Image.moon
                                [ width <| px 24
                                , height <| px 24
                                ]

                    Theme.Dark ->
                        images
                            |> Image.sun
                                [ width <| px 24
                                , height <| px 24
                                ]
                )
        }


body :
    { model
        | time : Posix
        , offset : Offset
        , chosenZone : ChosenZone
        , device : Device
        , backdrop : Backdrop
        , priceFeed : PriceFeed
        , images : Images
        , blockchain : Support User.NotSupported Blockchain
        , page : Page
        , theme : Theme
    }
    -> Element Msg
body ({ device, page } as model) =
    column
        [ Region.mainContent
        , width fill
        , height shrink
        , (case device of
            Desktop ->
                { top = 134
                , right = 80
                , bottom = 80
                , left = 80
                }

            Tablet ->
                { top = 134
                , right = 40
                , bottom = 80
                , left = 40
                }

            Phone ->
                { top = 134
                , right = 0
                , bottom = 80
                , left = 0
                }
          )
            |> paddingEach
        , spacing 20
        ]
        [ case device of
            Phone ->
                tabs model |> map never

            _ ->
                none
        , case model.blockchain of
            Supported blockchain ->
                Page.view model blockchain page |> map PageMsg

            NotSupported _ ->
                notSupportedBody model
        ]


notSupportedBody :
    { model | backdrop : Backdrop, images : Images, theme : Theme }
    -> Element Msg
notSupportedBody { backdrop, images, theme } =
    column
        ([ width <| px 375
         , height shrink
         , spacing 4
         , centerX
         , centerY
         , Border.rounded 8
         , Border.width 1
         , theme |> ThemeColor.border |> Border.color
         ]
            ++ Glass.background backdrop theme
        )
        [ el
            [ width fill
            , height shrink
            , centerX
            , theme |> ThemeColor.text |> Font.color
            , Font.size 18
            , Font.bold
            , Font.center
            , paddingXY 10 24
            , Border.widthEach
                { top = 0
                , right = 0
                , bottom = 1
                , left = 0
                }
            , theme |> ThemeColor.textboxBorder |> Border.color
            ]
            (text "Unsupported Network")
        , column
            [ width fill
            , height shrink
            , spacing 16
            , padding 24
            ]
            [ images
                |> Image.error
                    [ width <| px 30
                    , height <| px 30
                    , centerX
                    ]
            , column [ width fill, spacing 24 ]
                [ paragraph
                    [ width fill
                    , height shrink
                    , Font.center
                    ]
                    [ el
                        [ width shrink
                        , height shrink
                        , Font.size 14
                        , paddingXY 0 3
                        , theme |> ThemeColor.text |> Font.color
                        ]
                        (text "Please switch to a supported network.")
                    ]
                , Input.button
                    [ width shrink
                    , height <| px 44
                    , paddingXY 12 0
                    , centerX
                    , Background.color Color.negative500
                    , Border.rounded 4
                    ]
                    { onPress = Just OpenChainList
                    , label =
                        row
                            [ width shrink
                            , height shrink
                            , Font.color Color.light100
                            , Font.bold
                            , Font.size 16
                            , paddingXY 0 4
                            , centerX
                            , centerY
                            , spacing 8
                            ]
                            [ images
                                |> Image.energy
                                    [ width <| px 20
                                    , height <| px 20
                                    , centerX
                                    ]
                            , text "Switch Network"
                            ]
                    }
                ]
            ]
        ]
