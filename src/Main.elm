port module Main exposing (main)

import Blockchain.Main as Blockchain exposing (Blockchain)
import Blockchain.User.Main as User
import Blockchain.User.Txns.TxnWrite as TxnWrite
import Blockchain.User.WriteBorrow as WriteBorrow
import Blockchain.User.WriteCreate as WriteCreate
import Blockchain.User.WriteLend as WriteLend
import Blockchain.User.WriteLiquidity as WriteLiquidity
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
        , alignLeft
        , alignRight
        , centerX
        , centerY
        , column
        , el
        , fill
        , focusStyle
        , height
        , inFront
        , layoutWith
        , link
        , map
        , minimum
        , mouseDown
        , mouseOver
        , none
        , padding
        , paddingEach
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
import Element.Region as Region
import Html exposing (Html)
import Json.Encode exposing (Value)
import Modal.Main as Modal exposing (Modal)
import Page.Main as Page exposing (Page)
import Page.Route as Route
import Sort.Set as Set
import Task
import Time exposing (Posix)
import Url exposing (Url)
import Utility.Color as Color
import Utility.Image as Image


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
    , device : Device
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
    , modal : Maybe Modal
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
    | SwitchZone
    | ResizeWindow Int Int
    | VisibilityChange Visibility
    | SwitchTheme
    | OpenConnect
    | OpenChainList
    | ReceiveMetamaskInstalled ()
    | ReceiveUser Value
    | BlockchainMsg Blockchain.Msg
    | PageMsg Page.Msg
    | ModalMsg Modal.Msg


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
                  , device = flags.width |> Device.fromWidth
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
                  , modal = Nothing
                  }
                , [ Time.now |> Task.perform ReceiveTime
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
                            , modal = Nothing
                        }
                    )
                    (Cmd.map PageMsg)

        ReceiveTime posix ->
            ( { model | time = posix }
            , Cmd.none
            )

        SwitchZone ->
            ( { model | chosenZone = model.chosenZone |> ChosenZone.switch }
            , model.chosenZone
                |> ChosenZone.switch
                |> ChosenZone.encode
                |> cacheChosenZone
            )

        ResizeWindow width _ ->
            ( { model | device = width |> Device.fromWidth }
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
                    Modal.initConnect model.blockchain
                        |> Just
              }
            , Cmd.none
            )

        OpenChainList ->
            ( { model | modal = Modal.initChainList |> Just }
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
                                        |> Maybe.andThen Modal.receiveUser
                              }
                            , cmd |> Cmd.map BlockchainMsg
                            )

                        ( Nothing, Just userNotSupported ) ->
                            ( { model
                                | blockchain =
                                    userNotSupported |> NotSupported
                                , modal =
                                    model.modal
                                        |> Maybe.andThen Modal.receiveUser
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
                                        |> Maybe.andThen Modal.receiveUser
                              }
                            , cmd |> Cmd.map BlockchainMsg
                            )

                        ( Nothing, Just userNotSupported ) ->
                            ( { model
                                | blockchain =
                                    userNotSupported |> NotSupported
                                , modal =
                                    model.modal
                                        |> Maybe.andThen Modal.receiveUser
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
                        |> Blockchain.update blockchainMsg
                        |> Tuple.mapBoth
                            (\updated ->
                                { model
                                    | blockchain =
                                        updated |> Supported
                                }
                            )
                            (Cmd.map BlockchainMsg)

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
                                        , cmd |> Cmd.map PageMsg
                                        )
                           )

                _ ->
                    ( model
                    , Cmd.none
                    )

        ModalMsg modalMsg ->
            model.modal
                |> Maybe.map (Modal.update model modalMsg)
                |> Maybe.map
                    (\( updated, cmd, maybeEffect ) ->
                        maybeEffect
                            |> Maybe.map
                                (\effect ->
                                    { model | modal = updated }
                                        |> modalEffects effect
                                        |> Tuple.mapSecond List.singleton
                                        |> Tuple.mapSecond
                                            ((::) (cmd |> Cmd.map ModalMsg))
                                        |> Tuple.mapSecond Cmd.batch
                                )
                            |> Maybe.withDefault
                                ( { model | modal = updated }
                                , cmd |> Cmd.map ModalMsg
                                )
                    )
                |> Maybe.withDefault
                    ( model
                    , Cmd.none
                    )


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
                    tokenParam
                        |> Modal.initTokenList
                        |> Just
              }
            , Cmd.none
            )

        Page.OpenMaturityList pair ->
            Modal.initMaturityList blockchain pair
                |> Tuple.mapBoth
                    (\maturityList ->
                        { model
                            | modal =
                                maturityList |> Just
                        }
                    )
                    (Cmd.map ModalMsg)

        Page.OpenInputMaturity pair ->
            ( { model | modal = Modal.initInputMaturity pair |> Just }
            , Cmd.none
            )

        Page.OpenSettings ->
            ( { model | modal = Modal.initSettings model |> Just }
            , Cmd.none
            )

        Page.OpenConnect ->
            ( { model
                | modal =
                    Modal.initConnect (Supported blockchain)
                        |> Just
              }
            , Cmd.none
            )

        Page.Approve erc20 ->
            blockchain
                |> Blockchain.updateApprove erc20
                |> Tuple.mapBoth
                    (\updated ->
                        { model
                            | blockchain = Supported updated
                            , modal =
                                TxnWrite.Approve erc20
                                    |> Modal.initConfirm
                                    |> Just
                        }
                    )
                    (Cmd.map BlockchainMsg)

        Page.Lend writeLend ->
            blockchain
                |> Blockchain.updateLend model writeLend
                |> Tuple.mapBoth
                    (\updated ->
                        { model
                            | blockchain = Supported updated
                            , modal =
                                writeLend
                                    |> WriteLend.toPool
                                    |> TxnWrite.Lend
                                    |> Modal.initConfirm
                                    |> Just
                        }
                    )
                    (Cmd.map BlockchainMsg)

        Page.Borrow writeBorrow ->
            blockchain
                |> Blockchain.updateBorrow model writeBorrow
                |> Tuple.mapBoth
                    (\updated ->
                        { model
                            | blockchain = Supported updated
                            , modal =
                                writeBorrow
                                    |> WriteBorrow.toPool
                                    |> TxnWrite.Borrow
                                    |> Modal.initConfirm
                                    |> Just
                        }
                    )
                    (Cmd.map BlockchainMsg)

        Page.Liquidity writeLiquidity ->
            blockchain
                |> Blockchain.updateLiquidity model writeLiquidity
                |> Tuple.mapBoth
                    (\updated ->
                        { model
                            | blockchain = Supported updated
                            , modal =
                                writeLiquidity
                                    |> WriteLiquidity.toPool
                                    |> TxnWrite.Liquidity
                                    |> Modal.initConfirm
                                    |> Just
                        }
                    )
                    (Cmd.map BlockchainMsg)

        Page.Create writeCreate ->
            blockchain
                |> Blockchain.updateCreate model writeCreate
                |> Tuple.mapBoth
                    (\updated ->
                        { model
                            | blockchain = Supported updated
                            , modal =
                                writeCreate
                                    |> WriteCreate.toPool
                                    |> TxnWrite.Create
                                    |> Modal.initConfirm
                                    |> Just
                        }
                    )
                    (Cmd.map BlockchainMsg)


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
                    blockchain
                        |> Blockchain.toChain
                        |> (\chain ->
                                model.chains
                                    |> Chains.insert chain erc20
                                    |> (\chains ->
                                            ( { model | chains = chains }
                                            , [ Route.fromTab
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


port cacheChosenZone : Value -> Cmd msg


port cacheTheme : Value -> Cmd msg


port cacheCustom : Value -> Cmd msg


port cacheSettings : Value -> Cmd msg


port receiveMetamaskInstalled : (() -> msg) -> Sub msg


port receiveUser : (Value -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    [ Browser.Events.onResize ResizeWindow
    , Browser.Events.onVisibilityChange VisibilityChange
    , Time.every 1000 ReceiveTime
    , receiveMetamaskInstalled ReceiveMetamaskInstalled
    , receiveUser ReceiveUser
    , model.page
        |> Page.subscriptions
        |> Sub.map PageMsg
    , model.modal
        |> Maybe.map Modal.subscriptions
        |> (Maybe.map << Sub.map) ModalMsg
        |> Maybe.withDefault Sub.none
    ]
        |> Sub.batch


view : Model -> Document Msg
view model =
    { title = "Timeswap"
    , body = [ html model ]
    }


html : Model -> Html Msg
html model =
    layoutWith
        { options = options }
        [ width <| minimum 360 fill
        , height shrink
        , model.modal
            |> Maybe.map (Modal.view model)
            |> (Maybe.map << map) ModalMsg
            |> Maybe.withDefault none
            |> inFront
        , header model
            |> inFront
        , Font.family [ Font.typeface "Supreme" ]
        , (case model.theme of
            Theme.Light ->
                Color.light100

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


header :
    { model
        | offset : Offset
        , zoneName : Maybe ZoneName
        , chosenZone : ChosenZone
        , device : Device
        , backdrop : Backdrop
        , theme : Theme
        , images : Images
        , blockchain : Support User.NotSupported Blockchain
        , page : Page
    }
    -> Element Msg
header ({ device } as model) =
    if device |> Device.isPhoneOrTablet then
        column
            [ Region.navigation
            , width fill
            , height shrink
            , spacing 40
            ]
            [ row
                [ width fill
                , height <| px 55
                ]
                [ logo model |> map never ]
            , row
                [ width fill
                , height <| px 45
                ]
                [ tabs model |> map never ]
            ]

    else
        row
            [ Region.navigation
            , width fill
            , height <| px 80
            , spacing 76
            , paddingXY 16 0
            ]
            [ logo model |> map never
            , tabs model |> map never
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


logo : { model | device : Device, images : Images } -> Element Never
logo { device, images } =
    row
        [ Region.description "logo"
        , spacing 6
        , centerY
        ]
        [ case device of
            Phone ->
                images
                    |> Image.logoPure
                        [ height <| px 32 ]

            Tablet ->
                images
                    |> Image.logo
                        [ height <| px 32 ]

            _ ->
                images
                    |> Image.logo
                        [ height <| px 32 ]
        , el
            [ width shrink
            , height <| px 22
            , padding 5
            , alignLeft
            , centerY
            , Background.color Color.primary500
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


tabs :
    { model
        | device : Device
        , backdrop : Backdrop
        , page : Page
    }
    -> Element Never
tabs ({ device } as model) =
    row
        [ Region.description "tabs"
        , width shrink
        , height <| px 44
        , alignLeft
        , padding 4
        , spacing 4
        , Background.color Color.primary100
        , Border.rounded 8
        ]
        [ tab model Tab.Lend
        , tab model Tab.Borrow
        , tab model Tab.Liquidity
        ]


tab : { model | device : Device, page : Page } -> Tab -> Element Never
tab { device, page } givenTab =
    if (page |> Page.toTab) == givenTab then
        el
            [ width <| px 84
            , height fill
            , Background.color Color.primary500
            , Border.rounded 4
            ]
            (el
                [ centerX
                , centerY
                , Font.color Color.light100
                , Font.size 16
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
                    , Font.color Color.light100
                    , Font.size 16
                    ]
                    (givenTab
                        |> Tab.toString
                        |> text
                    )
            }


chainListButton :
    { model
        | backdrop : Backdrop
        , images : Images
        , blockchain : Support User.NotSupported Blockchain
    }
    -> Element Msg
chainListButton ({ images } as model) =
    Input.button
        [ Region.description "chains button"
        , width shrink
        , height <| px 44
        , paddingXY 12 0
        , Background.color Color.primary100
        , Border.rounded 8
        , mouseDown [ Background.color Color.primary300 ]
        , mouseOver [ Background.color Color.primary200 ]
        ]
        { onPress = Just OpenChainList
        , label =
            row
                [ centerX
                , centerY
                , paddingXY 0 3
                , spacing 6
                , Font.size 16
                , Font.color Color.transparent400
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
                        , blockchain
                            |> Blockchain.toChain
                            |> Chain.toString
                            |> text
                        ]

                    NotSupported _ ->
                        [ "Not Supported" |> text ]
                )
        }


connectButton :
    { model
        | backdrop : Backdrop
        , images : Images
        , blockchain : Support User.NotSupported Blockchain
    }
    -> Element Msg
connectButton ({ images } as model) =
    Input.button
        ([ width shrink
         , height <| px 44
         , paddingXY 12 0
         , Border.rounded 8
         ]
            ++ (case model.blockchain of
                    Supported blockchain ->
                        blockchain
                            |> Blockchain.toUser
                            |> Maybe.map
                                (\_ ->
                                    [ Region.description "account button"
                                    , Background.color Color.primary100
                                    , mouseDown [ Background.color Color.primary300 ]
                                    , mouseOver [ Background.color Color.primary200 ]
                                    ]
                                )
                            |> Maybe.withDefault
                                [ Region.description "connect button"
                                , Background.color Color.primary500
                                , mouseDown [ Background.color Color.primary300 ]
                                , mouseOver [ Background.color Color.primary400 ]
                                ]

                    NotSupported _ ->
                        [ Region.description "account button"
                        , Background.color Color.primary100
                        , mouseDown [ Background.color Color.primary300 ]
                        , mouseOver [ Background.color Color.primary200 ]
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
                                        , el
                                            [ width shrink
                                            , height shrink
                                            , Font.size 16
                                            , Font.color Color.transparent400
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
                                (el
                                    [ width shrink
                                    , height shrink
                                    , Font.size 16
                                    , Font.color Color.transparent400
                                    ]
                                    (text "Connect Wallet")
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
                            , el
                                [ width shrink
                                , height shrink
                                , Font.size 16
                                , Font.color Color.transparent400
                                ]
                                (user
                                    |> User.toAddressNotSupported
                                    |> Address.toStringShort
                                    |> text
                                )
                            ]
                )
        }


zoneButton :
    { model
        | offset : Offset
        , zoneName : Maybe ZoneName
        , chosenZone : ChosenZone
        , backdrop : Backdrop
    }
    -> Element Msg
zoneButton ({ offset, zoneName, chosenZone } as model) =
    Input.button
        [ Region.description "zone button"
        , width shrink
        , height <| px 44
        , paddingXY 12 0
        , Background.color Color.primary100
        , Border.rounded 8
        , mouseDown [ Background.color Color.primary300 ]
        , mouseOver [ Background.color Color.primary200 ]
        ]
        { onPress = Just SwitchZone
        , label =
            el
                [ centerX
                , centerY
                , Font.size 16
                , Font.color Color.transparent400
                ]
                ((case chosenZone of
                    ChosenZone.UTC ->
                        "UTC"

                    ChosenZone.Here ->
                        zoneName
                            |> ZoneName.toString offset

                    ChosenZone.Unix ->
                        "Unix"
                 )
                    |> text
                )
        }


themeButton : { model | backdrop : Backdrop, theme : Theme, images : Images } -> Element Msg
themeButton ({ theme, images } as model) =
    Input.button
        [ Region.description "theme button"
        , width <| px 44
        , height <| px 44
        , Background.color Color.primary100
        , Border.rounded 8
        , mouseDown [ Background.color Color.primary300 ]
        , mouseOver [ Background.color Color.primary200 ]
        ]
        { onPress = Just SwitchTheme
        , label =
            el
                [ centerX
                , centerY
                , Font.size 16
                , Font.color Color.transparent400
                ]
                (case theme of
                    Theme.Light ->
                        images
                            |> Image.blackSun
                                [ width <| px 24
                                , height <| px 24
                                ]

                    Theme.Dark ->
                        images
                            |> Image.whiteSun
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
        , backdrop : Backdrop
        , priceFeed : PriceFeed
        , images : Images
        , blockchain : Support User.NotSupported Blockchain
        , page : Page
    }
    -> Element Msg
body ({ page } as model) =
    el
        [ Region.mainContent
        , width fill
        , height shrink
        , paddingEach
            { top = 136
            , right = 80
            , bottom = 56
            , left = 56
            }
        ]
        (case model.blockchain of
            Supported blockchain ->
                Page.view model blockchain page |> map PageMsg

            NotSupported _ ->
                none
        )
