port module Main exposing (main)

import Blockchain.Main as Blockchain exposing (Blockchain)
import Blockchain.User.Main as User
import Browser exposing (Document, UrlRequest(..))
import Browser.Events exposing (Visibility)
import Browser.Navigation as Navigation exposing (Key)
import Data.Backdrop as Backdrop exposing (Backdrop)
import Data.Chains as Chains exposing (Chains)
import Data.ChosenZone as ChosenZone exposing (ChosenZone)
import Data.Deadline as Deadline exposing (Deadline)
import Data.Device as Device exposing (Device(..))
import Data.Images as Images exposing (Images)
import Data.Oracle as Oracle exposing (Oracle)
import Data.Parameter as Parameter
import Data.Slippage as Slippage exposing (Slippage)
import Data.Tab as Tab exposing (Tab)
import Data.Theme as Theme exposing (Theme)
import Data.Token as Token
import Data.Wallets as Wallets exposing (Wallets)
import Element
    exposing
        ( Element
        , Option
        , alignLeft
        , centerY
        , column
        , el
        , fill
        , focusStyle
        , height
        , layoutWith
        , link
        , minimum
        , padding
        , px
        , row
        , shrink
        , text
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Region as Region
import Html exposing (Html)
import Json.Encode exposing (Value)
import Modal.Main as Modal exposing (Modal)
import Page.Main as Page exposing (Page)
import Page.Route as Route
import Task
import Time exposing (Posix, Zone, ZoneName)
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
    , zone : Zone
    , zoneName : ZoneName
    , chosenZone : ChosenZone
    , device : Device
    , visibility : Visibility
    , backdrop : Backdrop
    , theme : Theme
    , images : Images
    , slippage : Slippage
    , deadline : Deadline
    , oracle : Oracle
    , wallets : Wallets
    , chains : Chains
    , blockchain : Blockchain
    , page : Page
    , modal : Maybe Modal
    }


type alias Flags =
    { time : Int
    , offset : Int
    , zoneName : Maybe String
    , chosenZone : ChosenZone.Flag
    , width : Int
    , hasBackdropSupport : Backdrop.Flag
    , theme : Theme.Flag
    , images : Images.Flags
    , tokenImages : Images.Flags
    , chainImages : Images.Flags
    , slippage : Slippage.Flag
    , deadline : Deadline.Flag
    , oracle : Oracle.Flag
    , wallets : Wallets.Flags
    , chains : Chains.Flags
    , user : Maybe User.Flag
    }


type Msg
    = RequestUrl UrlRequest
    | ChangeUrl Url
    | ReceiveTime Posix
    | ReceiveZone Zone
    | ReceiveZoneName ZoneName
    | SwitchZone
    | ResizeWindow Int Int
    | VisibilityChange Visibility
    | SwitchTheme
    | ReceiveUser Value
    | ModalMsg Modal.Msg


init : Flags -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    flags.chains
        |> Chains.init
        |> (\chains ->
                ( chains
                , flags.user |> Blockchain.init chains
                )
           )
        |> (\( chains, blockchain ) ->
                ( { key = key
                  , url = url
                  , time = flags.time |> Time.millisToPosix
                  , zone = Time.customZone flags.offset []
                  , zoneName =
                        flags.zoneName
                            |> Maybe.map Time.Name
                            |> Maybe.withDefault (Time.Offset flags.offset)
                  , chosenZone = flags.chosenZone |> ChosenZone.init
                  , device = flags.width |> Device.fromWidth
                  , visibility = Browser.Events.Visible
                  , backdrop = flags.hasBackdropSupport |> Backdrop.init
                  , theme = flags.theme |> Theme.init
                  , images = Images.init flags.images flags.tokenImages flags.chainImages
                  , slippage = flags.slippage |> Slippage.init
                  , deadline = flags.deadline |> Deadline.init
                  , oracle = flags.oracle |> Oracle.init
                  , wallets = flags.wallets |> Wallets.init
                  , chains = chains
                  , blockchain = blockchain
                  , page = url |> Page.init blockchain chains
                  , modal = Nothing
                  }
                , [ Time.now |> Task.perform ReceiveTime
                  , Time.here |> Task.perform ReceiveZone
                  , Time.getZoneName |> Task.perform ReceiveZoneName
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
            ( { model
                | url = url
                , page = url |> Page.init model.blockchain model.chains
              }
            , Cmd.none
            )

        ReceiveTime posix ->
            ( { model | time = posix }
            , Cmd.none
            )

        ReceiveZone zone ->
            ( { model | zone = zone }
            , Cmd.none
            )

        ReceiveZoneName zoneName ->
            ( { model | zoneName = zoneName }
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

        ReceiveUser value ->
            model.blockchain
                |> Blockchain.updateUser model value
                |> Tuple.mapFirst
                    (\blockchain ->
                        { model
                            | blockchain = blockchain
                            , modal =
                                model.modal
                                    |> Maybe.andThen Modal.closeConnect
                        }
                    )

        ModalMsg modalMsg ->
            model.modal
                |> Maybe.map (Modal.update modalMsg)
                |> Maybe.map
                    (\( updated, cmd, maybeEffect ) ->
                        maybeEffect
                            |> Maybe.map
                                (\effect ->
                                    { model | modal = updated }
                                        |> modalEffect effect
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


modalEffect :
    Modal.Effect
    -> Model
    -> ( Model, Cmd Msg )
modalEffect effect model =
    case effect of
        Modal.UpdateSettings slippage deadline oracle ->
            ( { model
                | slippage = slippage
                , deadline = deadline
                , oracle = oracle
              }
            , [ slippage |> Slippage.encode |> cacheSlippage
              , deadline |> Deadline.encode |> cacheDeadline
              , oracle |> Oracle.encode |> cacheOracle
              ]
                |> Cmd.batch
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
            model.blockchain
                |> Blockchain.toChain
                |> Maybe.map
                    (\chain ->
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
                |> Maybe.withDefault ( model, Cmd.none )

        Modal.RemoveERC20 erc20 ->
            model.blockchain
                |> Blockchain.toChain
                |> Maybe.map
                    (\chain ->
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
                |> Maybe.withDefault ( model, Cmd.none )

        Modal.RemoveAll ->
            model.blockchain
                |> Blockchain.toChain
                |> Maybe.map
                    (\chain ->
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
                |> Maybe.withDefault ( model, Cmd.none )


port cacheChosenZone : Value -> Cmd msg


port cacheTheme : Value -> Cmd msg


port cacheSlippage : Value -> Cmd msg


port cacheDeadline : Value -> Cmd msg


port cacheOracle : Value -> Cmd msg


port cacheCustom : Value -> Cmd msg


port receiveUser : (Value -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    [ Browser.Events.onResize ResizeWindow
    , Browser.Events.onVisibilityChange VisibilityChange
    , receiveUser ReceiveUser
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
        , height fill
        , Font.family [ Font.typeface "Supreme" ]
        ]
        (column
            [ width fill
            , height shrink
            ]
            [ header model ]
        )


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
        | device : Device
        , images : Images
        , page : Page
    }
    -> Element Msg
header ({ device } as model) =
    if device |> Device.isPhoneOrTablet then
        column
            [ Region.navigation
            , width fill
            , height shrink
            ]
            [ row
                [ width fill
                , height <| px 55
                ]
                [ logo model ]
            , row
                [ width fill
                , height <| px 45
                ]
                [ tabs model ]
            ]

    else
        row
            [ Region.navigation
            , width fill
            , height <| px 80
            ]
            [ logo model
            , tabs model
            ]


logo : { model | device : Device, images : Images } -> Element msg
logo { device, images } =
    row
        []
        [ case device of
            Phone ->
                images
                    |> Image.logoPure
                        []

            Tablet ->
                images
                    |> Image.logo
                        []

            _ ->
                images
                    |> Image.logo
                        []
        , el
            [ width shrink
            , height shrink
            , padding 5
            , alignLeft
            , centerY
            , Background.color Color.primary500
            , Border.rounded 4
            , Font.bold
            , Font.color Color.light100
            , Font.letterSpacing 1.28
            , (if device |> Device.isPhoneOrTablet then
                18

               else
                12
              )
                |> Font.size
            ]
            (text "ALPHA")
        ]


tabs : { model | device : Device, page : Page } -> Element msg
tabs ({ device } as model) =
    row
        [ (if device |> Device.isPhoneOrTablet then
            fill

           else
            shrink
          )
            |> width
        , height fill
        , alignLeft
        ]
        [ tab model Tab.Lend
        , tab model Tab.Borrow
        , tab model Tab.Liquidity
        ]


tab : { model | device : Device, page : Page } -> Tab -> Element msg
tab { device, page } givenTab =
    if (page |> Page.toTab) == givenTab then
        column
            []
            []

    else
        link
            []
            { url =
                page
                    |> Page.toParameter
                    |> Route.fromTab givenTab
                    |> Route.toUrlString
            , label =
                column
                    []
                    []
            }
