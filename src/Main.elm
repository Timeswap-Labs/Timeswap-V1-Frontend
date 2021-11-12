port module Main exposing (main)

import Aside
import Browser exposing (Document, UrlRequest(..))
import Browser.Events exposing (Visibility)
import Browser.Navigation as Navigation exposing (Key)
import Data.Backdrop as Backdrop exposing (Backdrop)
import Data.Chain exposing (Chain(..))
import Data.Connectivity as Connectivity exposing (Connectivity)
import Data.Deadline as Deadline exposing (Deadline)
import Data.Device as Device exposing (Device)
import Data.Images as Images exposing (Images)
import Data.Or exposing (Or(..))
import Data.Pools as Pools exposing (Pools)
import Data.Remote as Remote exposing (Remote(..))
import Data.Slippage as Slippage exposing (Slippage)
import Data.TokenImages as TokenImages exposing (TokenImages)
import Data.Tokens exposing (Tokens)
import Data.Whitelist as Whitelist
import Data.ZoneInfo exposing (ZoneInfo)
import Element
    exposing
        ( Option
        , behindContent
        , clip
        , column
        , fill
        , focusStyle
        , height
        , inFront
        , layoutWith
        , minimum
        , row
        , width
        )
import Element.Background as Background
import Element.Lazy as Lazy
import Error
import Header
import Html exposing (Html)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode exposing (Value)
import Modal exposing (Modal)
import Page exposing (Page)
import Route
import Service as Service exposing (Service)
import Task
import Time exposing (Posix)
import Url exposing (Url)
import User exposing (User)
import Utility.Color as Color
import Utility.Image as Image
import Utility.Router as Router


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
    { device : Device
    , visibility : Visibility
    , connectivity : Connectivity
    , time : Posix
    , zoneInfo : Maybe ZoneInfo
    , backdrop : Backdrop
    , key : Key
    , slippage : Slippage
    , deadline : Deadline
    , tokens : Tokens
    , images : Images
    , tokenImages : TokenImages
    , pools : Pools
    , user : Remote User.Error User
    , page : Page
    , modal : Maybe Modal
    , service : Maybe Service
    , dropdown : Maybe ()
    }


type alias Flags =
    { width : Int
    , time : Int
    , hasBackdropSupport : Bool
    , images : List ( String, String )
    , tokenImages : List ( String, String )
    , whitelist : Value
    , user : Value
    }


init : Flags -> Url -> Key -> ( Model, Cmd Msg )
init { width, time, hasBackdropSupport, images, tokenImages, whitelist, user } url key =
    Whitelist.init whitelist
        |> (\{ tokens, pools } ->
                { device = Device.fromWidth width
                , visibility = Browser.Events.Visible
                , connectivity = Connectivity.Connected
                , time = Time.millisToPosix time
                , zoneInfo = Nothing
                , backdrop = hasBackdropSupport |> Backdrop.setBackdrop
                , key = key
                , slippage = Slippage.init
                , deadline = Deadline.init
                , tokens = tokens
                , images = Images.init images
                , tokenImages = TokenImages.init tokenImages
                , pools = pools
                , user =
                    user
                        |> Decode.decodeValue (User.decoder |> Decode.nullable)
                        |> (\result ->
                                case result of
                                    Ok Nothing ->
                                        Loading

                                    Ok (Just successUser) ->
                                        successUser

                                    _ ->
                                        Loading
                           )
                , page =
                    Page.init
                        { pools = pools
                        , user = Loading
                        }
                , modal = Nothing
                , service = Nothing
                , dropdown = Nothing
                }
           )
        |> (\model ->
                ( model
                , Cmd.batch
                    [ Time.now |> Task.perform ReceiveTime
                    , Task.map2 ZoneInfo Time.here Time.getZoneName
                        |> Task.perform ReceiveZoneInfo
                    , Route.pushUrl model url
                    ]
                )
           )


type Msg
    = RequestUrl UrlRequest
    | ChangeUrl Url
    | ResizeWindow Int Int
    | ClickOutsideAside
    | VisibilityChange Visibility
    | ConnectivityMsg Value
    | ReceiveTime Posix
    | ReceiveZoneInfo ZoneInfo
    | OpenDropdown
    | CloseDropdown
    | ExitSettings
    | ChooseSlippageOption Slippage.Option
    | ChooseDeadlineOption Deadline.Option
    | InputSlippage String
    | InputDeadline String
    | ClickOutsideSlippage
    | ClickOutsideDeadline
    | PageMsg Page.Msg
    | ModalMsg Modal.Msg
    | ServiceMsg Service.Msg
    | MetamaskMsg Value
    | NoMetamask Value
    | SdkPoolsMsg Value
    | SdkPositionsMsg Value
    | SdkBalancesMsg Value
    | SdkAllowancesMsg Value


type alias Msgs =
    { openDropdown : Msg
    , closeDropdown : Msg
    , exitSettings : Msg
    , chooseSlippageOption : Slippage.Option -> Msg
    , chooseDeadlineOption : Deadline.Option -> Msg
    , inputSlippage : String -> Msg
    , inputDeadline : String -> Msg
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RequestUrl (Browser.Internal url) ->
            ( model
            , Route.pushUrl model url
            )

        RequestUrl (Browser.External url) ->
            ( model
            , Navigation.load url
            )

        ChangeUrl url ->
            url
                |> Route.fromUrl model
                |> Maybe.map
                    (\route ->
                        case route of
                            Route.Page page ->
                                ( { model
                                    | device = model.device |> Device.closeAside
                                    , page =
                                        if Page.same model.page page then
                                            model.page

                                        else
                                            page
                                    , modal = Nothing
                                    , service = Nothing
                                  }
                                , Cmd.none
                                )

                            Route.Modal ( modal, cmd ) ->
                                ( { model
                                    | device = model.device |> Device.closeAside
                                    , modal =
                                        model.modal
                                            |> Maybe.map
                                                (\modelModal ->
                                                    if Modal.same modelModal modal then
                                                        model.modal

                                                    else
                                                        Just modal
                                                )
                                            |> Maybe.withDefault (Just modal)
                                    , service = Nothing
                                  }
                                , cmd |> Cmd.map ModalMsg
                                )

                            Route.Service service ->
                                ( { model
                                    | device = model.device |> Device.closeAside
                                    , service =
                                        model.service
                                            |> Maybe.map
                                                (\modelService ->
                                                    if Service.same modelService service then
                                                        model.service

                                                    else
                                                        Just service
                                                )
                                            |> Maybe.withDefault (Just service)
                                  }
                                , Cmd.none
                                )

                            Route.Aside ->
                                ( { model | device = model.device |> Device.openAside }
                                , Cmd.none
                                )

                            Route.Exit ->
                                ( if model.device |> Device.checkAsideStatus then
                                    { model | device = model.device |> Device.closeAside }

                                  else
                                    model.service
                                        |> Maybe.map (\_ -> { model | service = Nothing })
                                        |> Maybe.withDefault
                                            (model.modal
                                                |> Maybe.map (\_ -> { model | modal = Nothing })
                                                |> Maybe.withDefault { model | page = Page.init model }
                                            )
                                , Cmd.none
                                )
                    )
                |> Maybe.withDefault ( model, Cmd.none )

        ResizeWindow width _ ->
            ( { model | device = Device.fromDeviceWidth model.device width }
            , Cmd.none
            )

        ClickOutsideAside ->
            ( model
            , model.service
                |> Maybe.map Service.toUrl
                |> Maybe.withDefault
                    (model.modal
                        |> Maybe.map Modal.toUrl
                        |> Maybe.withDefault (model.page |> Page.toUrl)
                    )
                |> Navigation.pushUrl model.key
            )

        VisibilityChange visibility ->
            ( { model | visibility = visibility }
            , Cmd.none
            )

        ConnectivityMsg value ->
            ( value
                |> Decode.decodeValue Connectivity.decoder
                |> Result.map
                    (\connectivity -> { model | connectivity = connectivity })
                |> Result.withDefault model
            , Cmd.none
            )

        ReceiveTime posix ->
            ( { model | time = posix }
            , Cmd.none
            )

        ReceiveZoneInfo zoneInfo ->
            ( { model | zoneInfo = Just zoneInfo }
            , Cmd.none
            )

        OpenDropdown ->
            ( { model | dropdown = Just () }
            , Cmd.none
            )

        CloseDropdown ->
            ( { model | dropdown = Nothing }
            , Cmd.none
            )

        ExitSettings ->
            ( { model
                | slippage =
                    model.service
                        |> Maybe.andThen Service.getSlippage
                        |> Maybe.withDefault model.slippage
                , deadline =
                    model.service
                        |> Maybe.andThen Service.getDeadline
                        |> Maybe.withDefault model.deadline
              }
            , model.service
                |> Maybe.map
                    (\_ ->
                        model.modal
                            |> Maybe.map Modal.toUrl
                            |> Maybe.withDefault (model.page |> Page.toUrl)
                    )
                |> Maybe.withDefault
                    (model.modal
                        |> Maybe.map (\_ -> model.page |> Page.toUrl)
                        |> Maybe.withDefault Router.toAllMarket
                    )
                |> Navigation.pushUrl model.key
            )

        ChooseSlippageOption option ->
            ( { model
                | slippage = option |> Slippage.fromOption
                , service = Just Service.refreshSettings
              }
            , Cmd.none
            )

        ChooseDeadlineOption option ->
            ( { model
                | deadline = option |> Deadline.fromOption
                , service = Just Service.refreshSettings
              }
            , Cmd.none
            )

        InputSlippage string ->
            ( { model | service = model.service |> Maybe.map (Service.inputSlippage string) }
            , Cmd.none
            )

        InputDeadline string ->
            ( { model | service = model.service |> Maybe.map (Service.inputDeadline string) }
            , Cmd.none
            )

        ClickOutsideSlippage ->
            ( { model
                | slippage =
                    model.service
                        |> Maybe.andThen Service.getSlippage
                        |> Maybe.withDefault model.slippage
                , service = Just Service.refreshSettings
              }
            , Cmd.none
            )

        ClickOutsideDeadline ->
            ( { model
                | deadline =
                    model.service
                        |> Maybe.andThen Service.getDeadline
                        |> Maybe.withDefault model.deadline
                , service = Just Service.refreshSettings
              }
            , Cmd.none
            )

        PageMsg pageMsg ->
            ( { model | page = model.page |> Page.update model pageMsg }
            , Cmd.none
            )

        ModalMsg modalMsg ->
            model.modal
                |> Maybe.map (Modal.update model modalMsg)
                |> Maybe.map
                    (\( modal, cmd ) ->
                        ( { model | modal = Just modal }
                        , cmd |> Cmd.map ModalMsg
                        )
                    )
                |> Maybe.withDefault ( model, Cmd.none )

        ServiceMsg serviceMsg ->
            model.service
                |> Maybe.map (Service.update model serviceMsg)
                |> Maybe.map
                    (\( service, cmd ) ->
                        ( { model | service = Just service }
                        , cmd |> Cmd.map ServiceMsg
                        )
                    )
                |> Maybe.withDefault ( model, Cmd.none )

        MetamaskMsg value ->
            ( value
                |> Decode.decodeValue (User.decoder |> Decode.nullable)
                |> (\result ->
                        case result of
                            Ok maybeUser ->
                                { model
                                    | user =
                                        case ( model.user, maybeUser ) of
                                            ( _, Nothing ) ->
                                                Loading

                                            ( Success modelUser, Just (Success user) ) ->
                                                if User.same modelUser user then
                                                    model.user

                                                else
                                                    Success user

                                            ( _, Just remoteUser ) ->
                                                remoteUser
                                }

                            _ ->
                                model
                   )
            , Cmd.none
            )

        NoMetamask _ ->
            ( model
            , Navigation.pushUrl model.key "#nometamask"
            )

        SdkPoolsMsg value ->
            ( { model | pools = model.pools |> Pools.update model.tokens value }
            , Cmd.none
            )

        SdkPositionsMsg value ->
            model.user
                |> Remote.map
                    (\user ->
                        user
                            |> User.updatePositions model.pools model.tokens value
                            |> (\({ positions } as successUser) ->
                                    ( { model | user = Success successUser }
                                        |> (\newModel ->
                                                { newModel
                                                    | page =
                                                        newModel.page
                                                            |> Page.updateLendDashboard user.positions newModel
                                                            |> Page.updateBorrowDashboard user.positions newModel
                                                }
                                           )
                                    , case positions of
                                        Success succcessPositions ->
                                            model.modal
                                                |> Maybe.map (Modal.updatePayDue succcessPositions)
                                                |> (Maybe.map << Cmd.map) ModalMsg
                                                |> Maybe.withDefault Cmd.none

                                        _ ->
                                            Cmd.none
                                    )
                               )
                    )
                |> (\result ->
                        case result of
                            Success successModel ->
                                successModel

                            _ ->
                                ( model, Cmd.none )
                   )

        SdkBalancesMsg value ->
            ( { model
                | user =
                    model.user
                        |> Remote.map (User.updateBalances model.tokens value)
              }
            , Cmd.none
            )

        SdkAllowancesMsg value ->
            ( { model
                | user =
                    model.user
                        |> Remote.map (User.updateAllowances model.tokens value)
              }
            , Cmd.none
            )


port connectivityMsg : (Value -> msg) -> Sub msg


port metamaskMsg : (Value -> msg) -> Sub msg


port noMetamask : (Value -> msg) -> Sub msg


port sdkPoolsMsg : (Value -> msg) -> Sub msg


port sdkPositionsMsg : (Value -> msg) -> Sub msg


port sdkBalancesMsg : (Value -> msg) -> Sub msg


port sdkAllowancesMsg : (Value -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions ({ modal, service } as model) =
    Sub.batch
        [ Browser.Events.onResize ResizeWindow
        , Browser.Events.onVisibilityChange VisibilityChange
        , connectivityMsg ConnectivityMsg
        , Time.every 1000 ReceiveTime
        , onClickOutsideDropdown model
        , onClickOutsideAside model
        , onClickOutsideSlippage model
        , onClickOutsideDeadline model
        , metamaskMsg MetamaskMsg
        , noMetamask NoMetamask
        , sdkPoolsMsg SdkPoolsMsg
        , sdkPositionsMsg SdkPositionsMsg
        , sdkBalancesMsg SdkBalancesMsg
        , sdkAllowancesMsg SdkAllowancesMsg
        , service
            |> Maybe.map (Service.subscriptions >> Sub.map ServiceMsg)
            |> Maybe.withDefault
                (modal
                    |> Maybe.map (Modal.subscriptions model)
                    |> (Maybe.map << Sub.map) ModalMsg
                    |> Maybe.withDefault Sub.none
                )
        ]


onClickOutsideDropdown : { model | dropdown : Maybe () } -> Sub Msg
onClickOutsideDropdown { dropdown } =
    dropdown
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
                if string /= "dropdown" then
                    Decode.succeed CloseDropdown

                else
                    Decode.fail "Its the dropdown"
            )


onClickOutsideAside : { model | device : Device } -> Sub Msg
onClickOutsideAside { device } =
    if Device.checkAsideStatus device then
        Browser.Events.onClick (Decode.at [ "target", "id" ] decoderOutsideAside)

    else
        Sub.none


decoderOutsideAside : Decoder Msg
decoderOutsideAside =
    Decode.string
        |> Decode.andThen
            (\string ->
                if string == "outside-aside" then
                    Decode.succeed ClickOutsideAside

                else
                    Decode.fail "Its the aside id"
            )


onClickOutsideSlippage : { model | service : Maybe Service } -> Sub Msg
onClickOutsideSlippage model =
    model.service
        |> Maybe.map
            (\service ->
                if service |> Service.hasSlippageInput then
                    Browser.Events.onClick (Decode.at [ "target", "id" ] decoderOutsideSlippage)

                else
                    Sub.none
            )
        |> Maybe.withDefault Sub.none


decoderOutsideSlippage : Decoder Msg
decoderOutsideSlippage =
    Decode.string
        |> Decode.andThen
            (\string ->
                if string /= "slippage" then
                    Decode.succeed ClickOutsideSlippage

                else
                    Decode.fail "Its the slippage input"
            )


onClickOutsideDeadline : { model | service : Maybe Service } -> Sub Msg
onClickOutsideDeadline model =
    model.service
        |> Maybe.map
            (\service ->
                if service |> Service.hasDeadlineInput then
                    Browser.Events.onClick (Decode.at [ "target", "id" ] decoderOutsideDeadline)

                else
                    Sub.none
            )
        |> Maybe.withDefault Sub.none


decoderOutsideDeadline : Decoder Msg
decoderOutsideDeadline =
    Decode.string
        |> Decode.andThen
            (\string ->
                if string /= "deadline" then
                    Decode.succeed ClickOutsideDeadline

                else
                    Decode.fail "Its the deadline input"
            )


view : Model -> Document Msg
view model =
    { title = "Timeswap"
    , body = [ html model ]
    }


msgs : Msgs
msgs =
    { openDropdown = OpenDropdown
    , closeDropdown = CloseDropdown
    , exitSettings = ExitSettings
    , chooseSlippageOption = ChooseSlippageOption
    , chooseDeadlineOption = ChooseDeadlineOption
    , inputSlippage = InputSlippage
    , inputDeadline = InputDeadline
    }


html : Model -> Html Msg
html model =
    if Device.isPhoneOrTablet model.device then
        layoutWith
            { options = options }
            ([ width <| minimum 360 fill
             , Background.color Color.dark400
             ]
                ++ (if Device.checkAsideStatus model.device then
                        [ Lazy.lazy Aside.view model |> inFront ]

                    else
                        model.service
                            |> Maybe.map
                                (\service ->
                                    [ Service.view msgs model service
                                        |> Element.map
                                            (\or ->
                                                case or of
                                                    Either serviceMsg ->
                                                        serviceMsg |> ServiceMsg

                                                    Or msg ->
                                                        msg
                                            )
                                        |> inFront
                                    ]
                                )
                            |> Maybe.map Just
                            |> Maybe.withDefault
                                (model.modal
                                    |> Maybe.map
                                        (\modal ->
                                            [ Lazy.lazy2 Modal.view model modal |> Element.map ModalMsg |> inFront ]
                                        )
                                )
                            |> Maybe.withDefault []
                   )
            )
            (column
                [ width fill
                , height fill
                , clip
                ]
                [ Lazy.lazy2 Header.view msgs model
                , Lazy.lazy2 Page.view model model.page |> Element.map PageMsg
                ]
            )

    else
        layoutWith
            { options = options }
            ([ width fill
             , Background.color Color.dark400
             , behindContent <| Image.background model.images
             ]
                ++ (model.service
                        |> Maybe.map
                            (\service ->
                                [ Service.view msgs model service
                                    |> Element.map
                                        (\or ->
                                            case or of
                                                Either serviceMsg ->
                                                    serviceMsg |> ServiceMsg

                                                Or msg ->
                                                    msg
                                        )
                                    |> inFront
                                ]
                            )
                        |> Maybe.map Just
                        |> Maybe.withDefault
                            (model.modal
                                |> Maybe.map
                                    (\modal ->
                                        [ Lazy.lazy2 Modal.view model modal |> Element.map ModalMsg |> inFront ]
                                    )
                            )
                        |> Maybe.withDefault []
                   )
            )
        <|
            column
                [ width fill
                , height fill
                , clip
                ]
                [ Lazy.lazy Error.view model
                , Lazy.lazy2 Header.view msgs model
                , row
                    [ width fill
                    , height fill
                    , clip
                    ]
                    [ Lazy.lazy Aside.view model
                    , Lazy.lazy2 Page.view model model.page |> Element.map PageMsg
                    ]
                ]


options : List Option
options =
    [ focusStyle
        { borderColor = Nothing
        , backgroundColor = Nothing
        , shadow = Nothing
        }
    ]
