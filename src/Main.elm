port module Main exposing (main)

import Aside
import Browser exposing (Document, UrlRequest(..))
import Browser.Events exposing (Visibility)
import Browser.Navigation as Navigation exposing (Key)
import Data.Backdrop as Backdrop exposing (Backdrop)
import Data.Chain exposing (Chain(..))
import Data.Device as Device exposing (Device)
import Data.Pools as Pools exposing (Pools)
import Data.ZoneInfo exposing (ZoneInfo)
import Element
    exposing
        ( Element
        , Option
        , behindContent
        , clip
        , column
        , el
        , fill
        , focusStyle
        , height
        , inFront
        , layoutWith
        , minimum
        , none
        , row
        , width
        )
import Element.Background as Background
import Element.Lazy as Lazy
import Header
import Html exposing (Html)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode exposing (Value)
import Modal exposing (Modal)
import Page exposing (Page)
import Pages.AllMarket.Main as AllMarket
import Pages.BorrowDashboard.Main as BorrowDashboard
import Pages.LendDashboard.Main as LendDashboard
import Route
import Service as Service exposing (Service)
import Task
import Time exposing (Posix)
import Url exposing (Url)
import User exposing (User)
import Utility.Color as Color


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
    , time : Posix
    , zoneInfo : Maybe ZoneInfo
    , backdrop : Backdrop
    , key : Key
    , pools : Pools
    , user : Maybe User
    , page : Page
    , modal : Maybe Modal
    , service : Maybe Service
    }


type alias Flags =
    { width : Int
    , time : Int
    , hasBackdropSupport : Bool
    }


init : Flags -> Url -> Key -> ( Model, Cmd Msg )
init { width, time, hasBackdropSupport } url key =
    { device = Device.fromWidth width
    , visibility = Browser.Events.Visible
    , time = Time.millisToPosix time
    , zoneInfo = Nothing
    , backdrop = hasBackdropSupport |> Backdrop.setBackdrop
    , key = key
    , pools = Pools.whitelist Rinkeby
    , user = Nothing
    , page =
        AllMarket.init
            { pools = Pools.whitelist Rinkeby
            , user = Nothing
            }
            |> Page.AllMarket
    , modal = Nothing
    , service = Nothing
    }
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
    | ReceiveTime Posix
    | ReceiveZoneInfo ZoneInfo
    | AllMarketMsg AllMarket.Msg
    | LendDashboardMsg LendDashboard.Msg
    | BorrowDashboardMsg BorrowDashboard.Msg
    | ModalMsg Modal.Msg
    | ServiceMsg Service.Msg
    | MetamaskConnected Value
    | NoMetamask Value


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
            ( url
                |> Route.fromUrl model
                |> Maybe.map
                    (\route ->
                        case route of
                            Route.Page page ->
                                { model
                                    | device = model.device |> Device.closeAside
                                    , page =
                                        if Page.same model.page page then
                                            model.page

                                        else
                                            page
                                    , modal = Nothing
                                    , service = Nothing
                                }

                            Route.Modal modal ->
                                { model
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

                            Route.Service service ->
                                { model
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

                            Route.Aside ->
                                { model | device = model.device |> Device.openAside }

                            Route.Exit ->
                                if model.device |> Device.checkAsideStatus then
                                    { model | device = model.device |> Device.closeAside }

                                else
                                    model.service
                                        |> Maybe.map (\_ -> { model | service = Nothing })
                                        |> Maybe.withDefault
                                            (model.modal
                                                |> Maybe.map (\_ -> { model | modal = Nothing })
                                                |> Maybe.withDefault { model | page = Page.AllMarket (AllMarket.init model) }
                                            )
                    )
                |> Maybe.withDefault model
            , Cmd.none
            )

        ResizeWindow width _ ->
            ( { model | device = Device.fromDeviceWidth model.device width }
            , Cmd.none
            )

        ClickOutsideAside ->
            ( model
            , Route.exit model
            )

        VisibilityChange visibility ->
            ( { model | visibility = visibility }
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

        AllMarketMsg allMarketMsg ->
            model.page
                |> Page.updateAllMarket allMarketMsg
                |> (\( page, cmd ) ->
                        ( { model | page = page }
                        , cmd |> Cmd.map AllMarketMsg
                        )
                   )

        LendDashboardMsg lendDashboardMsg ->
            model.page
                |> Page.updateLendDashboard lendDashboardMsg
                |> (\( page, cmd ) ->
                        ( { model | page = page }
                        , cmd |> Cmd.map LendDashboardMsg
                        )
                   )

        BorrowDashboardMsg borrowDashboardMsg ->
            model.page
                |> Page.updateBorrowDashboard borrowDashboardMsg
                |> (\( page, cmd ) ->
                        ( { model | page = page }
                        , cmd |> Cmd.map BorrowDashboardMsg
                        )
                   )

        ModalMsg modalMsg ->
            model.modal
                |> Maybe.map (Modal.update modalMsg)
                |> Maybe.map
                    (\( modal, cmd ) ->
                        ( { model | modal = Just modal }
                        , cmd |> Cmd.map ModalMsg
                        )
                    )
                |> Maybe.withDefault ( model, Cmd.none )

        ServiceMsg serviceMsg ->
            ( model
            , Service.update serviceMsg
                |> Cmd.map ServiceMsg
            )

        MetamaskConnected value ->
            ( value
                |> Decode.decodeValue User.decoder
                |> Result.map
                    (\user -> { model | user = Just user })
                |> Result.withDefault model
            , Cmd.none
            )

        NoMetamask _ ->
            ( model
            , Navigation.pushUrl model.key "#nometamask"
            )


port metamaskConnected : (Value -> msg) -> Sub msg


port noMetamask : (Value -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onResize ResizeWindow
        , Browser.Events.onVisibilityChange VisibilityChange
        , Time.every 1000 ReceiveTime
        , onClickOutsideAside model
        , metamaskConnected MetamaskConnected
        , noMetamask NoMetamask
        ]


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


view : Model -> Document Msg
view model =
    { title = "Timeswap"
    , body = [ html model ]
    }


html : Model -> Html Msg
html model =
    if Device.isPhoneOrTablet model.device then
        layoutWith
            { options = [ option ] }
            ([ width <| minimum 340 fill
             , Background.color Color.dark400
             ]
                ++ (if Device.checkAsideStatus model.device then
                        [ Lazy.lazy Aside.view model |> inFront ]

                    else
                        model.service
                            |> Maybe.map
                                (\service ->
                                    [ Lazy.lazy2 Service.view model service
                                        |> Element.map ServiceMsg
                                        |> inFront
                                    ]
                                )
                            |> Maybe.map Just
                            |> Maybe.withDefault
                                (model.modal
                                    |> Maybe.map (\modal -> [{- Lazy.lazy2 (Debug.todo "Modal.view") model modal |> inFront -}])
                                )
                            |> Maybe.withDefault []
                   )
            )
            (column
                [ width fill
                , height fill
                , clip
                ]
                [ Lazy.lazy Header.view model

                --, Debug.todo "Page.view model"
                ]
            )

    else
        layoutWith
            { options = [ option ] }
            ([ width <| minimum 340 fill
             , Background.color Color.dark400
             , behindContent <| background
             ]
                ++ (model.service
                        |> Maybe.map
                            (\service ->
                                [ Lazy.lazy2 Service.view model service
                                    |> Element.map ServiceMsg
                                    |> inFront
                                ]
                            )
                        |> Maybe.map Just
                        |> Maybe.withDefault
                            (model.modal
                                |> Maybe.map (\modal -> [{- Lazy.lazy2 (Debug.todo "Modal.view") model modal |> inFront -}])
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
                [ Lazy.lazy Header.view model
                , row
                    [ width fill
                    , height fill
                    , clip
                    ]
                    [ Lazy.lazy Aside.view model

                    --, Debug.log "Page.view model"
                    ]
                ]


background : Element msg
background =
    el
        [ width fill
        , height fill
        , Background.image "./../image/Background.svg"
        ]
        none


option : Option
option =
    focusStyle
        { borderColor = Nothing
        , backgroundColor = Nothing
        , shadow = Nothing
        }
