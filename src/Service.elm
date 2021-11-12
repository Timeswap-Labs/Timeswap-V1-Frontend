module Service exposing
    ( Msg
    , Service
    , fromFragment
    , getDeadline
    , getSlippage
    , hasDeadlineInput
    , hasSlippageInput
    , inputDeadline
    , inputSlippage
    , refreshSettings
    , same
    , subscriptions
    , toUrl
    , update
    , view
    )

import Browser.Navigation exposing (Key)
import Data.Address exposing (Address)
import Data.Backdrop exposing (Backdrop)
import Data.Balances exposing (Balances)
import Data.Chain exposing (Chain(..))
import Data.Deadline as Deadline exposing (Deadline)
import Data.Device as Device exposing (Device)
import Data.Images exposing (Images)
import Data.Or as Or exposing (Or(..))
import Data.Remote exposing (Remote(..))
import Data.Slippage as Slippage exposing (Slippage)
import Data.TokenImages exposing (TokenImages)
import Data.Tokens exposing (Tokens)
import Element
    exposing
        ( Element
        , el
        , fill
        , height
        , none
        , padding
        , paddingEach
        , scrollbarY
        , width
        )
import Element.Background as Background
import Element.Font as Font
import Element.Lazy as Lazy
import Modal exposing (Modal)
import Page exposing (Page)
import Services.Connect.Main as Connect
import Services.Faucet.Main as Faucet
import Services.NoMetamask.Main as NoMetamask
import Services.Settings.Main as Settings exposing (Settings)
import Services.Swap.Main as Swap
import Services.Wallet.Main as Wallet
import Utility.Color as Color
import Utility.Router as Router
import Utility.Typography as Typography


type Service
    = Connect
    | NoMetamask
    | Wallet Wallet.Service
    | Settings Settings.Service Settings
    | Faucet
    | Swap Swap.Service


fromFragment : { model | user : Remote userError { user | chain : Chain } } -> String -> Maybe Service
fromFragment { user } string =
    case ( string, user ) of
        ( "connect", Loading ) ->
            Just Connect

        ( "connect", Failure _ ) ->
            Just Connect

        ( "nometamask", Loading ) ->
            Just NoMetamask

        ( "nometamask", Failure _ ) ->
            Just NoMetamask

        ( "wallet", Success _ ) ->
            Wallet Wallet.init |> Just

        ( "settings", _ ) ->
            Settings Settings.init Settings.initSettings |> Just

        ( "faucet", _ ) ->
            Just Faucet

        ( "swap", _ ) ->
            Swap.init
                |> Swap
                |> Just

        _ ->
            Nothing


toUrl : Service -> String
toUrl service =
    case service of
        Connect ->
            Router.toConnect

        NoMetamask ->
            Router.toNoMetamask

        Wallet _ ->
            Router.toWallet

        Settings _ _ ->
            Router.toSettings

        Faucet ->
            Router.toFaucet

        Swap _ ->
            Router.toSwap


same : Service -> Service -> Bool
same service1 service2 =
    case ( service1, service2 ) of
        ( Connect, Connect ) ->
            True

        ( NoMetamask, NoMetamask ) ->
            True

        ( Wallet _, Wallet _ ) ->
            True

        ( Settings _ _, Settings _ _ ) ->
            True

        ( Faucet, Faucet ) ->
            True

        ( Swap _, Swap _ ) ->
            True

        _ ->
            False


type Msg
    = ConnectMsg Connect.Msg
    | WalletMsg Wallet.Msg
    | SettingsMsg Settings.Msg
    | FaucetMsg Faucet.Msg
    | SwapMsg Swap.Msg


update :
    { model
        | key : Key
        , page : Page
        , modal : Maybe Modal
    }
    -> Msg
    -> Service
    -> ( Service, Cmd Msg )
update model msg service =
    case ( msg, service ) of
        ( ConnectMsg connectMsg, Connect ) ->
            ( Connect
            , Connect.update model connectMsg
                |> Cmd.map ConnectMsg
            )

        ( WalletMsg walletMsg, Wallet wallet ) ->
            wallet
                |> Wallet.update walletMsg
                |> (\( updatedWallet, cmd ) ->
                        ( Wallet updatedWallet
                        , cmd |> Cmd.map WalletMsg
                        )
                   )

        ( SettingsMsg settingsMsg, Settings _ settings ) ->
            ( Settings (Settings.update settingsMsg) settings
            , Cmd.none
            )

        ( FaucetMsg faucetMsg, Faucet ) ->
            ( Faucet
            , Faucet.update faucetMsg
                |> Cmd.map FaucetMsg
            )

        ( SwapMsg swapMsg, Swap swap ) ->
            swap
                |> Swap.update swapMsg
                |> (\( updatedSwap, cmd ) ->
                        ( updatedSwap |> Swap
                        , cmd |> Cmd.map SwapMsg
                        )
                   )

        _ ->
            ( service, Cmd.none )


subscriptions : Service -> Sub Msg
subscriptions service =
    case service of
        Swap swap ->
            Swap.subscriptions swap
                |> Sub.map SwapMsg

        _ ->
            Sub.none


inputSlippage : String -> Service -> Service
inputSlippage string service =
    case service of
        Settings settingsService settings ->
            settings
                |> Settings.inputSlippage string
                |> Settings settingsService

        _ ->
            service


inputDeadline : String -> Service -> Service
inputDeadline string service =
    case service of
        Settings settingsService settings ->
            settings
                |> Settings.inputDeadline string
                |> Settings settingsService

        _ ->
            service


getSlippage : Service -> Maybe Slippage
getSlippage service =
    case service of
        Settings _ settings ->
            settings |> Settings.getSlippage

        _ ->
            Nothing


getDeadline : Service -> Maybe Deadline
getDeadline service =
    case service of
        Settings _ settings ->
            settings |> Settings.getDeadline

        _ ->
            Nothing


refreshSettings : Service
refreshSettings =
    Settings.initSettings
        |> Settings Settings.init


hasSlippageInput : Service -> Bool
hasSlippageInput service =
    case service of
        Settings _ settings ->
            settings |> Settings.hasSlippageInput

        _ ->
            False


hasDeadlineInput : Service -> Bool
hasDeadlineInput service =
    case service of
        Settings _ settings ->
            settings |> Settings.hasDeadlineInput

        _ ->
            False


view :
    { msgs
        | exitSettings : msg
        , chooseSlippageOption : Slippage.Option -> msg
        , chooseDeadlineOption : Deadline.Option -> msg
        , inputSlippage : String -> msg
        , inputDeadline : String -> msg
    }
    ->
        { model
            | device : Device
            , backdrop : Backdrop
            , slippage : Slippage
            , deadline : Deadline
            , tokens : Tokens
            , images : Images
            , tokenImages : TokenImages
            , user : Remote userError { user | address : Address, balances : Remote () Balances }
        }
    -> Service
    -> Element (Or Msg msg)
view msgs ({ device, user } as model) service =
    case ( service, user ) of
        ( Connect, Loading ) ->
            el
                [ width fill
                , height fill
                , if Device.isPhone device then
                    paddingEach
                        { top = 160
                        , right = 0
                        , bottom = 0
                        , left = 0
                        }

                  else
                    padding 80
                , scrollbarY
                , Background.color Color.modal
                , Font.family Typography.supreme
                ]
                (Lazy.lazy Connect.view model)
                |> Element.map ConnectMsg
                |> Element.map Either

        ( Connect, Failure _ ) ->
            el
                [ width fill
                , height fill
                , if Device.isPhone device then
                    paddingEach
                        { top = 160
                        , right = 0
                        , bottom = 0
                        , left = 0
                        }

                  else
                    padding 80
                , scrollbarY
                , Background.color Color.modal
                , Font.family Typography.supreme
                ]
                (Lazy.lazy Connect.view model)
                |> Element.map ConnectMsg
                |> Element.map Either

        ( NoMetamask, Loading ) ->
            el
                [ width fill
                , height fill
                , if Device.isPhone device then
                    paddingEach
                        { top = 160
                        , right = 0
                        , bottom = 0
                        , left = 0
                        }

                  else
                    padding 80
                , scrollbarY
                , Background.color Color.modal
                , Font.family Typography.supreme
                ]
                (Lazy.lazy NoMetamask.view model)
                |> Element.map Either

        ( NoMetamask, Failure _ ) ->
            el
                [ width fill
                , height fill
                , if Device.isPhone device then
                    paddingEach
                        { top = 160
                        , right = 0
                        , bottom = 0
                        , left = 0
                        }

                  else
                    padding 80
                , scrollbarY
                , Background.color Color.modal
                , Font.family Typography.supreme
                ]
                (Lazy.lazy NoMetamask.view model)
                |> Element.map Either

        ( Wallet wallet, Success successUser ) ->
            el
                [ width fill
                , height fill
                , if Device.isPhone device then
                    paddingEach
                        { top = 160
                        , right = 0
                        , bottom = 0
                        , left = 0
                        }

                  else
                    padding 80
                , scrollbarY
                , Background.color Color.modal
                , Font.family Typography.supreme
                ]
                (Lazy.lazy3 Wallet.view model successUser wallet)
                |> Element.map WalletMsg
                |> Element.map Either

        ( Settings settingsService settings, _ ) ->
            el
                [ width fill
                , height fill
                , if Device.isPhone device then
                    paddingEach
                        { top = 160
                        , right = 0
                        , bottom = 0
                        , left = 0
                        }

                  else
                    padding 80
                , scrollbarY
                , Background.color Color.modal
                , Font.family Typography.supreme
                ]
                (Lazy.lazy4 Settings.view msgs model settingsService settings)
                |> (Element.map << Or.mapEither) SettingsMsg

        ( Faucet, _ ) ->
            el
                [ width fill
                , height fill
                , if Device.isPhone device then
                    paddingEach
                        { top = 160
                        , right = 0
                        , bottom = 0
                        , left = 0
                        }

                  else
                    padding 80
                , scrollbarY
                , Background.color Color.modal
                , Font.family Typography.supreme
                ]
                (Lazy.lazy Faucet.view model)
                |> Element.map FaucetMsg
                |> Element.map Either

        ( Swap swap, Success successUser ) ->
            el
                [ width fill
                , height fill
                , if Device.isPhone device then
                    paddingEach
                        { top = 160
                        , right = 0
                        , bottom = 0
                        , left = 0
                        }

                  else
                    padding 80
                , scrollbarY
                , Background.color Color.modal
                , Font.family Typography.supreme
                ]
                (Lazy.lazy3 Swap.view model successUser swap)
                |> Element.map SwapMsg
                |> Element.map Either

        _ ->
            none
