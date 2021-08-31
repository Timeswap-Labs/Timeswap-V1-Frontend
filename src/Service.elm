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
    , toUrl
    , update
    , view
    )

import Data.Address exposing (Address)
import Data.Backdrop exposing (Backdrop)
import Data.Balances exposing (Balances)
import Data.Chain exposing (Chain(..))
import Data.Deadline as Deadline exposing (Deadline)
import Data.Device as Device exposing (Device)
import Data.Or exposing (Or(..))
import Data.Remote exposing (Remote)
import Data.Slippage as Slippage exposing (Slippage)
import Element
    exposing
        ( Element
        , el
        , fill
        , height
        , none
        , padding
        , scrollbarY
        , width
        )
import Element.Background as Background
import Element.Font as Font
import Element.Lazy as Lazy
import Services.Connect.Main as Connect
import Services.Faucet.Main as Faucet
import Services.NoMetamask.Main as NoMetamask
import Services.Settings.Main as Settings exposing (Settings)
import Services.Wallet.Main as Wallet
import Utility.Color as Color
import Utility.Router as Router
import Utility.Typography as Typography


type Service
    = Connect
    | NoMetamask
    | Wallet
    | Settings Settings
    | Faucet


fromFragment : { model | user : Maybe { user | chain : Chain } } -> String -> Maybe Service
fromFragment { user } string =
    case string of
        "connect" ->
            user
                |> Maybe.map (\_ -> Nothing)
                |> Maybe.withDefault (Just Connect)

        "nometamask" ->
            user
                |> Maybe.map (\_ -> Nothing)
                |> Maybe.withDefault (Just NoMetamask)

        "wallet" ->
            user
                |> Maybe.andThen (\_ -> Just Wallet)

        "settings" ->
            Settings.init
                |> Settings
                |> Just

        "faucet" ->
            user
                |> Maybe.map .chain
                |> Maybe.withDefault Rinkeby
                |> (\chain ->
                        case chain of
                            Mainnet ->
                                Nothing

                            Rinkeby ->
                                Just Faucet
                   )

        _ ->
            Nothing


toUrl : Service -> String
toUrl service =
    case service of
        Connect ->
            Router.toConnect

        NoMetamask ->
            Router.toNoMetamask

        Wallet ->
            Router.toWallet

        Settings _ ->
            Router.toSettings

        Faucet ->
            Router.toFaucet


same : Service -> Service -> Bool
same service1 service2 =
    case ( service1, service2 ) of
        ( Connect, Connect ) ->
            True

        ( NoMetamask, NoMetamask ) ->
            True

        ( Wallet, Wallet ) ->
            True

        ( Settings _, Settings _ ) ->
            True

        ( Faucet, Faucet ) ->
            True

        _ ->
            False


type Msg
    = ConnectMsg Connect.Msg


update : Msg -> Cmd Msg
update msg =
    case msg of
        ConnectMsg connectMsg ->
            Connect.update connectMsg
                |> Cmd.map ConnectMsg


inputSlippage : String -> Service -> Service
inputSlippage string service =
    case service of
        Settings settings ->
            settings
                |> Settings.inputSlippage string
                |> Settings

        _ ->
            service


inputDeadline : String -> Service -> Service
inputDeadline string service =
    case service of
        Settings settings ->
            settings
                |> Settings.inputDeadline string
                |> Settings

        _ ->
            service


getSlippage : Service -> Maybe Slippage
getSlippage service =
    case service of
        Settings settings ->
            settings |> Settings.getSlippage

        _ ->
            Nothing


getDeadline : Service -> Maybe Deadline
getDeadline service =
    case service of
        Settings settings ->
            settings |> Settings.getDeadline

        _ ->
            Nothing


refreshSettings : Service
refreshSettings =
    Settings.init
        |> Settings


hasSlippageInput : Service -> Bool
hasSlippageInput service =
    case service of
        Settings settings ->
            settings |> Settings.hasSlippageInput

        _ ->
            False


hasDeadlineInput : Service -> Bool
hasDeadlineInput service =
    case service of
        Settings settings ->
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
        , disconnect : msg
    }
    ->
        { model
            | device : Device
            , backdrop : Backdrop
            , slippage : Slippage
            , deadline : Deadline
            , user : Maybe { user | address : Address, balances : Remote Balances }
        }
    -> Service
    -> Or (Element Msg) (Element msg)
view msgs ({ device, user } as model) service =
    case service of
        Connect ->
            el
                [ width fill
                , height fill
                , if Device.isPhone device then
                    padding 0

                  else
                    padding 80
                , scrollbarY
                , Background.color Color.modal
                , Font.family Typography.supreme
                ]
                (Lazy.lazy Connect.view model)
                |> Element.map ConnectMsg
                |> Either

        NoMetamask ->
            user
                |> Maybe.map (\_ -> none |> Either)
                |> Maybe.withDefault
                    (el
                        [ width fill
                        , height fill
                        , if Device.isPhone device then
                            padding 0

                          else
                            padding 80
                        , scrollbarY
                        , Background.color Color.modal
                        , Font.family Typography.supreme
                        ]
                        (Lazy.lazy NoMetamask.view model)
                        |> Either
                    )

        Wallet ->
            user
                |> Maybe.map
                    (\userJust ->
                        el
                            [ width fill
                            , height fill
                            , if Device.isPhone device then
                                padding 0

                              else
                                padding 80
                            , scrollbarY
                            , Background.color Color.modal
                            , Font.family Typography.supreme
                            ]
                            (Lazy.lazy3 Wallet.view msgs model userJust)
                            |> Or
                    )
                |> Maybe.withDefault (none |> Either)

        Settings settings ->
            el
                [ width fill
                , height fill
                , if Device.isPhone device then
                    padding 0

                  else
                    padding 80
                , scrollbarY
                , Background.color Color.modal
                , Font.family Typography.supreme
                ]
                (Lazy.lazy3 Settings.view msgs model settings)
                |> Or

        Faucet ->
            el
                [ width fill
                , height fill
                , if Device.isPhone device then
                    padding 0

                  else
                    padding 80
                , scrollbarY
                , Background.color Color.modal
                , Font.family Typography.supreme
                ]
                (Lazy.lazy Faucet.view model)
                |> Either
