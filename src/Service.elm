module Service exposing (Service(..), fromFragment, same, toUrl, view)

import Data.Chain exposing (Chain(..))
import Data.Device as Device exposing (Device)
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
import Services.Connect.Main as Connect
import Services.Faucet.Main as Faucet
import Services.NoMetamask.Main as NoMetamask
import Services.Settings.Main as Settings
import Services.Wallet.Main as Wallet
import Utility.Color as Color


type Service
    = Connect
    | NoMetamask
    | Wallet
    | Settings
        { slippage : Maybe String
        , deadline : Maybe String
        }
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

        "changesettings" ->
            { slippage = Nothing
            , deadline = Nothing
            }
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
            Connect.toUrl

        NoMetamask ->
            NoMetamask.toUrl

        Wallet ->
            Wallet.toUrl

        Settings _ ->
            Settings.toUrl

        Faucet ->
            Faucet.toUrl


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


view ({ device } as model) service =
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
                ]
                (Connect.view model)

        _ ->
            none
