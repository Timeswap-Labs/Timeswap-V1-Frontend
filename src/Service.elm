module Service exposing (Service(..), fromFragment, toFragment)

import Data.Chain exposing (Chain(..))


type Service
    = Connect
    | NoMetamask
    | Wallet
    | ChangeSettings
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
                |> ChangeSettings
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


toFragment : Service -> String
toFragment service =
    case service of
        Connect ->
            "connect"

        NoMetamask ->
            "nometamask"

        Wallet ->
            "wallet"

        ChangeSettings _ ->
            "changesettings"

        Faucet ->
            "faucet"
