module Page.Route exposing (Route(..), fromTab, fromUrl, toUrlString)

import Blockchain.Main as Blockchain exposing (Blockchain)
import Data.Chains exposing (Chains)
import Data.Parameter as Parameter exposing (Parameter)
import Data.Tab as Tab exposing (Tab)
import Url exposing (Url)
import Url.Builder as Builder
import Url.Parser as Parser


type Route
    = Lend (Maybe Parameter)
    | Borrow (Maybe Parameter)
    | Liquidity (Maybe Parameter)


fromTab : Tab -> Maybe Parameter -> Route
fromTab tab parameter =
    case tab of
        Tab.Lend ->
            Lend parameter

        Tab.Borrow ->
            Borrow parameter

        Tab.Liquidity ->
            Liquidity parameter


toUrlString : Route -> String
toUrlString route =
    Builder.absolute
        [ case route of
            Lend parameter ->
                parameter
                    |> Maybe.map Parameter.toFragment
                    |> Maybe.map ((++) "?")
                    |> Maybe.withDefault ""
                    |> (++) "lend"

            Borrow parameter ->
                parameter
                    |> Maybe.map Parameter.toFragment
                    |> Maybe.map ((++) "?")
                    |> Maybe.withDefault ""
                    |> (++) "borrow"

            Liquidity parameter ->
                parameter
                    |> Maybe.map Parameter.toFragment
                    |> Maybe.map ((++) "?")
                    |> Maybe.withDefault ""
                    |> (++) "liquidity"
        ]
        []


fromUrl :
    Blockchain
    -> Chains
    -> Url
    -> Maybe Route
fromUrl blockchain chains =
    fromFragment blockchain chains
        |> Parser.fragment
        |> Parser.parse


fromFragment :
    Blockchain
    -> Chains
    -> Maybe String
    -> Route
fromFragment blockchain chains fragment =
    case blockchain of
        Blockchain.Supported { chain } ->
            fragment
                |> Maybe.map
                    (\string ->
                        case string |> String.split "?" of
                            "lend" :: parameters :: _ ->
                                parameters
                                    |> Parameter.fromFragment chain chains
                                    |> Lend

                            "lend" :: _ ->
                                Nothing |> Lend

                            "borrow" :: parameters :: _ ->
                                parameters
                                    |> Parameter.fromFragment chain chains
                                    |> Borrow

                            "borrow" :: _ ->
                                Nothing |> Borrow

                            "liquidity" :: parameters :: _ ->
                                parameters
                                    |> Parameter.fromFragment chain chains
                                    |> Liquidity

                            "liquidity" :: _ ->
                                Nothing |> Liquidity

                            _ ->
                                Nothing |> Lend
                    )
                |> Maybe.withDefault (Nothing |> Lend)

        Blockchain.NotSupported _ ->
            Nothing |> Lend
