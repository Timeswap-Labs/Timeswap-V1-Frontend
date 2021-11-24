module Page.Route exposing (Route(..), fromTab, fromUrl, toUrlString)

import Blockchain.Main as Blockchain exposing (Blockchain)
import Data.Chains exposing (Chains)
import Data.Parameter as Parameter exposing (Parameter)
import Data.Support exposing (Support(..))
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
    Support userNotSupported Blockchain
    -> Chains
    -> Url
    -> Maybe Route
fromUrl blockchain chains =
    fromFragment blockchain chains
        |> Parser.fragment
        |> Parser.parse


fromFragment :
    Support userNotSupported Blockchain
    -> Chains
    -> Maybe String
    -> Route
fromFragment blockchain chains fragment =
    case
        ( case blockchain of
            Supported block ->
                block
                    |> Blockchain.toChain
                    |> Just

            _ ->
                Nothing
        , fragment |> Maybe.map (String.split "?")
        )
    of
        ( Just chain, Just ("lend" :: parameters :: _) ) ->
            parameters
                |> Parameter.fromFragment chain chains
                |> Lend

        ( Just _, Just ("lend" :: _) ) ->
            Nothing |> Lend

        ( Nothing, Just ("lend" :: _) ) ->
            Nothing |> Lend

        ( Just chain, Just ("borrow" :: parameters :: _) ) ->
            parameters
                |> Parameter.fromFragment chain chains
                |> Borrow

        ( Just _, Just ("borrow" :: _) ) ->
            Nothing |> Borrow

        ( Nothing, Just ("borrow" :: _) ) ->
            Nothing |> Borrow

        ( Just chain, Just ("liquidity" :: parameters :: _) ) ->
            parameters
                |> Parameter.fromFragment chain chains
                |> Liquidity

        ( Just _, Just ("liquidity" :: _) ) ->
            Nothing |> Liquidity

        ( Nothing, Just ("liquidity" :: _) ) ->
            Nothing |> Liquidity

        _ ->
            Nothing |> Lend
