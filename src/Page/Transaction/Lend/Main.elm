module Page.Transaction.Lend.Main exposing
    ( Section
    , init
    , toParameter
    )

import Data.Pair exposing (Pair)
import Data.Parameter as Parameter exposing (Parameter)
import Data.Pool exposing (Pool)
import Data.Token exposing (Token)


type Section
    = None
    | Asset Token
    | Collateral Token
    | Pair Pair
    | Pool Pool


init : Maybe Parameter -> Section
init parameter =
    case parameter of
        Nothing ->
            None

        Just (Parameter.Asset asset) ->
            Asset asset

        Just (Parameter.Collateral collateral) ->
            Collateral collateral

        Just (Parameter.Pair pair) ->
            Pair pair

        Just (Parameter.Pool pool) ->
            Pool pool


toParameter : Section -> Maybe Parameter
toParameter section =
    case section of
        None ->
            Nothing

        Asset asset ->
            Parameter.Asset asset
                |> Just

        Collateral collateral ->
            Parameter.Collateral collateral
                |> Just

        Pair pair ->
            Parameter.Pair pair
                |> Just

        Pool pool ->
            Parameter.Pool pool
                |> Just
