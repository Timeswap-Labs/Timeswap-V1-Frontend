module Page.Query exposing (toCustomUrlString, toUrlString)

import Data.Chain as Chain exposing (Chain)
import Data.Maturity as Maturity
import Data.Pair as Pair
import Data.Pool exposing (Pool)
import Data.Token as Token
import Data.TokenParam as TokenParam
import Url.Builder as Builder


toUrlString : Chain -> String -> Pool -> String
toUrlString chain endPoint { pair, maturity } =
    Builder.crossOrigin
        endPoint
        [ "pool" ]
        [ chain |> Chain.toQueryParameter
        , pair
            |> Pair.toAsset
            |> Token.toQueryParameter TokenParam.Asset
        , pair
            |> Pair.toCollateral
            |> Token.toQueryParameter TokenParam.Collateral
        , maturity
            |> Maturity.toQueryParameter
        ]


toCustomUrlString : Chain -> String -> Pool -> String
toCustomUrlString chain endPoint { pair, maturity } =
    Builder.crossOrigin
        (decision (Maturity.toUnix maturity) endPoint)
        [ "pool" ]
        [ chain |> Chain.toQueryParameter
        , pair
            |> Pair.toAsset
            |> Token.toQueryParameter TokenParam.Asset
        , pair
            |> Pair.toCollateral
            |> Token.toQueryParameter TokenParam.Collateral
        , maturity
            |> Maturity.toQueryParameter
        ]


decision : Int -> String -> String
decision maturity endPoint =
    case maturity of
        1648483200 ->
            "https://ts-mainnet-week1.herokuapp.com/v1"

        1649412000 ->
            "https://ts-mainnet-week2-3.herokuapp.com/v1"

        1650366000 ->
            "https://ts-mainnet-week2-3.herokuapp.com/v1"

        1651149000 ->
            "https://ts-mainnet-week-4-7.herokuapp.com/v1"

        1652061600 ->
            "https://ts-mainnet-week-4-7.herokuapp.com/v1"

        1652709600 ->
            "https://ts-mainnet-week-4-7.herokuapp.com/v1"

        1653318000 ->
            "https://ts-mainnet-week-4-7.herokuapp.com/v1"

        1654009200 ->
            "https://ts-mainnet-week8.herokuapp.com/v1"

        1654614000 ->
            "https://ts-gamification-api.herokuapp.com/v1"

        1655218800 ->
            "https://ts-gamification-api.herokuapp.com/v1"

        1655906400 ->
            "https://ts-gamification-api.herokuapp.com/v1"

        _ ->
            endPoint
