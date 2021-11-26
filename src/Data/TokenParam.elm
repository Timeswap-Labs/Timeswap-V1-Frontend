module Data.TokenParam exposing (TokenParam(..), fromFragment, toQueryParameter)

import Url.Builder as Builder exposing (QueryParameter)


type TokenParam
    = Asset
    | Collateral


toQueryParameter : TokenParam -> String -> QueryParameter
toQueryParameter tokenParam =
    (case tokenParam of
        Asset ->
            "asset"

        Collateral ->
            "collateral"
    )
        |> Builder.string


fromFragment : String -> Maybe TokenParam
fromFragment fragment =
    case fragment of
        "asset" ->
            Just Asset

        "collateral" ->
            Just Collateral

        _ ->
            Nothing
