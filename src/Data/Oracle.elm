module Data.Oracle exposing (Flag, Oracle(..), encode, init)

import Json.Encode as Encode exposing (Value)


type Oracle
    = None
    | Uniswap


type alias Flag =
    Maybe String


init : Flag -> Oracle
init maybeString =
    maybeString
        |> Maybe.andThen
            (\string ->
                case string of
                    "none" ->
                        Just None

                    "uniswap" ->
                        Just Uniswap

                    _ ->
                        Nothing
            )
        |> Maybe.withDefault Uniswap


encode : Oracle -> Value
encode oracle =
    (case oracle of
        None ->
            "none"

        Uniswap ->
            "uniswap"
    )
        |> Encode.string
