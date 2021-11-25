module Data.Spot exposing
    ( Flag
    , Spot(..)
    , encode
    , init
    )

import Json.Encode as Encode exposing (Value)


type Spot
    = None
    | Uniswap


type alias Flag =
    Maybe String


init : Flag -> Spot
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


encode :
    Spot
    -> Value
encode oracle =
    (case oracle of
        None ->
            "none"

        Uniswap ->
            "uniswap"
    )
        |> Encode.string
