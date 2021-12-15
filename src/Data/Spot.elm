module Data.Spot exposing
    ( Flag
    , Spot(..)
    , encode
    , init
    )

import Json.Encode as Encode exposing (Value)


type Spot
    = None
    | Utilize


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
                        Just Utilize

                    _ ->
                        Nothing
            )
        |> Maybe.withDefault Utilize


encode :
    Spot
    -> Value
encode oracle =
    (case oracle of
        None ->
            "none"

        Utilize ->
            "uniswap"
    )
        |> Encode.string
