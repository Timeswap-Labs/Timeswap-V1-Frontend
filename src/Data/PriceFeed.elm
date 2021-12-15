module Data.PriceFeed exposing
    ( Flag
    , PriceFeed(..)
    , encode
    , init
    )

import Json.Encode as Encode exposing (Value)


type PriceFeed
    = Ignore
    | Utilize


type alias Flag =
    Maybe String


init : Flag -> PriceFeed
init maybeString =
    maybeString
        |> Maybe.andThen
            (\string ->
                case string of
                    "ignore" ->
                        Just Ignore

                    "utilize" ->
                        Just Utilize

                    _ ->
                        Nothing
            )
        |> Maybe.withDefault Utilize


encode :
    PriceFeed
    -> Value
encode oracle =
    (case oracle of
        Ignore ->
            "ignore"

        Utilize ->
            "utilize"
    )
        |> Encode.string
