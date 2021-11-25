module Data.ChosenZone exposing (ChosenZone(..), Flag, encode, init, switch)

import Json.Encode as Encode exposing (Value)


type ChosenZone
    = Here
    | UTC
    | Unix


type alias Flag =
    Maybe String


init : Flag -> ChosenZone
init maybeString =
    maybeString
        |> Maybe.andThen
            (\string ->
                case string of
                    "here" ->
                        Just Here

                    "utc" ->
                        Just UTC

                    "unix" ->
                        Just Unix

                    _ ->
                        Nothing
            )
        |> Maybe.withDefault Here


encode : ChosenZone -> Value
encode chosenZone =
    (case chosenZone of
        Here ->
            "here"

        UTC ->
            "utc"

        Unix ->
            "unix"
    )
        |> Encode.string


switch : ChosenZone -> ChosenZone
switch chosenZone =
    case chosenZone of
        Here ->
            UTC

        UTC ->
            Unix

        Unix ->
            Here
