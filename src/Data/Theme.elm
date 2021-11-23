module Data.Theme exposing (Flag, Theme, encode, init, switch)

import Json.Encode as Encode exposing (Value)


type Theme
    = Light
    | Dark


type alias Flag =
    Maybe String


init : Flag -> Theme
init maybeString =
    maybeString
        |> Maybe.andThen
            (\string ->
                case string of
                    "light" ->
                        Just Light

                    "dark" ->
                        Just Dark

                    _ ->
                        Nothing
            )
        |> Maybe.withDefault Dark


encode : Theme -> Value
encode mode =
    (case mode of
        Light ->
            "light"

        Dark ->
            "dark"
    )
        |> Encode.string


switch : Theme -> Theme
switch theme =
    case theme of
        Light ->
            Dark

        Dark ->
            Light
