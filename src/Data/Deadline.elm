module Data.Deadline exposing
    ( Deadline
    , Flag
    , Option(..)
    , encode
    , encodeUnix
    , fromSettings
    , init
    , isCorrect
    , toOption
    , toSettings
    , toString
    , toUnix
    )

import Data.Or exposing (Or(..))
import Json.Encode as Encode exposing (Value)
import Time exposing (Posix)


type Deadline
    = Deadline Int


type alias Flag =
    Maybe Int


type Option
    = Short
    | Medium
    | Long


init : Flag -> Deadline
init maybeMinutes =
    maybeMinutes
        |> Maybe.andThen
            (\minutes ->
                if minutes > 0 && minutes <= 180 then
                    minutes
                        |> Deadline
                        |> Just

                else
                    Nothing
            )
        |> Maybe.withDefault (Deadline 20)


encode : Deadline -> Value
encode (Deadline minutes) =
    minutes
        |> Encode.int


encodeUnix : Posix -> Deadline -> Value
encodeUnix time deadline =
    deadline
        |> toUnix time
        |> Encode.int


toUnix : Posix -> Deadline -> Int
toUnix time (Deadline int) =
    time
        |> Time.posixToMillis
        |> (\millis -> millis // 1000)
        |> (+) (int * 60)


toSettings : Deadline -> Or Option String
toSettings (Deadline minutes) =
    if minutes == 10 then
        Short |> Left

    else if minutes == 20 then
        Medium |> Left

    else if minutes == 30 then
        Long |> Left

    else
        minutes
            |> String.fromInt
            |> Right


fromSettings : Or Option String -> Deadline
fromSettings or =
    case or of
        Left Short ->
            Deadline 10

        Left Medium ->
            Deadline 20

        Left Long ->
            Deadline 30

        Right string ->
            string
                |> String.toInt
                |> Maybe.andThen
                    (\minutes ->
                        if minutes > 0 && minutes <= 180 then
                            minutes
                                |> Deadline
                                |> Just

                        else
                            Nothing
                    )
                |> Maybe.withDefault (Deadline 20)


toOption : Or Option string -> Maybe Option
toOption or =
    case or of
        Left option ->
            Just option

        _ ->
            Nothing


toString : Or option String -> Maybe String
toString or =
    case or of
        Right string ->
            Just string

        _ ->
            Nothing


isCorrect : String -> Bool
isCorrect string =
    string
        |> String.toInt
        |> Maybe.map
            (\minutes ->
                if minutes > 0 && minutes <= 180 then
                    True

                else
                    False
            )
        |> Maybe.withDefault False
