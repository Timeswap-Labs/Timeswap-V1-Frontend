module Data.Offset exposing (Flag, Offset, init, toString, toZone)

import Time exposing (Zone)


type alias Offset =
    Int


type alias Flag =
    Int


init : Flag -> Offset
init =
    identity


toZone : Offset -> Zone
toZone offset =
    Time.customZone -offset []


toString : Offset -> String
toString offset =
    if offset >= 0 then
        "UTC+"
            ++ (offset // 60 |> String.fromInt)
            ++ ":"
            ++ (remainderBy 60 offset |> String.fromInt)

    else
        "UTC-"
            ++ (offset // 60 |> negate |> String.fromInt)
            ++ ":"
            ++ (remainderBy 60 offset |> negate |> String.fromInt)
