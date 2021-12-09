module Data.ZoneName exposing (Flag, ZoneName, init, toString)

import Data.Offset as Offset exposing (Offset)


type alias ZoneName =
    String


type alias Flag =
    Maybe String


init : Flag -> Maybe ZoneName
init =
    identity


toString : Offset -> Maybe ZoneName -> String
toString offset zoneName =
    zoneName
        |> Maybe.withDefault
            (offset |> Offset.toString)
