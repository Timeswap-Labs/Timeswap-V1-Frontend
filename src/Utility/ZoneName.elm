module Utility.ZoneName exposing (toString)

import Time exposing (ZoneName)


toString : ZoneName -> String
toString zoneName =
    case zoneName of
        Time.Name name ->
            name

        Time.Offset offset ->
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
