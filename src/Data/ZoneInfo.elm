module Data.ZoneInfo exposing
    ( ZoneInfo
    , toDateString
    , toString
    , toTimeString
    )

import Time exposing (Month(..), Posix, Zone, ZoneName)


type alias ZoneInfo =
    { zone : Zone
    , zoneName : ZoneName
    }


toString : Maybe ZoneInfo -> Posix -> String
toString maybeZoneInfo posix =
    [ posix |> toDateString maybeZoneInfo
    , posix |> toTimeString maybeZoneInfo
    ]
        |> String.join " "


toDateString : Maybe ZoneInfo -> Posix -> String
toDateString maybeZoneInfo posix =
    case maybeZoneInfo of
        Just { zone } ->
            let
                day : String
                day =
                    posix
                        |> Time.toDay zone
                        |> String.fromInt

                month : String
                month =
                    posix
                        |> Time.toMonth zone
                        |> fromMonthToString

                year : String
                year =
                    posix
                        |> Time.toYear zone
                        |> String.fromInt
            in
            day
                ++ " "
                ++ month
                ++ " "
                ++ year

        Nothing ->
            let
                day : String
                day =
                    posix
                        |> Time.toDay Time.utc
                        |> String.fromInt

                month : String
                month =
                    posix
                        |> Time.toMonth Time.utc
                        |> fromMonthToString

                year : String
                year =
                    posix
                        |> Time.toYear Time.utc
                        |> String.fromInt
            in
            day
                ++ " "
                ++ month
                ++ " "
                ++ year


toTimeString : Maybe ZoneInfo -> Posix -> String
toTimeString maybeZoneInfo posix =
    case maybeZoneInfo of
        Just { zone, zoneName } ->
            let
                hour : String
                hour =
                    posix
                        |> Time.toHour zone
                        |> String.fromInt
                        |> String.padLeft 2 '0'

                minute : String
                minute =
                    posix
                        |> Time.toMinute zone
                        |> String.fromInt
                        |> String.padLeft 2 '0'

                second : String
                second =
                    posix
                        |> Time.toSecond zone
                        |> String.fromInt
                        |> String.padLeft 2 '0'

                timeZone : String
                timeZone =
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
            in
            hour
                ++ ":"
                ++ minute
                ++ ":"
                ++ second
                ++ " "
                ++ timeZone

        Nothing ->
            let
                hour : String
                hour =
                    posix
                        |> Time.toHour Time.utc
                        |> String.fromInt

                minute : String
                minute =
                    posix
                        |> Time.toMinute Time.utc
                        |> String.fromInt

                second : String
                second =
                    posix
                        |> Time.toSecond Time.utc
                        |> String.fromInt
            in
            hour
                ++ ":"
                ++ minute
                ++ ":"
                ++ second
                ++ " "
                ++ "UTC"


fromMonthToString : Month -> String
fromMonthToString month =
    case month of
        Jan ->
            "January"

        Feb ->
            "February"

        Mar ->
            "March"

        Apr ->
            "April"

        May ->
            "May"

        Jun ->
            "Jun"

        Jul ->
            "July"

        Aug ->
            "August"

        Sep ->
            "September"

        Oct ->
            "October"

        Nov ->
            "November"

        Dec ->
            "December"
