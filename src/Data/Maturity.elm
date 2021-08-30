module Data.Maturity exposing
    ( Maturity
    , fromFragment
    , isActive
    , sorter
    , toFragment
    , toPosix
    , toString
    , unix1635364800
    , unix1650889815
    , unix962654400
    )

import Data.ZoneInfo exposing (ZoneInfo)
import Sort as Sort exposing (Sorter)
import Time exposing (Month(..), Posix)


type Maturity
    = Maturity Posix


fromFragment : String -> Maybe Maturity
fromFragment string =
    string
        |> String.split "="
        |> (\list ->
                case list of
                    "maturity" :: number :: _ ->
                        number
                            |> String.toInt
                            |> Maybe.map ((*) 1000)
                            |> Maybe.map Time.millisToPosix
                            |> Maybe.map Maturity

                    _ ->
                        Nothing
           )


toFragment : Maturity -> String
toFragment (Maturity posix) =
    [ "maturity"
    , posix
        |> Time.posixToMillis
        |> (\millis -> millis // 1000)
        |> String.fromInt
    ]
        |> String.join "="


toPosix : Maturity -> Posix
toPosix (Maturity posix) =
    posix


sorter : Sorter Maturity
sorter =
    Sort.increasing
        |> Sort.by (\(Maturity posix) -> posix |> Time.posixToMillis)


isActive : Posix -> Maturity -> Bool
isActive now (Maturity posix) =
    (posix |> Time.posixToMillis) > (now |> Time.posixToMillis)


toString : Maybe ZoneInfo -> Maturity -> String
toString maybeZoneInfo maturity =
    [ maturity |> toDateString maybeZoneInfo
    , maturity |> toTimeString maybeZoneInfo
    ]
        |> String.join " "


toDateString : Maybe ZoneInfo -> Maturity -> String
toDateString maybeZoneInfo (Maturity posix) =
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


toTimeString : Maybe ZoneInfo -> Maturity -> String
toTimeString maybeZoneInfo (Maturity posix) =
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


unix962654400 : Maturity
unix962654400 =
    Maturity (962654400000 |> Time.millisToPosix)


unix1635364800 : Maturity
unix1635364800 =
    Maturity (1635364800000 |> Time.millisToPosix)


unix1650889815 : Maturity
unix1650889815 =
    Maturity (1650889815000 |> Time.millisToPosix)
