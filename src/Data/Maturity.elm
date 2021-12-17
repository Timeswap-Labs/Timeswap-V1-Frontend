module Data.Maturity exposing
    ( Maturity
    , Status(..)
    , decoder
    , dummy
    , dummy2
    , encode
    , fromFragment
    , isActive
    , sorter
    , toDuration
    , toQueryParameter
    , toString
    , toUnix
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Sort as Sort exposing (Sorter)
import Time exposing (Month(..), Posix, Zone)
import Url.Builder as Builder exposing (QueryParameter)


type Maturity
    = Maturity Posix


dummy : Maturity
dummy =
    Time.millisToPosix 1668664864000 |> Maturity


dummy2 : Maturity
dummy2 =
    Time.millisToPosix 1665654865000 |> Maturity


type Status matured active
    = Active active
    | Matured matured


decoder : Decoder Maturity
decoder =
    Decode.int
        |> Decode.map ((*) 1000)
        |> Decode.map Time.millisToPosix
        |> Decode.map Maturity


encode : Maturity -> Value
encode (Maturity posix) =
    posix
        |> Time.posixToMillis
        |> (\millis -> millis // 1000)
        |> Encode.int


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


toQueryParameter : Maturity -> QueryParameter
toQueryParameter maturity =
    maturity
        |> toUnix
        |> Builder.int "maturity"


toUnix : Maturity -> Int
toUnix (Maturity posix) =
    posix
        |> Time.posixToMillis
        |> (\millis -> millis // 1000)


sorter : Sorter Maturity
sorter =
    Sort.increasing
        |> Sort.by (\(Maturity posix) -> posix |> Time.posixToMillis)


isActive : Posix -> Maturity -> Bool
isActive now (Maturity posix) =
    (posix |> Time.posixToMillis) > (now |> Time.posixToMillis)


toString : Zone -> Maturity -> String
toString zone maturity =
    [ maturity |> toDateString zone
    , "|"
    , maturity |> toTimeString zone
    ]
        |> String.join " "


toDuration : Posix -> Maturity -> Status String String
toDuration now (Maturity maturity) =
    let
        nowInt : Int
        nowInt =
            now |> Time.posixToMillis

        maturityInt : Int
        maturityInt =
            maturity |> Time.posixToMillis

        total : Int
        total =
            if maturityInt > nowInt then
                (maturityInt - nowInt) // 1000

            else
                (nowInt - maturityInt) // 1000

        days : String
        days =
            total
                // 86400
                |> String.fromInt

        remainderDays : Int
        remainderDays =
            total |> modBy 86400

        hours : String
        hours =
            remainderDays
                // 3600
                |> String.fromInt
                |> String.padLeft 2 '0'

        remainderHours : Int
        remainderHours =
            remainderDays |> modBy 3600

        minutes : String
        minutes =
            remainderHours
                // 60
                |> String.fromInt
                |> String.padLeft 2 '0'

        seconds : String
        seconds =
            remainderHours
                |> modBy 60
                |> String.fromInt
                |> String.padLeft 2 '0'

        string : String
        string =
            days ++ "d : " ++ hours ++ "h : " ++ minutes ++ "m : " ++ seconds ++ "s"
    in
    if maturityInt > nowInt then
        Active string

    else
        Matured string


toDateString : Zone -> Maturity -> String
toDateString zone (Maturity posix) =
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


toTimeString : Zone -> Maturity -> String
toTimeString zone (Maturity posix) =
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
    in
    hour
        ++ ":"
        ++ minute
        ++ ":"
        ++ second


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
