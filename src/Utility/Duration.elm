module Utility.Duration exposing (toString)

import Time exposing (Posix)


toString : Posix -> Posix -> String
toString now maturity =
    let
        total : Int
        total =
            (maturity |> Time.posixToMillis)
                - (now |> Time.posixToMillis)
                // 1000

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

        remainderHours : Int
        remainderHours =
            remainderDays |> modBy 3600

        minutes : String
        minutes =
            remainderHours
                // 60
                |> String.fromInt

        seconds : String
        seconds =
            remainderHours
                |> modBy 60
                |> String.fromInt
    in
    days ++ "d : " ++ hours ++ "h : " ++ minutes ++ "m : " ++ seconds ++ "s"
