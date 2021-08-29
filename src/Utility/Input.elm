module Utility.Input exposing (isFloat, isInt)


isFloat : String -> Bool
isFloat string =
    String.all isDigitOrPoint string && hasMaxOnePoint string


isInt : String -> Bool
isInt string =
    String.all Char.isDigit string


isDigitOrPoint : Char -> Bool
isDigitOrPoint char =
    Char.isDigit char || isPoint char


isPoint : Char -> Bool
isPoint char =
    char == '.'


hasMaxOnePoint : String -> Bool
hasMaxOnePoint string =
    string
        |> String.filter isPoint
        |> String.length
        |> (>=) 1
