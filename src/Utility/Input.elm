module Utility.Input exposing (isFloat, isInt, isZero)


isFloat : String -> Bool
isFloat string =
    String.all isDigitOrPoint string && hasMaxOnePoint string


isInt : String -> Bool
isInt string =
    String.all Char.isDigit string


isZero : String -> Bool
isZero string =
    String.all isZeroOrPoint string && hasMaxOnePoint string


isZeroOrPoint : Char -> Bool
isZeroOrPoint char =
    char == '0' || isPoint char


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
