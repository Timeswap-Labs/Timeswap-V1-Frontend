module Data.Uint exposing
    ( Uint
    , compare
    , example
    , fromAmount
    , fromString
    , isAmount
    , sorter
    , toAmount
    , toString
    )

import Data.Token as Token exposing (Token)
import Sort exposing (Sorter)
import Utility.Input as Input


type Uint
    = Uint String


fromString : String -> Maybe Uint
fromString string =
    string
        |> trimZero
        |> (\formattedString ->
                if formattedString |> String.all Char.isDigit then
                    case compare (Uint formattedString) uint256 of
                        LT ->
                            Uint string |> Just

                        _ ->
                            Nothing

                else
                    Nothing
           )


fromAmount : Token -> String -> Maybe Uint
fromAmount token string =
    (if string |> Input.isFloat then
        (\list decimals ->
            case list of
                whole :: fraction :: [] ->
                    if (fraction |> String.length) <= decimals then
                        [ whole |> trimZero
                        , fraction |> String.padRight decimals '0'
                        ]
                            |> String.concat
                            |> trimZero
                            |> Uint
                            |> Just

                    else
                        Nothing

                whole :: [] ->
                    [ whole |> trimZero
                    , "0" |> String.repeat decimals
                    ]
                        |> String.concat
                        |> trimZero
                        |> Uint
                        |> Just

                [] ->
                    Uint "0" |> Just

                _ ->
                    Nothing
        )
            (string |> String.split ".")
            (token |> Token.toDecimals)

     else
        Nothing
    )
        |> Maybe.andThen
            (\uint ->
                case compare uint uint256 of
                    LT ->
                        Just uint

                    _ ->
                        Nothing
            )


trimZero : String -> String
trimZero string =
    string
        |> String.foldl
            (\char accumulator ->
                if accumulator == "" && char == '0' then
                    ""

                else
                    String.cons char accumulator
            )
            ""
        |> (\newString ->
                if newString == "" then
                    "0"

                else
                    newString
           )


isAmount : Token -> String -> Bool
isAmount token string =
    fromAmount token string
        |> Maybe.map (\_ -> True)
        |> Maybe.withDefault False


toString : Uint -> String
toString (Uint string) =
    string


toAmount : Int -> Uint -> String
toAmount decimals (Uint string) =
    string
        |> String.padLeft (decimals + 1) '0'
        |> (\paddedString ->
                [ paddedString |> String.dropRight decimals
                , paddedString |> String.right decimals
                ]
                    |> String.join "."
           )


compare : Uint -> Uint -> Order
compare (Uint string1) (Uint string2) =
    Basics.compare (string1 |> String.length) (string2 |> String.length)
        |> (\order ->
                case order of
                    EQ ->
                        List.map2
                            (\char1 char2 ->
                                ( char1 |> String.fromChar, char2 |> String.fromChar )
                            )
                            (string1 |> String.toList)
                            (string2 |> String.toList)
                            |> List.foldl
                                (\( char1, char2 ) accumulator ->
                                    case accumulator of
                                        EQ ->
                                            Maybe.map2 Basics.compare
                                                (char1 |> String.toInt)
                                                (char2 |> String.toInt)
                                                |> Maybe.withDefault EQ

                                        _ ->
                                            accumulator
                                )
                                EQ

                    _ ->
                        order
           )


sorter : Sorter Uint
sorter =
    Sort.custom compare


uint256 : Uint
uint256 =
    Uint "115792089237316195423570985008687907853269984665640564039457584007913129639936"


example : List Uint
example =
    [ Uint "0"
    , Uint "1"
    , Uint "2"
    , Uint "3"
    ]
