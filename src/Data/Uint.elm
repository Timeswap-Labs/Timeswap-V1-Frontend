module Data.Uint exposing
    ( Uint
    , add
    , decoder
    , encode
    , fromAmount
    , fromString
    , hasEnough
    , isAmount
    , isZero
    , proportion
    , sorter
    , toAmount
    , toAmountTruncated
    , toLP
    , toString
    , zero
    )

import BigInt
import Data.Token as Token exposing (Token)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Sort exposing (Sorter)
import Utility.Input as Input
import Utility.Maybe as Maybe


type Uint
    = Uint String


decoder : Decoder Uint
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                string
                    |> fromString
                    |> Maybe.map Decode.succeed
                    |> Maybe.withDefault (Decode.fail "not a uint")
            )


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


encode : Uint -> Value
encode (Uint string) =
    Encode.string string


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


toAmount : Token -> Uint -> String
toAmount token (Uint string) =
    token
        |> Token.toDecimals
        |> (\decimals ->
                string
                    |> String.padLeft (decimals + 1) '0'
                    |> (\paddedString ->
                            [ paddedString |> String.dropRight decimals
                            , paddedString |> String.right decimals
                            ]
                                |> String.join "."
                       )
                    |> String.foldr
                        (\char accumulator ->
                            if char == '0' && accumulator == "" then
                                accumulator

                            else
                                accumulator
                                    |> String.cons char
                        )
                        ""
                    |> (\formattedString ->
                            if formattedString |> String.endsWith "." then
                                formattedString
                                    |> String.dropRight 1

                            else
                                formattedString
                       )
           )


toAmountTruncated : Token -> Uint -> String
toAmountTruncated token (Uint string) =
    token
        |> Token.toDecimals
        |> (\decimals ->
                string
                    |> String.padLeft (decimals + 1) '0'
                    |> (\paddedString ->
                            [ paddedString |> String.dropRight decimals
                            , paddedString |> String.right decimals |> String.left 2
                            ]
                                |> String.join "."
                       )
                    |> String.foldr
                        (\char accumulator ->
                            if char == '0' && accumulator == "" then
                                accumulator

                            else
                                accumulator
                                    |> String.cons char
                        )
                        ""
                    |> (\formattedString ->
                            if formattedString |> String.endsWith "." then
                                formattedString
                                    |> String.dropRight 1

                            else
                                formattedString
                       )
           )


toLP : Uint -> String
toLP (Uint string) =
    string
        |> String.padLeft 19 '0'
        |> (\paddedString ->
                [ paddedString |> String.dropRight 18
                , paddedString |> String.right 18
                ]
                    |> String.join "."
           )
        |> String.foldr
            (\char accumulator ->
                if char == '0' && accumulator == "" then
                    accumulator

                else
                    accumulator
                        |> String.cons char
            )
            ""
        |> (\formattedString ->
                if formattedString |> String.endsWith "." then
                    formattedString
                        |> String.dropRight 1

                else
                    formattedString
           )


isAmount : Token -> String -> Bool
isAmount token string =
    fromAmount token string
        |> Maybe.map (\_ -> True)
        |> Maybe.withDefault False


{-| if uint2 is greater than uint1, return true.
-}
hasEnough : Uint -> Uint -> Bool
hasEnough uint1 uint2 =
    case compare uint1 uint2 of
        GT ->
            False

        _ ->
            True


toString : Uint -> String
toString (Uint string) =
    string


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
                    newString |> String.reverse
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


zero : Uint
zero =
    Uint "0"


isZero : Uint -> Bool
isZero (Uint string) =
    string == "0"


add : Uint -> Uint -> Maybe Uint
add (Uint uint1) (Uint uint2) =
    Just BigInt.add
        |> Maybe.apply (uint1 |> BigInt.fromIntString)
        |> Maybe.apply (uint2 |> BigInt.fromIntString)
        |> Maybe.map BigInt.toString
        |> Maybe.andThen fromString


proportion : Uint -> Uint -> Uint -> Maybe Uint
proportion (Uint uint1) (Uint uint2) (Uint uint3) =
    Just
        (\num1 num2 den ->
            if BigInt.lte num1 den then
                BigInt.div (BigInt.mul num1 num2) den
                    |> Just

            else
                Nothing
        )
        |> Maybe.apply (uint1 |> BigInt.fromIntString)
        |> Maybe.apply (uint2 |> BigInt.fromIntString)
        |> Maybe.apply (uint3 |> BigInt.fromIntString)
        |> Maybe.andThen identity
        |> Maybe.map BigInt.toString
        |> Maybe.andThen fromString
