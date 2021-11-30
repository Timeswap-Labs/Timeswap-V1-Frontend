module Data.Address exposing
    ( Address
    , compare
    , decoder
    , encode
    , fromString
    , sorter
    , toQueryParameter
    , toString
    , toStringShort
    )

import Hex
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Keccak.Int as Keccak
import Sort exposing (Sorter)
import Url.Builder as Builder exposing (QueryParameter)


type Address
    = Address String


fromString : String -> Maybe Address
fromString string =
    let
        initial : String
        initial =
            string
                |> String.left 2

        remaining : String
        remaining =
            string
                |> String.dropLeft 2

        length : Int
        length =
            string
                |> String.length
    in
    if
        (initial == "0x" || initial == "0X")
            && (remaining |> String.all Char.isHexDigit)
            && (length == 42)
    then
        string
            |> String.toLower
            |> Address
            |> Just

    else
        Nothing


decoder : Decoder Address
decoder =
    Decode.string
        |> Decode.map fromString
        |> Decode.andThen
            (\address ->
                address
                    |> Maybe.map Decode.succeed
                    |> Maybe.withDefault (Decode.fail "Incorrect address")
            )


encode : Address -> Value
encode (Address string) =
    Encode.string string


toString : Address -> String
toString (Address string) =
    string
        |> checksumHelper
        |> (\( addrChars, hashInts ) ->
                List.map2 compareCharToHash addrChars hashInts
                    |> String.fromList
                    |> (++) "0x"
           )


toStringShort : Address -> String
toStringShort address =
    let
        string : String
        string =
            address
                |> toString
    in
    String.left 5 string ++ "..." ++ String.right 3 string


toQueryParameter : Address -> QueryParameter
toQueryParameter address =
    address
        |> toString
        |> Builder.string "address"


compare : Address -> Address -> Order
compare (Address address1) (Address address2) =
    Basics.compare address1 address2


sorter : Sorter Address
sorter =
    Sort.custom compare


checksumHelper : String -> ( List Char, List Int )
checksumHelper address =
    let
        addressChars =
            address
                |> String.dropLeft 2
                |> String.toList
    in
    addressChars
        |> List.map (Char.toLower >> Char.toCode)
        |> Keccak.ethereum_keccak_256
        |> List.take 20
        |> List.map (Hex.toString >> toByteLength)
        |> String.join ""
        |> String.split ""
        |> List.map Hex.fromString
        |> List.foldr (Result.map2 (::)) (Ok [])
        |> Result.withDefault []
        |> (\b -> ( addressChars, b ))


compareCharToHash : Char -> Int -> Char
compareCharToHash char hashInt =
    if hashInt >= 8 then
        char |> Char.toUpper

    else
        char


toByteLength : String -> String
toByteLength string =
    if (String.length string |> modBy 2) == 1 then
        string |> String.append "0"

    else
        string
