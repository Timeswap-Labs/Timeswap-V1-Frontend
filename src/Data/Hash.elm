module Data.Hash exposing
    ( Hash
    , decoder
    , encode
    , fromString
    , sorter
    , toString
    , toUrlString
    )

import Data.Chain as Chain exposing (Chain)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Sort exposing (Sorter)
import Url.Builder as Builder


type Hash
    = Hash String


fromString : String -> Maybe Hash
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
            && (length == 66)
    then
        string
            |> String.toLower
            |> Hash
            |> Just

    else
        Nothing


toString : Hash -> String
toString (Hash string) =
    string


toUrlString : Chain -> Hash -> String
toUrlString chain hash =
    Builder.crossOrigin (chain |> Chain.toBlockExplorerUrl)
        [ "tx", hash |> toString ]
        []


decoder : Decoder Hash
decoder =
    Decode.string
        |> Decode.map fromString
        |> Decode.andThen
            (\hash ->
                hash
                    |> Maybe.map Decode.succeed
                    |> Maybe.withDefault (Decode.fail "Incorrect hash")
            )


encode : Hash -> Value
encode (Hash string) =
    Encode.string string


sorter : Sorter Hash
sorter =
    Sort.alphabetical
        |> Sort.by (\(Hash string) -> string)
