module Data.Address exposing
    ( Address
    , compare
    , decoder
    , encode
    , fromString
    , participantAddresses
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


participantAddresses : List Address
participantAddresses =
    [ "0xD5451D0C1ec4cAEF1197c9544Ac1C6782F588DFC"
    , "0x0717C21AC72F2B7C64F196c13ee504c726dDb8fD"
    , "0xb932c0d5c87B5F8354CC8898Ba74E5F28510C10E"
    , "0x4830212faCF11d2E22Cdd46aF6b9992b226B5A67"
    , "0x94c820722D1A9F2Ce3aa640a272a63cE2e032aCE"
    , "0xd1B856ee12Bd00922cae8DD86ab068f8c0F95224"
    , "0xc840D0A9bb73e1C76915c013804B7b6Cb67462ec"
    , "0xa1d1a9E75Ba80725890eed232869A8321E84Bb32"
    , "0xf402B174B1D6C1DCCc0725ACa2D0F1f6Bb4823F0"
    , "0x99b56b239EbA2Ff5249E0f88C5aD7A29611BDd92"
    , "0x641B87A1a09f6261e8c85179afFe14135ECd4aB0"
    , "0xb33ea596194B7d0cA77263246B5E9AF4a619853C"
    , "0x2f70859627952d4DC8237d13c15bd1620f507BAF"
    , "0x781938b7Ce5474097849EeCDF6c803E8C353191C"
    , "0xe91A790292870eF71F70Bf8A98A01e11DeeF3a74"
    , "0x630999EEe371D5C96526344EA1e513F31afB113c"

    -- Community
    ]
        |> List.map String.toLower
        |> List.map Address


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
