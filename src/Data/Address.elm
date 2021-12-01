module Data.Address exposing (Address, axie, compare, daiMaticRinkeby, daiRinkeby, daiWethRinkeby, decoder, doge, encode, fromString, matic, maticRinkeby, participantAddresses, shiba, sorter, toString, toStringShort, usdc, wethDaiRinkeby, wethRinkeby)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Sort exposing (Sorter)


type Address
    = Address String


shiba : Address
shiba =
    Address "0xCb0DfB7f2375BDBAA2E02A39BD55fda918b53c54"


doge : Address
doge =
    Address "0x1A416eE7Cb46DF23c8e2beD192b491c37dDE9Da7"


usdc : Address
usdc =
    Address "0x4d86d74cfcdd70897a6d0bf10698aefe7df9d49e"


axie : Address
axie =
    Address "0x4bABb4A5e0324b75E5c9401906E16434a7B59d89"


matic : Address
matic =
    Address "0x2586D0c90b4D432033Cc5fB370c54d4B3A93b857"


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

        length : Int
        length =
            string
                |> String.length
    in
    if (initial == "0x" || initial == "0X") && length == 42 then
        string
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


toStringShort : Address -> String
toStringShort address =
    let
        string : String
        string =
            address
                |> toString
    in
    String.left 5 string ++ "..." ++ String.right 3 string


compare : Address -> Address -> Order
compare (Address address1) (Address address2) =
    Basics.compare address1 address2


sorter : Sorter Address
sorter =
    Sort.custom compare



-- Change the address before testnet


daiRinkeby : Address
daiRinkeby =
    Address "0xf18f57a842398Aba5420A0405f4D1cf3De8D99Ba"


maticRinkeby : Address
maticRinkeby =
    Address "0x98ea5A9f9621F160EC378e1F1b1Be78A4809eF32"


wethRinkeby : Address
wethRinkeby =
    Address "0xa1fcEeFd0bA04519729815Fc0512E47869C8818e"


daiWethRinkeby : Address
daiWethRinkeby =
    Address "0xf18f57a842398Aba5420A0405f4D1cf3De8D29Ba"


daiMaticRinkeby : Address
daiMaticRinkeby =
    Address "0xf18f57a8423985ba5420A0405f4D1cf3De8D99Ba"


wethDaiRinkeby : Address
wethDaiRinkeby =
    Address "0xa1fcEeFd0bA04519729814Fc0512E47869C8818e"
