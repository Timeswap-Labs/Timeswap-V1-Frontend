module Data.Address exposing (Address, compare, daiMaticRinkeby, daiRinkeby, daiWethRinkeby, decoder, encode, fromString, maticRinkeby, sorter, toString, toStringShort, wethDaiRinkeby, wethRinkeby)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Sort exposing (Sorter)


type Address
    = Address String


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
