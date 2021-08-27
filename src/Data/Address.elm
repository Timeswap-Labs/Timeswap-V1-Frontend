module Data.Address exposing (Address, compare, fromString, sorter, toString, toStringShort)

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
            |> String.toLower
            |> Address
            |> Just

    else
        Nothing


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
