module Data.Native exposing (Flag, Native, encode, init, toDecimals, toName, toSymbol)

import Json.Encode as Encode exposing (Value)


type Native
    = Native
        { name : String
        , symbol : String
        , decimals : Int
        }


type alias Flag =
    { name : String
    , symbol : String
    , decimals : Int
    }


init : Flag -> Native
init native =
    native |> Native


encode : Native -> Value
encode (Native { symbol }) =
    symbol |> Encode.string


toName : Native -> String
toName (Native { name }) =
    name


toSymbol : Native -> String
toSymbol (Native { symbol }) =
    symbol


toDecimals : Native -> Int
toDecimals (Native { decimals }) =
    decimals
