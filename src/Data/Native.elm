module Data.Native exposing
    ( Flag
    , Native
    , decoder
    , encode
    , init
    , toDecimals
    , toName
    , toSymbol
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
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


decoder : Decoder Native
decoder =
    Decode.succeed
        (\name symbol decimals ->
            { name = name
            , symbol = symbol
            , decimals = decimals
            }
                |> Native
        )
        |> Pipeline.required "name" Decode.string
        |> Pipeline.required "symbol" Decode.string
        |> Pipeline.required "decimals" Decode.int


encode : Native -> Value
encode (Native { name, symbol, decimals }) =
    [ ( "name", name |> Encode.string )
    , ( "symbol", symbol |> Encode.string )
    , ( "decimals", decimals |> Encode.int )
    ]
        |> Encode.object


toName : Native -> String
toName (Native { name }) =
    name


toSymbol : Native -> String
toSymbol (Native { symbol }) =
    symbol


toDecimals : Native -> Int
toDecimals (Native { decimals }) =
    decimals
