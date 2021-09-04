module Data.Percent exposing (Percent, decoder, encode, fromFloat, init, toFloat)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type Percent
    = Percent Int


init : Percent
init =
    Percent 64


decoder : Decoder Percent
decoder =
    Decode.int
        |> Decode.map (\int -> int // 33554432)
        |> Decode.map Percent


encode : Percent -> Value
encode (Percent int) =
    Encode.int (int * 33554432)


toFloat : Percent -> Float
toFloat (Percent int) =
    int |> Basics.toFloat


fromFloat : Float -> Percent
fromFloat float =
    float
        |> truncate
        |> clamp 0 128
        |> Percent
