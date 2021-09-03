module Data.Percent exposing (Percent, fromFloat, init, toFloat)


type Percent
    = Percent Int


init : Percent
init =
    Percent 64


toFloat : Percent -> Float
toFloat (Percent int) =
    int |> Basics.toFloat


fromFloat : Float -> Percent
fromFloat float =
    float
        |> truncate
        |> clamp 0 128
        |> Percent
