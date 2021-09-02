module Data.Percent exposing (Percent, toFloat)


type Percent
    = Percent Int


toFloat : Percent -> Float
toFloat (Percent int) =
    int |> Basics.toFloat


fromFloat : Float -> Percent
fromFloat float =
    float
        |> truncate
        |> clamp 0 128
        |> Percent
