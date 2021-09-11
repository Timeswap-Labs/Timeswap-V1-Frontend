module Data.Timeout exposing
    ( Timeout
    , countdown
    , start
    )


type Timeout
    = Timeout Int


start : Int -> Maybe Timeout
start int =
    if int <= 0 then
        Nothing

    else
        Timeout int |> Just


countdown : Timeout -> Maybe Timeout
countdown (Timeout int) =
    if int <= 1 then
        Nothing

    else
        Timeout (int - 1) |> Just
