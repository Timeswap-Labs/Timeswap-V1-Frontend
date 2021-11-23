module Utility.Millis exposing (add)

import Time exposing (Posix)


add : Int -> Posix -> Posix
add num posix =
    (posix |> Time.posixToMillis) + num |> Time.millisToPosix
