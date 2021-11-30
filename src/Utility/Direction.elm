module Utility.Direction exposing (Direction(..), toAlign, toMove)

import Element
    exposing
        ( Attribute
        , alignLeft
        , alignRight
        , moveLeft
        , moveRight
        )


type Direction a
    = Left a
    | Right a


toMove : Direction Int -> Attribute msg
toMove direction =
    case direction of
        Left int ->
            moveLeft (int |> toFloat)

        Right int ->
            moveRight (int |> toFloat)


toAlign : Direction () -> Attribute msg
toAlign direction =
    case direction of
        Left () ->
            alignLeft

        Right () ->
            alignRight
