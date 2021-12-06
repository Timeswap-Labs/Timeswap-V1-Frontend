module Utility.Maybe exposing (apply)


apply : Maybe a -> Maybe (a -> b) -> Maybe b
apply a functor =
    Maybe.map2 (<|) functor a
