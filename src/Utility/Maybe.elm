module Utility.Maybe exposing (apply, orElse)


apply : Maybe a -> Maybe (a -> b) -> Maybe b
apply a functor =
    Maybe.map2 (<|) functor a


orElse : Maybe a -> Maybe a -> Maybe a
orElse ma mb =
    case mb of
        Nothing ->
            ma

        Just _ ->
            mb
