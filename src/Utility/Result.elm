module Utility.Result exposing (apply)


apply : Result err a -> Result err (a -> b) -> Result err b
apply a functor =
    Result.map2 (<|) functor a
