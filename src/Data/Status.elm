module Data.Status exposing (Status(..))


type Status a b
    = Active a
    | Matured b
