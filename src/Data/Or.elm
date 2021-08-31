module Data.Or exposing (Or(..))


type Or a b
    = Either a
    | Or b
