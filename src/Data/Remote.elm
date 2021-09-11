module Data.Remote exposing (Remote(..), map)


type Remote error a
    = Loading
    | Failure error
    | Success a


map : (a -> b) -> Remote error a -> Remote error b
map functor remote =
    case remote of
        Loading ->
            Loading

        Failure error ->
            Failure error

        Success success ->
            functor success
                |> Success
