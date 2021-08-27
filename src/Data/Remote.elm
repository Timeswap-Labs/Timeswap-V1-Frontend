module Data.Remote exposing (Remote(..), map, withDefault)


type Remote a
    = Loading
    | Failure
    | Success a


map : (a -> b) -> Remote a -> Remote b
map functor remote =
    case remote of
        Loading ->
            Loading

        Failure ->
            Failure

        Success success ->
            functor success
                |> Success


withDefault : a -> a -> Remote a -> a
withDefault default fail remote =
    case remote of
        Loading ->
            default

        Failure ->
            fail

        Success success ->
            success
