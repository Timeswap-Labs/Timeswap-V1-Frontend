module Data.Remote exposing
    ( Remote(..)
    , andThen
    , map
    , withDefault
    )


type Remote failure a
    = Loading
    | Failure failure
    | Success a


map : (a -> b) -> Remote failure a -> Remote failure b
map functor remote =
    case remote of
        Loading ->
            Loading

        Failure failure ->
            Failure failure

        Success success ->
            functor success
                |> Success


withDefault : a -> Remote failure a -> a
withDefault default remote =
    case remote of
        Success success ->
            success

        _ ->
            default


andThen : (a -> Remote failure b) -> Remote failure a -> Remote failure b
andThen monad remote =
    case remote of
        Loading ->
            Loading

        Failure error ->
            Failure error

        Success success ->
            monad success
