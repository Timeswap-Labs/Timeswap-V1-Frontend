module Data.Remote exposing
    ( Remote(..)
    , andThen
    , loading
    , map
    , subscriptions
    , update
    , withDefault
    )

import Animator exposing (Animator, Timeline)
import Animator.Css
import Time exposing (Posix)


type Remote failure a
    = Loading (Timeline ())
    | Failure failure
    | Success a


loading : Remote failure a
loading =
    Animator.init ()
        |> Loading


map : (a -> b) -> Remote failure a -> Remote failure b
map functor remote =
    case remote of
        Loading timeline ->
            Loading timeline

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
        Loading timeline ->
            Loading timeline

        Failure error ->
            Failure error

        Success success ->
            monad success


update : Posix -> Remote failure a -> Remote failure a
update posix remote =
    case remote of
        Loading timeline ->
            Animator.update posix animator timeline
                |> Loading

        _ ->
            remote


animator : Animator (Timeline ())
animator =
    Animator.animator
        |> Animator.Css.watching
            identity
            always


subscriptions : (Posix -> msg) -> Remote failure a -> Sub msg
subscriptions tick remote =
    case remote of
        Loading timeline ->
            Animator.toSubscription tick timeline animator

        _ ->
            Sub.none
