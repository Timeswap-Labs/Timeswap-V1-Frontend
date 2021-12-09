module Data.Web exposing (Web, fromHttpRemote)

import Data.Remote exposing (Remote(..))
import Data.WebError as WebError exposing (WebError)
import Http


type alias Web a =
    Remote WebError a


fromHttpRemote : Remote Http.Error a -> Maybe (Web a)
fromHttpRemote remote =
    case remote of
        Loading ->
            Loading
                |> Just

        Failure httpError ->
            httpError
                |> WebError.fromHttpError
                |> Maybe.map Failure

        Success success ->
            Success success
                |> Just
