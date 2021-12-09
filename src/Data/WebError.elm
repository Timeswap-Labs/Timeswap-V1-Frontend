module Data.WebError exposing (WebError(..), fromHttpError, toResult)

import Http


type WebError
    = Timeout
    | NetworkError
    | BadStatus Int


fromHttpError : Http.Error -> Maybe WebError
fromHttpError error =
    case error of
        Http.Timeout ->
            Just Timeout

        Http.NetworkError ->
            Just NetworkError

        Http.BadStatus code ->
            BadStatus code
                |> Just

        _ ->
            Nothing


toResult : Result Http.Error a -> Maybe (Result WebError a)
toResult result =
    case result of
        Ok a ->
            Ok a
                |> Just

        Err error ->
            error
                |> fromHttpError
                |> Maybe.map Err
