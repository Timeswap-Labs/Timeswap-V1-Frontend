module Data.Web exposing (Web, fromResult)

import Data.Remote exposing (Remote(..))
import Http


type alias Web a =
    Remote Http.Error a


fromResult : Result Http.Error a -> Web a
fromResult result =
    case result of
        Ok a ->
            Success a

        Err error ->
            Failure error
