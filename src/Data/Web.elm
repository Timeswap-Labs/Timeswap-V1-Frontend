module Data.Web exposing (Web)

import Data.Remote exposing (Remote)
import Http


type alias Web a =
    Remote Http.Error a
