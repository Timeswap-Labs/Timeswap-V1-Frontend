module Data.Images exposing (Flags, Images, init)

import Sort
import Sort.Dict as Dict exposing (Dict)


type alias Images =
    { images : Dict String String
    , tokens : Dict String String
    , chains : Dict String String
    }


type alias Flags =
    List ( String, String )


init : Flags -> Flags -> Flags -> Images
init images tokens chains =
    { images = images |> Dict.fromList Sort.alphabetical
    , tokens = tokens |> Dict.fromList Sort.alphabetical
    , chains = chains |> Dict.fromList Sort.alphabetical
    }
