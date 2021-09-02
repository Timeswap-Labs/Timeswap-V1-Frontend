module Data.Images exposing (Images, init)

import Sort
import Sort.Dict as Dict exposing (Dict)


type alias Images =
    Dict String String


init : List ( String, String ) -> Images
init list =
    Dict.fromList Sort.alphabetical list
