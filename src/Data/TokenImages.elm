module Data.TokenImages exposing (TokenImages, init)

import Sort
import Sort.Dict as Dict exposing (Dict)


type alias TokenImages =
    Dict String String


init : List ( String, String ) -> TokenImages
init list =
    Dict.fromList Sort.alphabetical list
