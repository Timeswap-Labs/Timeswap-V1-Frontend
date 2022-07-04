module Utility.Class exposing (is)

import Element
    exposing
        ( Attribute
        , htmlAttribute
        )
import Html.Attributes


is : String -> Attribute msg
is string =
    Html.Attributes.class string
        |> htmlAttribute
