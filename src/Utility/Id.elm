module Utility.Id exposing (is)

import Element
    exposing
        ( Attribute
        , htmlAttribute
        )
import Html.Attributes


is : String -> Attribute msg
is string =
    Html.Attributes.id string
        |> htmlAttribute
