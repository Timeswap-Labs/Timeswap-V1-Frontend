module Utility.Pointer exposing (off, offHtml, on, onHtml)

import Element
    exposing
        ( Attribute
        , htmlAttribute
        )
import Html
import Html.Attributes


on : Attribute msg
on =
    onHtml
        |> htmlAttribute


off : Attribute msg
off =
    offHtml
        |> htmlAttribute


onHtml : Html.Attribute msg
onHtml =
    Html.Attributes.style "pointer-events" "auto"


offHtml : Html.Attribute msg
offHtml =
    Html.Attributes.style "pointer-events" "none"
