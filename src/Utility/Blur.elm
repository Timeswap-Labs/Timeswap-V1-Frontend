module Utility.Blur exposing (ten, tenHtml)

import Element
    exposing
        ( Attribute
        , htmlAttribute
        )
import Html
import Html.Attributes


ten : List (Attribute msg)
ten =
    tenHtml
        |> List.map htmlAttribute


tenHtml : List (Html.Attribute msg)
tenHtml =
    [ "blur(10px)"
        |> Html.Attributes.style "-webkit-backdrop-filter"
    , "blur(10px)"
        |> Html.Attributes.style "backdrop-filter"
    ]
