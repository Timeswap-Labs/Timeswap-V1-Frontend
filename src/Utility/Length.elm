module Utility.Length exposing (heightFill, widthFill)

import Html
import Html.Attributes


widthFill : Html.Attribute msg
widthFill =
    Html.Attributes.style "width" "100%"


heightFill : Html.Attribute msg
heightFill =
    Html.Attributes.style "height" "100%"
