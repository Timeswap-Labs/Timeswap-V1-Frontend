module Utility.FontStyle exposing (FontStyle(..), toAttribute)

import Element exposing (Attribute)
import Element.Font as Font


type FontStyle
    = Regular
    | Bold


toAttribute : FontStyle -> Attribute msg
toAttribute fontStyle =
    case fontStyle of
        Regular ->
            Font.regular

        Bold ->
            Font.bold
