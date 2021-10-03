module Utility.Loading exposing (view, viewSmall)

import Element
    exposing
        ( Element
        , centerX
        , centerY
        , el
        , height
        , none
        , px
        , width
        )
import Html.Attributes


view : Element msg
view =
    el
        [ width <| px 10
        , height <| px 10
        , centerX
        , centerY
        , Element.htmlAttribute <| Html.Attributes.class "dot-pulse"
        ]
        none


viewSmall : Element msg
viewSmall =
    el
        [ width <| px 10
        , height <| px 10
        , centerX
        , centerY
        , Element.htmlAttribute <| Html.Attributes.class "dot-pulse"
        ]
        none
