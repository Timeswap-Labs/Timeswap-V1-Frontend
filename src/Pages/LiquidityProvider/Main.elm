module Pages.LiquidityProvider.Main exposing (view)

import Element
    exposing
        ( Element
        , centerX
        , centerY
        , el
        , fill
        , height
        , text
        , width
        )
import Element.Font as Font
import Utility.Color as Color


view : Element msg
view =
    el
        [ width fill
        , height fill
        ]
        (el
            [ centerX
            , centerY
            , Font.size 24
            , Font.bold
            , Font.color Color.transparent500
            ]
            (text "Coming soon")
        )
