module Modal.Connect.Terms exposing (..)

import Element
    exposing
        ( Element
        , centerX
        , el
        , height
        , newTabLink
        , paddingXY
        , paragraph
        , shrink
        , text
        , width
        )
import Element.Font as Font
import Url.Builder as Builder
import Utility.Color as Color


view : Element msg
view =
    paragraph
        [ width shrink
        , height shrink
        , paddingXY 0 3
        , centerX
        , Font.regular
        , Font.size 14
        ]
        [ el
            [ Font.color Color.transparent300 ]
            (text "By connecting, I accept Timeswap's ")
        , newTabLink
            [ Font.color Color.primary300 ]
            { url =
                Builder.crossOrigin "https://timeswap.io"
                    [ "terms" ]
                    []
            , label = text "terms of service"
            }
        ]
