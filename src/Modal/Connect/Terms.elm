module Modal.Connect.Terms exposing (..)

import Data.Theme exposing (Theme)
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
import Utility.ThemeColor as ThemeColor


view : Theme -> Element msg
view theme =
    paragraph
        [ width shrink
        , height shrink
        , paddingXY 0 3
        , centerX
        , Font.regular
        , Font.size 14
        ]
        [ el
            [ theme |> ThemeColor.textLight |> Font.color ]
            (text "By connecting, I accept Timeswap's ")
        , newTabLink
            [ theme |> ThemeColor.btnPressBG |> Font.color ]
            { url =
                Builder.crossOrigin "https://timeswap.io"
                    [ "terms" ]
                    []
            , label = text " terms of service"
            }
        ]
