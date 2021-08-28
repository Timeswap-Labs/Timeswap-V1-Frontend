module Utility.Exit exposing (button, toUrl)

import Element
    exposing
        ( Element
        , alignRight
        , alignTop
        , el
        , height
        , link
        , padding
        , px
        , shrink
        , width
        )
import Utility.Image as Image


toUrl : String
toUrl =
    "#"


button : Element msg
button =
    el
        [ width shrink
        , height shrink
        , padding 20
        , alignRight
        , alignTop
        ]
        (link
            [ width shrink
            , height shrink
            ]
            { url = toUrl
            , label = Image.close [ width <| px 24 ]
            }
        )
