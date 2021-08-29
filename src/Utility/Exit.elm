module Utility.Exit exposing (button, buttonWithMsg, toUrl)

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
import Element.Input as Input
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


buttonWithMsg : msg -> Element msg
buttonWithMsg msg =
    el
        [ width shrink
        , height shrink
        , padding 20
        , alignRight
        , alignTop
        ]
        (Input.button
            [ width shrink
            , height shrink
            ]
            { onPress = Just msg
            , label = Image.close [ width <| px 24 ]
            }
        )
