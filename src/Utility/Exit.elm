module Utility.Exit exposing (button, buttonWithMsg)

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
import Utility.Router as Router


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
            { url = Router.exit
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
