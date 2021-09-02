module Utility.Exit exposing (button, buttonWithMsg)

import Data.Images exposing (Images)
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


button : Images -> Element msg
button images =
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
            , label = Image.close images [ width <| px 24 ]
            }
        )


buttonWithMsg : Images -> msg -> Element msg
buttonWithMsg images msg =
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
            , label = Image.close images [ width <| px 24 ]
            }
        )
