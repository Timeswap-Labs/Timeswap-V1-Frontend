module Utility.IconButton exposing (back, exit)

import Data.Images exposing (Images)
import Element
    exposing
        ( Element
        , alignLeft
        , alignRight
        , centerY
        , height
        , px
        , rotate
        , shrink
        , width
        )
import Element.Input as Input
import Utility.Image as Image


back : { model | images : Images } -> msg -> Element msg
back { images } msg =
    Input.button
        [ width shrink
        , height shrink
        , alignLeft
        , centerY
        ]
        { onPress = Just msg
        , label =
            images
                |> Image.arrowDown
                    [ width <| px 18
                    , height <| px 18
                    , rotate (pi / 2)
                    ]
        }


exit : { model | images : Images } -> msg -> Element msg
exit { images } msg =
    Input.button
        [ width shrink
        , height shrink
        , alignRight
        , centerY
        ]
        { onPress = Just msg
        , label =
            images
                |> Image.close
                    [ width <| px 24
                    , height <| px 24
                    ]
        }
