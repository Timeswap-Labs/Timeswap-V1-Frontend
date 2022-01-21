module Utility.IconButton exposing (back, exit)

import Data.Images exposing (Images)
import Data.Theme as Theme exposing (Theme)
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


back : { model | images : Images, theme : Theme } -> msg -> Element msg
back { images, theme } msg =
    Input.button
        [ width shrink
        , height shrink
        , alignLeft
        , centerY
        ]
        { onPress = Just msg
        , label =
            images
                |> (case theme of
                        Theme.Dark ->
                            Image.arrowLeft

                        Theme.Light ->
                            Image.arrowLeftDark
                   )
                    [ width <| px 16
                    , height <| px 15
                    ]
        }


exit : { model | images : Images, theme : Theme } -> msg -> Element msg
exit { images, theme } msg =
    Input.button
        [ width shrink
        , height shrink
        , alignRight
        , centerY
        ]
        { onPress = Just msg
        , label =
            images
                |> (case theme of
                        Theme.Dark ->
                            Image.close

                        Theme.Light ->
                            Image.closeDark
                   )
                    [ width <| px 24
                    , height <| px 24
                    ]
        }
