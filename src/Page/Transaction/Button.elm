module Page.Transaction.Button exposing (disabled, error, view)

import Element
    exposing
        ( Element
        , centerX
        , centerY
        , el
        , height
        , px
        , text
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import Utility.Color as Color


view : msg -> String -> Element msg
view msg string =
    Input.button
        [ width <| px 335
        , height <| px 44
        , Background.color Color.primary500
        , Border.rounded 8
        ]
        { onPress = Just msg
        , label =
            el
                [ centerX
                , centerY
                ]
                (text string)
        }


disabled : String -> Element msg
disabled string =
    el
        [ width <| px 335
        , height <| px 44
        , Background.color Color.light500
        , Border.rounded 8
        ]
        (el
            [ centerX
            , centerY
            ]
            (text string)
        )


error : String -> Element msg
error string =
    el
        [ width <| px 335
        , height <| px 44
        , Background.color Color.light500
        , Border.rounded 8
        ]
        (el
            [ centerX
            , centerY
            ]
            (text string)
        )
