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
import Element.Region as Region
import Utility.Color as Color


view :
    { msg : msg
    , label : String
    , description : String
    }
    -> Element msg
view { msg, label, description } =
    Input.button
        [ Region.description description
        , width <| px 335
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
                (text label)
        }


disabled : String -> Element Never
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


error : String -> Element Never
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
