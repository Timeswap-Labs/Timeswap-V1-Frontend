module Utility.Animated exposing (modal)

import Animator exposing (Timeline)
import Animator.Css
import Data.Backdrop as Backdrop exposing (Backdrop)
import Element
    exposing
        ( Element
        , Option
        , focusStyle
        , html
        , layoutWith
        , noStaticStyleSheet
        )
import Html.Attributes


modal :
    { model | backdrop : Backdrop }
    -> Timeline state
    -> List (Animator.Css.Attribute state)
    -> List (Element.Attribute msg)
    -> Element msg
    -> Element msg
modal { backdrop } timeline animation attributes child =
    Animator.Css.div timeline
        animation
        ([ Html.Attributes.style "width" "100%"
         , Html.Attributes.style "height" "100%"
         ]
            ++ (case backdrop of
                    Backdrop.Supported ->
                        [ "blur(10px)"
                            |> Html.Attributes.style "-webkit-backdrop-filter"
                        , "blur(10px)"
                            |> Html.Attributes.style "backdrop-filter"
                        ]

                    Backdrop.NotSupported ->
                        []
               )
        )
        [ layoutWith { options = options } attributes child ]
        |> html


options : List Option
options =
    [ noStaticStyleSheet
    , { borderColor = Nothing
      , backgroundColor = Nothing
      , shadow = Nothing
      }
        |> focusStyle
    ]
