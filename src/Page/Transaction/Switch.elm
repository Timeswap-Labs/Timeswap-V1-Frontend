module Page.Transaction.Switch exposing (Mode(..), disabled, empty, view)

import Element
    exposing
        ( Element
        , centerX
        , centerY
        , el
        , fill
        , height
        , padding
        , px
        , row
        , spacing
        , text
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input exposing (OptionState)
import Utility.Color as Color


type Mode
    = Recommended
    | Pro


view :
    { onChange : Mode -> msg
    , mode : Mode
    }
    -> Element msg
view { onChange, mode } =
    Input.radioRow
        [ width fill
        , height <| px 36
        , padding 4
        , spacing 16
        , Background.color Color.primary100
        , Border.rounded 8
        ]
        { onChange = onChange
        , options =
            [ viewRecommended
                |> Input.optionWith Recommended
            , viewPro
                |> Input.optionWith Pro
            ]
        , selected = Just mode
        , label = Input.labelHidden "switch mode"
        }


disabled : Mode -> Element Never
disabled mode =
    row
        [ width fill
        , height <| px 36
        , padding 4
        , spacing 16
        , Background.color Color.primary100
        , Border.rounded 8
        ]
        [ disabledRecommended mode
        , disabledPro mode
        ]


empty : Element Never
empty =
    row
        [ width fill
        , height <| px 36
        , padding 4
        , spacing 16
        , Background.color Color.primary100
        , Border.rounded 8
        ]
        [ emptyRecommended
        , emptyPro
        ]


viewRecommended : OptionState -> Element msg
viewRecommended optionState =
    el
        ([ width fill
         , height fill
         ]
            ++ (case optionState of
                    Input.Selected ->
                        [ Background.color Color.primary500
                        , Border.rounded 4
                        ]

                    _ ->
                        []
               )
        )
        (el
            [ centerX
            , centerY
            , Font.color Color.light100
            , Font.size 14
            ]
            (text "Recommended")
        )


disabledRecommended : Mode -> Element Never
disabledRecommended mode =
    el
        ([ width fill
         , height fill
         ]
            ++ (case mode of
                    Recommended ->
                        [ Background.color Color.primary500
                        , Border.rounded 4
                        ]

                    Pro ->
                        []
               )
        )
        (el
            [ centerX
            , centerY
            , Font.color Color.light100
            , Font.size 14
            ]
            (text "Recommended")
        )


emptyRecommended : Element Never
emptyRecommended =
    el
        [ width fill
        , height fill
        , Background.color Color.primary500
        , Border.rounded 4
        ]
        (el
            [ centerX
            , centerY
            , Font.color Color.light100
            , Font.size 14
            ]
            (text "Recommended")
        )


viewPro : OptionState -> Element msg
viewPro optionState =
    el
        ([ width fill
         , height fill
         ]
            ++ (case optionState of
                    Input.Selected ->
                        [ Background.color Color.primary500
                        , Border.rounded 4
                        ]

                    _ ->
                        []
               )
        )
        (el
            [ centerX
            , centerY
            , Font.color Color.light100
            , Font.size 14
            ]
            (text "Pro")
        )


disabledPro : Mode -> Element Never
disabledPro mode =
    el
        ([ width fill
         , height fill
         ]
            ++ (case mode of
                    Recommended ->
                        []

                    Pro ->
                        [ Background.color Color.primary500
                        , Border.rounded 4
                        ]
               )
        )
        (el
            [ centerX
            , centerY
            , Font.color Color.light100
            , Font.size 14
            ]
            (text "Pro")
        )


emptyPro : Element Never
emptyPro =
    el
        [ width fill
        , height fill
        ]
        (el
            [ centerX
            , centerY
            , Font.color Color.light100
            , Font.size 14
            ]
            (text "Pro")
        )
