module Page.Transaction.Switch exposing (disabled, empty, view)

import Data.Mode as Mode exposing (Mode)
import Data.Theme exposing (Theme)
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
import Element.Input as Input
import Utility.ThemeColor as Color


view :
    { onChange : Mode -> msg
    , mode : Mode
    , theme : Theme
    }
    -> Element msg
view param =
    row
        [ width fill
        , height <| px 44
        , padding 4
        , spacing 16
        , param.theme |> Color.primary100 |> Background.color
        , Border.rounded 8
        ]
        [ viewRecommended param
        , viewAdvanced param
        ]


disabled : { mode : Mode, theme : Theme } -> Element Never
disabled ({ mode, theme } as model) =
    row
        [ width fill
        , height <| px 44
        , padding 4
        , spacing 16
        , theme |> Color.primary100 |> Background.color
        , Border.rounded 8
        ]
        [ disabledRecommended model
        , disabledAdvanced model
        ]


empty : Theme -> Element Never
empty theme =
    row
        [ width fill
        , height <| px 44
        , padding 4
        , spacing 16
        , theme |> Color.primary100 |> Background.color
        , Border.rounded 8
        ]
        [ emptyRecommended theme
        , emptyAdvanced theme
        ]


viewRecommended :
    { onChange : Mode -> msg
    , mode : Mode
    , theme : Theme
    }
    -> Element msg
viewRecommended { onChange, mode, theme } =
    Input.button
        ([ width fill
         , height fill
         ]
            ++ (case mode of
                    Mode.Recommended ->
                        [ theme |> Color.primary500 |> Background.color
                        , Border.rounded 4
                        ]

                    _ ->
                        []
               )
        )
        { onPress =
            case mode of
                Mode.Recommended ->
                    Nothing

                Mode.Advanced ->
                    Just (onChange Mode.Recommended)
        , label =
            el
                [ centerX
                , centerY
                , theme |> Color.light100 |> Font.color
                , Font.size 14
                ]
                (text "Recommended")
        }


disabledRecommended :
    { mode : Mode
    , theme : Theme
    }
    -> Element Never
disabledRecommended { mode, theme } =
    el
        ([ width fill
         , height fill
         ]
            ++ (case mode of
                    Mode.Recommended ->
                        [ theme |> Color.primary500 |> Background.color
                        , Border.rounded 4
                        ]

                    Mode.Advanced ->
                        []
               )
        )
        (el
            [ centerX
            , centerY
            , theme |> Color.light100 |> Font.color
            , Font.size 14
            ]
            (text "Recommended")
        )


emptyRecommended : Theme -> Element Never
emptyRecommended theme =
    el
        [ width fill
        , height fill
        , theme |> Color.primary500 |> Background.color
        , Border.rounded 4
        ]
        (el
            [ centerX
            , centerY
            , theme |> Color.light100 |> Font.color
            , Font.size 14
            ]
            (text "Recommended")
        )


viewAdvanced :
    { onChange : Mode -> msg
    , mode : Mode
    , theme : Theme
    }
    -> Element msg
viewAdvanced { onChange, mode, theme } =
    Input.button
        ([ width fill
         , height fill
         ]
            ++ (case mode of
                    Mode.Advanced ->
                        [ theme |> Color.primary500 |> Background.color
                        , Border.rounded 4
                        ]

                    _ ->
                        []
               )
        )
        { onPress =
            case mode of
                Mode.Recommended ->
                    Just (onChange Mode.Advanced)

                Mode.Advanced ->
                    Nothing
        , label =
            el
                [ centerX
                , centerY
                , theme |> Color.light100 |> Font.color
                , Font.size 14
                ]
                (text "Advanced")
        }


disabledAdvanced : { mode : Mode, theme : Theme } -> Element Never
disabledAdvanced { mode, theme } =
    el
        ([ width fill
         , height fill
         ]
            ++ (case mode of
                    Mode.Recommended ->
                        []

                    Mode.Advanced ->
                        [ theme |> Color.primary500 |> Background.color
                        , Border.rounded 4
                        ]
               )
        )
        (el
            [ centerX
            , centerY
            , theme |> Color.light100 |> Font.color
            , Font.size 14
            ]
            (text "Advanced")
        )


emptyAdvanced : Theme -> Element Never
emptyAdvanced theme =
    el
        [ width fill
        , height fill
        ]
        (el
            [ centerX
            , centerY
            , theme |> Color.light100 |> Font.color
            , Font.size 14
            ]
            (text "Advanced")
        )
