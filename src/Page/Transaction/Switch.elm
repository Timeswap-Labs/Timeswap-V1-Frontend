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
import Utility.Color as Color
import Utility.ThemeColor as ThemeColor


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
        , param.theme |> ThemeColor.border |> Background.color
        , Border.rounded 8
        ]
        [ viewRecommended param
        , viewAdvanced param
        ]


disabled : { mode : Mode, theme : Theme } -> Element Never
disabled ({ theme } as model) =
    row
        [ width fill
        , height <| px 44
        , padding 4
        , spacing 16
        , theme |> ThemeColor.border |> Background.color
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
        , theme |> ThemeColor.border |> Background.color
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
                        [ theme |> ThemeColor.primaryBtn |> Background.color
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
                , (case mode of
                    Mode.Recommended ->
                        Color.light100

                    Mode.Advanced ->
                        theme |> ThemeColor.textLight
                  )
                    |> Font.color
                , Font.size 16
                , Font.bold
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
                        [ theme |> ThemeColor.primaryBtn |> Background.color
                        , Border.rounded 4
                        ]

                    Mode.Advanced ->
                        []
               )
        )
        (el
            [ centerX
            , centerY
            , (case mode of
                Mode.Recommended ->
                    Color.light100

                Mode.Advanced ->
                    theme |> ThemeColor.textLight
              )
                |> Font.color
            , Font.size 16
            , Font.bold
            ]
            (text "Recommended")
        )


emptyRecommended : Theme -> Element Never
emptyRecommended theme =
    el
        [ width fill
        , height fill
        , theme |> ThemeColor.primaryBtn |> Background.color
        , Border.rounded 4
        ]
        (el
            [ centerX
            , centerY
            , Color.light100 |> Font.color
            , Font.size 16
            , Font.bold
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
                        [ theme |> ThemeColor.primaryBtn |> Background.color
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
                , (case mode of
                    Mode.Recommended ->
                        theme |> ThemeColor.textLight

                    Mode.Advanced ->
                        Color.light100
                  )
                    |> Font.color
                , Font.size 16
                , Font.bold
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
                        [ theme |> ThemeColor.primaryBtn |> Background.color
                        , Border.rounded 4
                        ]
               )
        )
        (el
            [ centerX
            , centerY
            , (case mode of
                Mode.Recommended ->
                    theme |> ThemeColor.textLight

                Mode.Advanced ->
                    Color.light100
              )
                |> Font.color
            , Font.size 16
            , Font.bold
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
            , theme |> ThemeColor.textLight |> Font.color
            , Font.size 16
            , Font.bold
            ]
            (text "Advanced")
        )
