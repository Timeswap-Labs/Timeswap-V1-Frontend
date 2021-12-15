module Page.Transaction.Slider exposing (disabled, view)

import Data.Percent as Percent exposing (Percent)
import Element
    exposing
        ( Element
        , alignLeft
        , alignRight
        , behindContent
        , centerY
        , column
        , el
        , fill
        , height
        , moveRight
        , newTabLink
        , none
        , paddingXY
        , px
        , row
        , shrink
        , spacing
        , text
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Utility.Color as Color


view :
    { onChange : Float -> msg
    , click : msg
    , percent : Percent
    , min : Float
    , max : Float
    , learnMore : String
    }
    -> Element msg
view param =
    column
        [ width fill
        , height shrink
        , spacing 10
        ]
        [ row
            [ width fill
            , height shrink
            , paddingXY 0 3
            , Font.size 14
            ]
            [ el
                [ alignLeft
                , Font.regular
                , Font.color Color.transparent500
                ]
                (text "Adjust your APR")
            , newTabLink
                [ alignRight
                , Font.regular
                , Font.color Color.primary500
                ]
                { url = param.learnMore
                , label = text "Learn more"
                }
            ]
        , column
            [ width fill
            , height shrink
            , spacing 6
            ]
            [ slider param
            , row
                [ width fill
                , height shrink
                , paddingXY 0 2
                , Font.size 12
                , Font.color Color.transparent300
                ]
                [ el
                    [ alignLeft
                    , Font.regular
                    ]
                    (text "Low")
                , el
                    [ alignRight
                    , Font.regular
                    ]
                    (text "High")
                ]
            ]
        ]


slider :
    { param
        | onChange : Float -> msg
        , click : msg
        , percent : Percent
        , min : Float
        , max : Float
    }
    -> Element msg
slider { onChange, click, percent, min, max } =
    Input.slider
        [ width fill
        , height <| px 20
        , el
            [ width fill
            , height <| px 2
            , centerY
            , Background.color Color.transparent100
            ]
            none
            |> behindContent
        , Events.onClick click
        ]
        { onChange = onChange
        , label = Input.labelHidden "slider"
        , min = min
        , max = max
        , value = percent |> Percent.toFloat
        , thumb =
            Input.thumb
                [ width <| px 20
                , height <| px 20
                , Background.color Color.primary500
                , Border.rounded 999
                , Border.width 2
                , Border.color Color.transparent500
                ]
        , step = Just 1
        }


disabled : Percent -> Element Never
disabled percent =
    column
        [ width fill
        , height shrink
        , spacing 10
        ]
        [ row
            [ width fill
            , height shrink
            , paddingXY 0 3
            , Font.size 14
            ]
            [ el
                [ alignLeft
                , Font.regular
                , Font.color Color.transparent500
                ]
                (text "Adjust your APR")
            , el
                [ alignRight
                , Font.regular
                , Font.color Color.primary500
                ]
                (text "Learn more")
            ]
        , column
            [ width fill
            , height shrink
            , spacing 6
            ]
            [ disabledSlider percent
            , row
                [ width fill
                , height shrink
                , paddingXY 0 2
                , Font.size 12
                , Font.color Color.transparent300
                ]
                [ el
                    [ alignLeft
                    , Font.regular
                    ]
                    (text "Low")
                , el
                    [ alignRight
                    , Font.regular
                    ]
                    (text "High")
                ]
            ]
        ]


disabledSlider : Percent -> Element Never
disabledSlider percent =
    el
        [ width fill
        , height <| px 20
        , el
            [ width fill
            , height <| px 2
            , centerY
            , Background.color Color.transparent100
            ]
            none
            |> behindContent
        ]
        (el
            [ width <| px 20
            , height <| px 20
            , alignLeft
            , 291
                |> (*) (percent |> Percent.toFloat)
                |> (\dividend -> dividend / 128)
                |> moveRight
            , Background.color Color.primary500
            , Border.rounded 999
            , Border.width 2
            , Border.color Color.transparent500
            ]
            none
        )
