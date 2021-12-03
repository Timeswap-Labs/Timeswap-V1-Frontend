module Page.Transaction.Slider exposing (disabled, view)

import Data.Percent as Percent exposing (Percent)
import Element
    exposing
        ( Element
        , alignLeft
        , behindContent
        , centerY
        , el
        , fill
        , height
        , moveRight
        , none
        , px
        , shrink
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Input as Input
import Utility.Color as Color


view :
    { onChange : Float -> msg
    , click : msg
    , percent : Percent
    }
    -> Element msg
view { onChange, click, percent } =
    Input.slider
        [ width fill
        , height shrink
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
        , min = 0
        , max = 128
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
    el
        [ width fill
        , height shrink
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
