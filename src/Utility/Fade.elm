module Utility.Fade exposing (view, viewLP)

import Data.Token exposing (Token)
import Data.Uint as Uint exposing (Uint)
import Element
    exposing
        ( Element
        , el
        , height
        , none
        , paddingXY
        , row
        , shrink
        , text
        , width
        )
import Element.Font as Font
import Utility.Color as Color


view : Token -> Uint -> Element msg
view token amount =
    if amount |> Uint.isZero then
        el
            [ width shrink
            , height shrink
            , Font.color Color.transparent200
            , Font.size 16
            ]
            (text "0.0")

    else
        row
            [ width shrink
            , height shrink
            ]
            (amount
                |> Uint.toAmount token
                |> cut
                |> (\( unFaded, faded ) ->
                        [ el
                            [ width shrink
                            , height shrink
                            , Font.color Color.light100
                            , Font.size 16
                            ]
                            (text unFaded)
                        , el
                            [ width shrink
                            , height shrink
                            , Font.color Color.transparent200
                            , Font.size 16
                            ]
                            (text faded)
                        ]
                   )
            )


viewLP : Uint -> Element msg
viewLP amount =
    if amount |> Uint.isZero then
        none

    else
        row
            [ width shrink
            , height shrink
            ]
            (amount
                |> Uint.toLP
                |> cut
                |> (\( unFaded, faded ) ->
                        [ el
                            [ width shrink
                            , height shrink
                            , Font.color Color.light100
                            , Font.size 16
                            ]
                            (text unFaded)
                        , el
                            [ width shrink
                            , height shrink
                            , Font.color Color.transparent300
                            , Font.size 16
                            ]
                            (text faded)
                        ]
                   )
            )


cut : String -> ( String, String )
cut full =
    full
        |> String.split "."
        |> (\list ->
                case list of
                    whole :: fraction :: _ ->
                        ( [ whole
                          , fraction |> String.left 2
                          ]
                            |> String.join "."
                        , fraction |> String.dropLeft 2
                        )

                    whole :: _ ->
                        ( whole
                        , ""
                        )

                    _ ->
                        ( full
                        , ""
                        )
           )
