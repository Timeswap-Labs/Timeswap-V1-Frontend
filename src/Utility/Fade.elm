module Utility.Fade exposing (view, viewLP)

import Data.Theme exposing (Theme)
import Data.Token exposing (Token)
import Data.Uint as Uint exposing (Uint)
import Element
    exposing
        ( Element
        , alignRight
        , el
        , height
        , row
        , shrink
        , text
        , width
        )
import Element.Font as Font
import Utility.Color as Color
import Utility.ThemeColor as ThemeColor


view : Theme -> Token -> Uint -> Element msg
view theme token amount =
    if amount |> Uint.isZero then
        el
            [ width shrink
            , height shrink
            , theme |> ThemeColor.textLight |> Font.color
            , Font.size 16
            , alignRight
            ]
            (text "0.0")

    else
        row
            [ width shrink
            , height shrink
            , Font.alignRight
            ]
            (amount
                |> Uint.toAmount token
                |> cut
                |> (\( unFaded, faded ) ->
                        [ el
                            [ width shrink
                            , height shrink
                            , theme |> ThemeColor.text |> Font.color
                            , Font.size 16
                            , Font.alignRight
                            ]
                            (text unFaded)
                        , el
                            [ width shrink
                            , height shrink
                            , theme |> ThemeColor.placeholder |> Font.color
                            , Font.size 16
                            , Font.alignRight
                            ]
                            (text faded)
                        ]
                   )
            )


viewLP : Theme -> Uint -> Element msg
viewLP theme amount =
    if amount |> Uint.isZero then
        el
            [ width shrink
            , height shrink
            , theme |> ThemeColor.placeholder |> Font.color
            , Font.size 16
            ]
            (text "0.0")

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
