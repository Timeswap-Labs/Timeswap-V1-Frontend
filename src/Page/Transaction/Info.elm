module Page.Transaction.Info exposing (emptyAPR, emptyCDP, lendAPR, lendCDP)

import Data.CDP exposing (CDP)
import Data.Pair exposing (Pair)
import Data.Spot as Spot exposing (Spot)
import Element
    exposing
        ( Element
        , alignRight
        , centerY
        , column
        , el
        , fill
        , height
        , none
        , paddingXY
        , px
        , row
        , shrink
        , spacing
        , text
        , width
        )
import Element.Font as Font
import Utility.Color as Color
import Utility.Truncate as Truncate


lendAPR : Float -> Element msg
lendAPR float =
    column
        [ width fill
        , height shrink
        , spacing 5
        ]
        [ el
            [ width shrink
            , height shrink
            , Font.size 14
            , paddingXY 0 3
            , Font.color Color.primary400
            ]
            (text "APR")
        , el
            [ width fill
            , height <| px 24
            , Font.size 18
            , paddingXY 0 3
            , Font.color Color.positive400
            ]
            ([ float
                |> (*) 10000
                |> truncate
                |> toFloat
                |> (/) 100
                |> String.fromFloat
             , "%"
             ]
                |> String.concat
                |> text
            )
        ]


lendCDP :
    { model | spot : Spot }
    ->
        { onMouseEnter : tooltip -> msg
        , onMouseLeave : msg
        , cdpTooltip : tooltip
        , symbolTooltip : tooltip
        , opened : Maybe tooltip
        , pair : Pair
        , cdp : CDP
        }
    -> Element msg
lendCDP { spot } param =
    column
        [ width fill
        , height shrink
        , spacing 5
        ]
        [ row
            [ width fill
            , height shrink
            ]
            [ el
                [ width shrink
                , height shrink
                , Font.size 14
                , paddingXY 0 3
                , Font.color Color.primary400
                ]
                (text "CDP")
            , if
                case spot of
                    Spot.None ->
                        False

                    Spot.Uniswap ->
                        param.cdp.percent
                            |> Maybe.map (\_ -> True)
                            |> Maybe.withDefault False
              then
                none

              else
                el
                    [ width shrink
                    , height shrink
                    , alignRight
                    , centerY
                    ]
                    (Truncate.viewCDPSymbol
                        { onMouseEnter = param.onMouseEnter
                        , onMouseLeave = param.onMouseLeave
                        , tooltip = param.symbolTooltip
                        , opened = param.opened
                        , pair = param.pair
                        }
                    )
            ]
        , el
            [ width fill
            , height <| px 24
            ]
            ((case spot of
                Spot.None ->
                    Nothing

                Spot.Uniswap ->
                    param.cdp.percent
             )
                |> Maybe.map
                    (\percent ->
                        el
                            [ width fill
                            , height <| px 24
                            , Font.size 18
                            , paddingXY 0 3
                            , (if percent <= 1 then
                                Color.negative400

                               else
                                Color.warning400
                              )
                                |> Font.color
                            ]
                            ([ percent
                                |> (*) 10000
                                |> truncate
                                |> toFloat
                                |> (/) 100
                                |> String.fromFloat
                             , "%"
                             ]
                                |> String.concat
                                |> text
                            )
                    )
                |> Maybe.withDefault
                    (Truncate.viewCDP
                        { onMouseEnter = param.onMouseEnter
                        , onMouseLeave = param.onMouseLeave
                        , tooltip = param.cdpTooltip
                        , opened = param.opened
                        , pair = param.pair
                        , cdp = param.cdp.ratio
                        }
                    )
            )
        ]


emptyAPR : Element Never
emptyAPR =
    column
        [ width fill
        , height shrink
        , spacing 5
        ]
        [ el
            [ width shrink
            , height shrink
            , Font.size 14
            , paddingXY 0 3
            , Font.color Color.primary400
            ]
            (text "APR")
        , el
            [ width fill
            , height <| px 24
            ]
            none
        ]


emptyCDP : Element Never
emptyCDP =
    column
        [ width fill
        , height shrink
        , spacing 5
        ]
        [ el
            [ width shrink
            , height shrink
            , Font.size 14
            , paddingXY 0 3
            , Font.color Color.primary400
            ]
            (text "CDP")
        , el
            [ width fill
            , height <| px 24
            ]
            none
        ]
