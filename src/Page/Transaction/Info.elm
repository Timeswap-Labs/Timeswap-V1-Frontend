module Page.Transaction.Info exposing
    ( borrowAPR
    , borrowCDP
    , emptyAPR
    , emptyCDP
    , lendAPR
    , lendCDP
    )

import Data.CDP exposing (CDP)
import Data.Pair exposing (Pair)
import Data.PriceFeed as PriceFeed exposing (PriceFeed)
import Data.Remote as Remote exposing (Remote(..))
import Element
    exposing
        ( Element
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
import Utility.Calculate as Calculate
import Utility.Color as Color
import Utility.Loading as Loading
import Utility.Truncate as Truncate


lendAPR : Remote failure Float -> Element msg
lendAPR remote =
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
        , case remote of
            Loading timeline ->
                el
                    [ width shrink
                    , height <| px 24
                    ]
                    (el
                        [ width shrink
                        , height shrink
                        , centerY
                        ]
                        (Loading.view timeline)
                    )

            Failure _ ->
                el
                    [ width shrink
                    , height <| px 24
                    ]
                    none

            Success float ->
                el
                    [ width fill
                    , height <| px 24
                    , Font.size 18
                    , paddingXY 0 3
                    , (if float == 0 then
                        Color.transparent200

                       else
                        Color.positive400
                      )
                        |> Font.color
                    ]
                    (Calculate.apr float)
        ]


borrowAPR : Remote failure Float -> Element msg
borrowAPR remote =
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
        , case remote of
            Loading timeline ->
                el
                    [ width shrink
                    , height <| px 24
                    ]
                    (el
                        [ width shrink
                        , height shrink
                        , centerY
                        ]
                        (Loading.view timeline)
                    )

            Failure _ ->
                el
                    [ width shrink
                    , height <| px 24
                    ]
                    none

            Success float ->
                el
                    [ width fill
                    , height <| px 24
                    , Font.size 18
                    , paddingXY 0 3
                    , (if float == 0 then
                        Color.transparent200

                       else
                        Color.negative400
                      )
                        |> Font.color
                    ]
                    (Calculate.apr float)
        ]


lendCDP :
    { model | priceFeed : PriceFeed }
    ->
        { onMouseEnter : tooltip -> msg
        , onMouseLeave : msg
        , cdpTooltip : tooltip
        , symbolTooltip : tooltip
        , opened : Maybe tooltip
        , pair : Pair
        , cdp : Remote foalure CDP
        }
    -> Element msg
lendCDP { priceFeed } param =
    column
        [ width fill
        , height shrink
        , spacing 5
        ]
        [ row
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
            , case param.cdp of
                Success cdp ->
                    if
                        case priceFeed of
                            PriceFeed.Ignore ->
                                False

                            PriceFeed.Utilize ->
                                cdp.percent
                                    |> Maybe.map (\_ -> True)
                                    |> Maybe.withDefault False
                    then
                        none

                    else
                        el
                            [ width shrink
                            , height shrink
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

                _ ->
                    none
            ]
        , case param.cdp of
            Loading timeline ->
                el
                    [ width shrink
                    , height <| px 24
                    ]
                    (el
                        [ width shrink
                        , height shrink
                        , centerY
                        ]
                        (Loading.view timeline)
                    )

            Failure _ ->
                el
                    [ width shrink
                    , height <| px 24
                    ]
                    none

            Success cdp ->
                el
                    [ width shrink
                    , height <| px 24
                    ]
                    ((case priceFeed of
                        PriceFeed.Ignore ->
                            Nothing

                        PriceFeed.Utilize ->
                            cdp.percent
                     )
                        |> Maybe.map
                            (\percent ->
                                el
                                    [ width fill
                                    , height <| px 24
                                    , Font.size 18
                                    , paddingXY 0 3
                                    , (if percent == 0 then
                                        Color.transparent200

                                       else if percent <= 1 then
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
                                        |> (\number -> number / 100)
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
                                , cdp = cdp.ratio
                                }
                            )
                    )
        ]


borrowCDP :
    { model | priceFeed : PriceFeed }
    ->
        { onMouseEnter : tooltip -> msg
        , onMouseLeave : msg
        , cdpTooltip : tooltip
        , symbolTooltip : tooltip
        , opened : Maybe tooltip
        , pair : Pair
        , cdp : Remote failure CDP
        }
    -> Element msg
borrowCDP { priceFeed } param =
    column
        [ width fill
        , height shrink
        , spacing 5
        ]
        [ row
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
            , case param.cdp of
                Success cdp ->
                    if
                        case priceFeed of
                            PriceFeed.Ignore ->
                                False

                            PriceFeed.Utilize ->
                                cdp.percent
                                    |> Maybe.map (\_ -> True)
                                    |> Maybe.withDefault False
                    then
                        none

                    else
                        el
                            [ width shrink
                            , height shrink
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

                _ ->
                    none
            ]
        , case param.cdp of
            Loading timeline ->
                el
                    [ width shrink
                    , height <| px 24
                    ]
                    (el
                        [ width shrink
                        , height shrink
                        , centerY
                        ]
                        (Loading.view timeline)
                    )

            Failure _ ->
                el
                    [ width shrink
                    , height <| px 24
                    ]
                    none

            Success cdp ->
                el
                    [ width shrink
                    , height <| px 24
                    ]
                    ((case priceFeed of
                        PriceFeed.Ignore ->
                            Nothing

                        PriceFeed.Utilize ->
                            cdp.percent
                     )
                        |> Maybe.map
                            (\percent ->
                                el
                                    [ width fill
                                    , height <| px 24
                                    , Font.size 18
                                    , paddingXY 0 3
                                    , (if percent == 0 then
                                        Color.transparent200

                                       else if percent <= 1 then
                                        Color.positive400

                                       else
                                        Color.warning400
                                      )
                                        |> Font.color
                                    ]
                                    ([ percent
                                        |> (*) 10000
                                        |> truncate
                                        |> toFloat
                                        |> (\number -> number / 100)
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
                                , cdp = cdp.ratio
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
