module Page.Transaction.Info exposing
    ( borrowAPR
    , borrowCDP
    , emptyAPR
    , emptyCDP
    , lendAPR
    , lendCDP
    )

import Data.CDP exposing (CDP)
import Data.MaturityParam exposing (MaturityParam(..))
import Data.Pair exposing (Pair)
import Data.PriceFeed as PriceFeed exposing (PriceFeed)
import Data.Remote exposing (Remote(..))
import Data.Theme exposing (Theme)
import Data.Uint as Uint
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
import Page.PoolInfo exposing (PoolInfo)
import Utility.Calculate as Calculate
import Utility.Color as Color
import Utility.Input as Input
import Utility.Loading as Loading
import Utility.ThemeColor as ThemeColor
import Utility.Truncate as Truncate


lendAPR :
    Remote failure Float
    -> String
    -> Maybe PoolInfo
    -> Theme
    -> Element msg
lendAPR remote assetIn maybePoolInfo theme =
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
            , theme |> ThemeColor.textLight |> Font.color
            ]
            ((case maybePoolInfo of
                Just _ ->
                    if assetIn |> Input.isZero then
                        "Pool Max APR"

                    else
                        "APR"

                _ ->
                    "APR"
             )
                |> text
            )
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
                        (Loading.view timeline theme)
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
                    , Font.color Color.warning500
                    ]
                    ((case maybePoolInfo of
                        Just poolInfo ->
                            if assetIn |> Input.isZero then
                                poolInfo.apr

                            else
                                float

                        _ ->
                            float
                     )
                        |> Calculate.apr
                    )
        ]


borrowAPR : Remote failure Float -> String -> Maybe PoolInfo -> Theme -> Element msg
borrowAPR remote assetOut maybePoolInfo theme =
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
            , theme |> ThemeColor.textLight |> Font.color
            ]
            ((case maybePoolInfo of
                Just _ ->
                    if assetOut |> Input.isZero then
                        "Pool Max APR"

                    else
                        "APR"

                _ ->
                    "APR"
             )
                |> text
            )
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
                        (Loading.view timeline theme)
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
                    , Font.color Color.warning500
                    ]
                    ((case maybePoolInfo of
                        Just poolInfo ->
                            if assetOut |> Input.isZero then
                                poolInfo.apr

                            else
                                float

                        _ ->
                            float
                     )
                        |> Calculate.apr
                    )
        ]


lendCDP :
    { model | priceFeed : PriceFeed, theme : Theme }
    ->
        { onMouseEnter : tooltip -> msg
        , onMouseLeave : msg
        , cdpTooltip : tooltip
        , symbolTooltip : tooltip
        , opened : Maybe tooltip
        , pair : Pair
        , cdp : Remote failure CDP
        , poolInfo : Maybe PoolInfo
        , assetIn : String
        }
    -> Element msg
lendCDP { priceFeed, theme } param =
    column
        [ width fill
        , height shrink
        , spacing 5
        ]
        [ row
            [ width fill
            , height shrink
            , centerY
            , spacing 5
            ]
            [ el
                [ width shrink
                , height shrink
                , Font.size 14
                , centerY
                , paddingXY 0 3
                , theme |> ThemeColor.textLight |> Font.color
                ]
                ((case param.poolInfo of
                    Just _ ->
                        if param.assetIn |> Input.isZero then
                            "Pool Min CDP"

                        else
                            "CDP"

                    _ ->
                        "CDP"
                 )
                    |> text
                )
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
                                , theme = theme
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
                        (Loading.view timeline theme)
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
                            case param.poolInfo of
                                Just poolInfo ->
                                    if param.assetIn |> Input.isZero then
                                        poolInfo.cdp.percent

                                    else
                                        cdp.percent

                                _ ->
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
                                        theme |> ThemeColor.textLight

                                       else if percent < 1 then
                                        Color.negative400

                                       else
                                        Color.warning500
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
                                , cdp =
                                    case ( Uint.isZero cdp.ratio, param.poolInfo ) of
                                        ( True, Just poolInfo ) ->
                                            poolInfo.cdp.ratio

                                        ( _, _ ) ->
                                            cdp.ratio
                                , theme = theme
                                , styles =
                                    [ Font.size 18
                                    ]
                                }
                            )
                    )
        ]


borrowCDP :
    { model | priceFeed : PriceFeed, theme : Theme }
    ->
        { onMouseEnter : tooltip -> msg
        , onMouseLeave : msg
        , cdpTooltip : tooltip
        , symbolTooltip : tooltip
        , opened : Maybe tooltip
        , pair : Pair
        , cdp : Remote failure CDP
        , poolInfo : Maybe PoolInfo
        , assetOut : String
        }
    -> Element msg
borrowCDP { priceFeed, theme } param =
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
                , theme |> ThemeColor.textLight |> Font.color
                ]
                ((case param.poolInfo of
                    Just _ ->
                        if param.assetOut |> Input.isZero then
                            "Pool Min CDP"

                        else
                            "CDP"

                    _ ->
                        "CDP"
                 )
                    |> text
                )
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
                                , theme = theme
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
                        (Loading.view timeline theme)
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
                            case param.poolInfo of
                                Just poolInfo ->
                                    if param.assetOut |> Input.isZero then
                                        poolInfo.cdp.percent

                                    else
                                        cdp.percent

                                _ ->
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
                                        theme |> ThemeColor.textLight

                                       else if percent < 1 then
                                        Color.negative400

                                       else
                                        Color.warning500
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
                                , cdp =
                                    case ( Uint.isZero cdp.ratio, param.poolInfo ) of
                                        ( True, Just poolInfo ) ->
                                            poolInfo.cdp.ratio

                                        ( _, _ ) ->
                                            cdp.ratio
                                , theme = theme
                                , styles =
                                    [ Font.size 18
                                    ]
                                }
                            )
                    )
        ]


emptyAPR : Theme -> Element Never
emptyAPR theme =
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
            , theme |> ThemeColor.textLight |> Font.color
            ]
            (text "APR")
        , el
            [ width fill
            , height <| px 24
            ]
            none
        ]


emptyCDP : Theme -> Element Never
emptyCDP theme =
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
            , theme |> ThemeColor.textLight |> Font.color
            ]
            (text "CDP")
        , el
            [ width fill
            , height <| px 24
            ]
            none
        ]
