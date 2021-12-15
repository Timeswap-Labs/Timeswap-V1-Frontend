module Utility.Calculate exposing (apr, cdp)

import Data.CDP exposing (CDP)
import Data.Pair exposing (Pair)
import Data.PriceFeed as PriceFeed exposing (PriceFeed)
import Element
    exposing
        ( Color
        , Element
        , el
        , fill
        , height
        , paddingXY
        , px
        , text
        , width
        )
import Element.Font as Font
import Utility.Color as Color
import Utility.Truncate as Truncate


apr : Float -> Element msg
apr float =
    [ float
        |> (*) 10000
        |> truncate
        |> toFloat
        |> (\number -> number / 100)
        |> String.fromFloat
    , "%"
    ]
        |> String.concat
        |> text


cdp :
    { onMouseEnter : tooltip -> msg
    , onMouseLeave : msg
    , cdpTooltip : tooltip
    , opened : Maybe tooltip
    , pair : Pair
    , cdp : CDP
    }
    -> PriceFeed
    -> Color
    -> Int
    -> Element msg
cdp params spot color fontSize =
    el
        [ width fill
        , height <| px 24
        ]
        ((case spot of
            PriceFeed.Ignore ->
                Nothing

            PriceFeed.Utilize ->
                params.cdp.percent
         )
            |> Maybe.map
                (\percent ->
                    el
                        [ width fill
                        , height <| px 24
                        , Font.size fontSize
                        , paddingXY 0 3
                        , (if percent <= 1 then
                            color

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
                    { onMouseEnter = params.onMouseEnter
                    , onMouseLeave = params.onMouseLeave
                    , tooltip = params.cdpTooltip
                    , opened = params.opened
                    , pair = params.pair
                    , cdp = params.cdp.ratio
                    }
                )
        )
