module Utility.Duration exposing (viewExpiredMaturity, viewMaturity)

import Data.ChosenZone as ChosenZone exposing (ChosenZone)
import Data.Images exposing (Images)
import Data.Maturity as Maturity exposing (Maturity)
import Data.Offset as Offset exposing (Offset)
import Data.Theme exposing (Theme)
import Element
    exposing
        ( Element
        , below
        , centerX
        , centerY
        , el
        , height
        , inFront
        , none
        , paddingEach
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
import Time exposing (Posix)
import Utility.Color as Color
import Utility.Image as Image
import Utility.ThemeColor as ThemeColor
import Utility.Tooltip as Tooltip


viewMaturity :
    { onMouseEnter : tooltip -> msg
    , onMouseLeave : msg
    , tooltip : tooltip
    , opened : Maybe tooltip
    , time : Posix
    , offset : Offset
    , chosenZone : ChosenZone
    , maturity : Maturity
    , theme : Theme
    }
    -> Element msg
viewMaturity param =
    el
        [ width shrink
        , height shrink
        , paddingEach
            { top = 3
            , right = 0
            , bottom = 2
            , left = 0
            }
        , Border.widthEach
            { top = 0
            , right = 0
            , bottom = 1
            , left = 0
            }
        , Border.dashed
        , param.theme |> ThemeColor.placeholder |> Border.color
        , Events.onMouseEnter
            (param.onMouseEnter param.tooltip)
        , Events.onMouseLeave param.onMouseLeave
        , (if param.opened == Just param.tooltip then
            el
                [ Font.size 14
                , param.theme |> ThemeColor.textLight |> Font.color
                ]
                (param.maturity
                    |> Maturity.toDuration param.time
                    |> (\status ->
                            case status of
                                Maturity.Active duration ->
                                    [ "Maturing in"
                                    , duration
                                    ]
                                        |> String.join " "

                                Maturity.Matured duration ->
                                    [ "Matured"
                                    , duration
                                    , "ago"
                                    ]
                                        |> String.join " "
                       )
                    |> text
                )
                |> Tooltip.belowAlignLeft param.theme

           else
            none
          )
            |> below
        , Font.size 14
        , param.theme |> ThemeColor.text |> Font.color
        ]
        ((case param.chosenZone of
            ChosenZone.Here ->
                param.maturity
                    |> Maturity.toString
                        (param.offset
                            |> Offset.toZone
                        )

            ChosenZone.UTC ->
                param.maturity
                    |> Maturity.toString Time.utc

            ChosenZone.Unix ->
                param.maturity
                    |> Maturity.toUnix
                    |> String.fromInt
         )
            |> text
        )


viewExpiredMaturity :
    { onMouseEnter : tooltip -> msg
    , onMouseLeave : msg
    , tooltip : tooltip
    , opened : Maybe tooltip
    , time : Posix
    , offset : Offset
    , chosenZone : ChosenZone
    , maturity : Maturity
    , theme : Theme
    , images : Images
    }
    -> Element msg
viewExpiredMaturity param =
    if
        param.maturity
            |> Maturity.isActive param.time
    then
        viewMaturity
            { onMouseEnter = param.onMouseEnter
            , onMouseLeave = param.onMouseLeave
            , tooltip = param.tooltip
            , opened = param.opened
            , time = param.time
            , offset = param.offset
            , chosenZone = param.chosenZone
            , maturity = param.maturity
            , theme = param.theme
            }

    else
        row
            [ centerY
            , spacing 8
            ]
            [ el
                [ width <| px 40
                , height <| px 40
                , Border.rounded 999
                , Background.color Color.positive100
                , (param.images
                    |> Image.matured
                        [ width <| px 20
                        , height <| px 20
                        , centerX
                        , centerY
                        , Font.center
                        ]
                  )
                    |> inFront
                ]
                none
            , el
                [ width shrink
                , height shrink
                , paddingEach
                    { top = 3
                    , right = 0
                    , bottom = 2
                    , left = 0
                    }
                , Border.widthEach
                    { top = 0
                    , right = 0
                    , bottom = 1
                    , left = 0
                    }
                , Border.dashed
                , param.theme |> ThemeColor.placeholder |> Border.color
                , Events.onMouseEnter
                    (param.onMouseEnter param.tooltip)
                , Events.onMouseLeave param.onMouseLeave
                , (if param.opened == Just param.tooltip then
                    el
                        [ Font.size 14
                        , param.theme |> ThemeColor.textLight |> Font.color
                        ]
                        (param.maturity
                            |> Maturity.toDuration param.time
                            |> (\status ->
                                    case status of
                                        Maturity.Matured duration ->
                                            [ "Matured"
                                            , duration
                                            , "ago"
                                            ]
                                                |> String.join " "

                                        _ ->
                                            "Matured"
                               )
                            |> text
                        )
                        |> Tooltip.belowAlignLeft param.theme

                   else
                    none
                  )
                    |> below
                , Font.size 14
                , param.theme |> ThemeColor.text |> Font.color
                ]
                ("Matured" |> text)
            ]
