module Utility.Duration exposing (viewMaturity)

import Data.ChosenZone as ChosenZone exposing (ChosenZone)
import Data.Maturity as Maturity exposing (Maturity)
import Data.Offset as Offset exposing (Offset)
import Element
    exposing
        ( Element
        , below
        , el
        , height
        , none
        , paddingEach
        , shrink
        , text
        , width
        )
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Time exposing (Posix)
import Utility.Color as Color
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
        , Border.color Color.transparent200
        , Events.onMouseEnter
            (param.onMouseEnter param.tooltip)
        , Events.onMouseLeave param.onMouseLeave
        , (if param.opened == Just param.tooltip then
            el
                [ Font.size 14
                , Font.color Color.transparent300
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
                |> Tooltip.belowAlignLeft

           else
            none
          )
            |> below
        , Font.size 14
        , Font.color Color.light100
        ]
        (param.maturity
            |> Maturity.toString
                (case param.chosenZone of
                    ChosenZone.Here ->
                        param.offset
                            |> Offset.toZone

                    ChosenZone.UTC ->
                        Time.utc
                )
            |> text
        )