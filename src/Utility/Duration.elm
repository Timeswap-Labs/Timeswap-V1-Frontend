module Utility.Duration exposing (viewMaturity)

import Data.ChosenZone as ChosenZone exposing (ChosenZone)
import Data.Maturity as Maturity exposing (Maturity)
import Element
    exposing
        ( Element
        , alignLeft
        , below
        , centerY
        , el
        , height
        , maximum
        , none
        , paddingEach
        , shrink
        , text
        , width
        )
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Time exposing (Posix, Zone)
import Utility.Color as Color
import Utility.Direction exposing (Direction)
import Utility.Tooltip as Tooltip


viewMaturity :
    { model
        | time : Posix
        , zone : Zone
        , chosenZone : ChosenZone
    }
    ->
        { tooltip :
            { align : Direction ()
            , move : Direction Int
            , onMouseEnterMsg : tooltip -> msg
            , onMouseLeaveMsg : msg
            , given : tooltip
            , opened : Maybe tooltip
            }
        , maturity : Maturity
        }
    -> Element msg
viewMaturity { time, zone, chosenZone } { tooltip, maturity } =
    el
        [ width shrink
        , height shrink
        , paddingEach
            { top = 4
            , right = 4
            , bottom = 3
            , left = 4
            }
        , alignLeft
        , centerY
        , Border.widthEach
            { top = 0
            , right = 0
            , bottom = 1
            , left = 0
            }
        , Border.dashed
        , Border.color Color.transparent200
        , Font.size 16
        , Font.color Color.light300
        , Events.onMouseEnter
            (tooltip.onMouseEnterMsg tooltip.given)
        , Events.onMouseLeave
            tooltip.onMouseLeaveMsg
        , tooltip.opened
            |> Maybe.map
                (\openedTooltip ->
                    if
                        openedTooltip
                            == tooltip.given
                    then
                        Tooltip.below
                            { width =
                                shrink
                                    |> maximum 335
                            , align = tooltip.align
                            , move = tooltip.move
                            , text =
                                case
                                    maturity
                                        |> Maturity.toDuration
                                            time
                                of
                                    Maturity.Active string ->
                                        [ "Matures in"
                                        , string
                                        ]
                                            |> String.join " "

                                    Maturity.Matured string ->
                                        [ "Matured"
                                        , string
                                        , "ago"
                                        ]
                                            |> String.join " "
                            }

                    else
                        none
                )
            |> Maybe.withDefault none
            |> below
        ]
        ((case chosenZone of
            ChosenZone.Here ->
                Just zone

            ChosenZone.UTC ->
                Just Time.utc

            ChosenZone.Unix ->
                Nothing
         )
            |> Maybe.map
                (\givenZone ->
                    maturity
                        |> Maturity.toString
                            givenZone
                )
            |> Maybe.withDefault
                (maturity
                    |> Maturity.toUnix
                    |> String.fromInt
                )
            |> text
        )
