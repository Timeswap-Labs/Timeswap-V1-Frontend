module Pages.PairMarket.ListPools exposing (..)

import Data.Device as Device exposing (Device)
import Data.Maturity as Maturity exposing (Maturity)
import Data.ZoneInfo as ZoneInfo exposing (ZoneInfo)
import Element
    exposing
        ( Element
        , above
        , alignLeft
        , alignRight
        , alpha
        , below
        , centerX
        , centerY
        , column
        , el
        , fill
        , height
        , none
        , padding
        , paddingEach
        , paddingXY
        , paragraph
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
import Utility.Duration as Duration
import Utility.Image as Image


singlePool : { model | device : Device, time : Posix, zoneInfo : Maybe ZoneInfo } -> { poolInfo | maturity : Maturity } -> Element msg
singlePool ({ device } as model) poolInfo =
    if device |> Device.isPhoneOrTablet then
        none

    else
        row
            [ width fill
            , height <| px 72
            , paddingXY 24 0
            , Border.solid
            , Border.widthEach
                { top = 0
                , right = 1
                , bottom = 1
                , left = 1
                }
            , Border.color Color.transparent100
            ]
            [ maturityInfo model poolInfo ]


maturityInfo : { model | time : Posix, zoneInfo : Maybe ZoneInfo } -> { poolInfo | maturity : Maturity } -> Element msg
maturityInfo { time, zoneInfo } { maturity } =
    row
        [ width shrink
        , height shrink
        , alignLeft
        , centerY
        , spacing 12
        ]
        [ Image.hourglassPrimary
            [ width <| px 16
            , centerY
            ]
        , column
            [ width shrink
            , height shrink
            , spacing 1
            ]
            [ el
                [ width shrink
                , height shrink
                , paddingXY 0 3
                , spacing 4
                , Font.size 14
                , Font.bold
                , Font.color Color.transparent500
                ]
                (maturity
                    |> Maturity.toPosix
                    |> Maybe.map (ZoneInfo.toString zoneInfo)
                    |> Maybe.withDefault (maturity |> Maturity.toString)
                    |> text
                )
            , el
                [ width shrink
                , height shrink
                , paddingXY 0 3
                , Font.size 12
                , Font.regular
                , Font.color Color.transparent300
                ]
                (maturity
                    |> Maturity.toPosix
                    |> Maybe.map (Duration.toString time)
                    |> Maybe.map text
                    |> Maybe.withDefault none
                )
            ]
        ]
