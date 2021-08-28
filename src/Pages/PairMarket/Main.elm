module Pages.PairMarket.Main exposing (toUrl, view)

import Data.Device as Device exposing (Device)
import Data.Pair as Pair exposing (Pair)
import Data.Pools as Pools exposing (Pools)
import Data.Token as Token
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
import Utility.Color as Color
import Utility.TokenImage as TokenImage


toUrl : Pair -> String
toUrl pair =
    [ "#market"
    , pair |> Pair.toFragment
    ]
        |> String.join "?"


view : { model | device : Device, pools : Pools } -> Pair -> Element msg
view ({ device } as model) pair =
    column
        ([ height shrink
         , spacing 24
         , centerX
         ]
            ++ (if Device.isPhone device then
                    [ width <| px 335
                    , paddingXY 0 29
                    ]

                else if Device.isTablet device then
                    [ width <| px 552
                    , paddingXY 0 29
                    ]

                else
                    [ width <| px 1074
                    , paddingXY 0 38
                    ]
               )
        )
        [ title model pair ]


title : { model | device : Device, pools : Pools } -> Pair -> Element msg
title model pair =
    row
        [ width fill
        , height shrink
        , paddingXY 0 6
        , spacing 14
        ]
        [ icons pair
        , symbols model pair
        , size model pair
        ]


icons : Pair -> Element msg
icons pair =
    row
        [ width shrink
        , height shrink
        , spacing 4
        , alignLeft
        , centerY
        ]
        [ pair |> Pair.toAsset |> TokenImage.getIcon [ height <| px 24 ]
        , pair |> Pair.toCollateral |> TokenImage.getIcon [ height <| px 24 ]
        ]


symbols : { model | device : Device } -> Pair -> Element msg
symbols { device } pair =
    el
        [ width shrink
        , height shrink
        , paddingXY 0 3
        , alignLeft
        , centerY
        , Font.bold
        , if Device.isPhoneOrTablet device then
            Font.size 16

          else
            Font.size 18
        , Font.color Color.transparent500
        ]
        ([ pair |> Pair.toAsset |> Token.toSymbol -- short symbol
         , pair |> Pair.toCollateral |> Token.toSymbol -- short symbol
         ]
            |> String.join " - "
            |> text
        )


size : { model | device : Device, pools : Pools } -> Pair -> Element msg
size { device, pools } pair =
    el
        [ width shrink
        , height shrink
        , paddingXY 0 3
        , alignRight
        , centerY
        , Font.family [ Font.typeface "Supreme" ]
        , Font.regular
        , if Device.isPhoneOrTablet device then
            Font.size 16

          else
            Font.size 18
        , Font.color Color.transparent200
        ]
        (pools
            |> Pools.getSize pair
            |> String.fromInt
            |> (\string ->
                    if string == "1" then
                        "1 Pool"

                    else
                        string ++ " Pools"
               )
            |> text
        )
