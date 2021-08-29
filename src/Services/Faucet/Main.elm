module Services.Faucet.Main exposing (toUrl, view)

import Data.Backdrop exposing (Backdrop)
import Data.Device as Device exposing (Device)
import Element
    exposing
        ( Element
        , alignBottom
        , alignRight
        , alignTop
        , alpha
        , centerX
        , centerY
        , column
        , el
        , fill
        , height
        , inFront
        , none
        , padding
        , paddingEach
        , paddingXY
        , px
        , row
        , shrink
        , spacing
        , text
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Utility.Color as Color
import Utility.Exit as Exit
import Utility.Glass as Glass


toUrl : String
toUrl =
    "#faucet"


view : { model | device : Device, backdrop : Backdrop, user : Maybe user } -> Element msg
view { device, backdrop, user } =
    column
        ([ padding 40
         , spacing 32
         , centerX
         , centerY
         , inFront Exit.button
         ]
            ++ Glass.darkPrimaryModal backdrop 0
            ++ (if Device.isPhone device then
                    [ width fill
                    , height shrink
                    , alignBottom
                    ]

                else
                    [ width <| px 533
                    , height shrink
                    ]
               )
        )
        [ title ]


title : Element msg
title =
    el
        [ width shrink
        , height shrink
        , paddingXY 0 4
        , centerX
        , Font.family [ Font.typeface "Supreme" ]
        , Font.bold
        , Font.size 24
        , Font.color Color.light100
        , Font.center
        ]
        (text "Test Token Faucets")
