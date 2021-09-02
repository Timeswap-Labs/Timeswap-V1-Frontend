module Services.Faucet.Main exposing (view)

import Data.Backdrop exposing (Backdrop)
import Data.Device as Device exposing (Device)
import Data.Images exposing (Images)
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


view : { model | device : Device, backdrop : Backdrop, images : Images, user : Maybe user } -> Element msg
view { device, backdrop, images, user } =
    column
        ([ padding 40
         , spacing 32
         , centerX
         , centerY
         , Exit.button images |> inFront
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
