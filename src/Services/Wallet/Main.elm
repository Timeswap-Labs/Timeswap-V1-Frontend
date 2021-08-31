module Services.Wallet.Main exposing (view)

import Data.Address exposing (Address)
import Data.Backdrop exposing (Backdrop)
import Data.Balances exposing (Balances)
import Data.Device as Device exposing (Device)
import Data.Remote exposing (Remote(..))
import Element
    exposing
        ( Element
        , alignBottom
        , centerX
        , centerY
        , column
        , el
        , fill
        , height
        , inFront
        , padding
        , paddingXY
        , px
        , shrink
        , spacing
        , text
        , width
        )
import Element.Font as Font
import Utility.Color as Color
import Utility.Exit as Exit
import Utility.Glass as Glass


view :
    { msgs | disconnect : msg }
    -> { model | device : Device, backdrop : Backdrop }
    -> { user | address : Address, balances : Remote Balances }
    -> Element msg
view msgs { device, backdrop } user =
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
        (text "Wallet Balance")
