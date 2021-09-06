module Services.NoMetamask.Main exposing (view)

import Data.Backdrop exposing (Backdrop)
import Data.Device as Device exposing (Device)
import Data.Images exposing (Images)
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
        , newTabLink
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
import Utility.Image as Image


view : { model | device : Device, backdrop : Backdrop, images : Images } -> Element msg
view { device, backdrop, images } =
    column
        ([ paddingXY 40 100
         , spacing 18
         , centerX
         , centerY
         , Exit.button images |> inFront
         ]
            ++ Glass.lightPrimaryModal backdrop 0
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
        [ Image.metamask images
            [ width <| px 72
            , centerX
            , centerY
            ]
        , title
        , content
        ]


title : Element msg
title =
    el
        [ width shrink
        , height shrink
        , paddingXY 0 4
        , centerX
        , centerY
        , Font.family [ Font.typeface "Supreme" ]
        , Font.bold
        , Font.size 24
        , Font.color Color.light100
        , Font.center
        ]
        (text "No Metamask Wallet Detected")


content : Element msg
content =
    column
        [ width fill
        , height shrink
        , centerX
        , centerY
        , Font.family [ Font.typeface "Supreme" ]
        , Font.bold
        , Font.size 18
        ]
        [ el
            [ paddingXY 0 3
            , centerX
            , Font.color Color.light300
            ]
            (text "Refresh the browser after downloading Metamask. ")
        , newTabLink
            [ paddingXY 0 3
            , centerX
            , Font.color Color.primary300
            ]
            { url = "https://metamask.io/"
            , label = text "Download here"
            }
        ]
