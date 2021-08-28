module Services.Connect.Main exposing (toUrl, view)

import Data.Backdrop exposing (Backdrop)
import Data.Device as Device exposing (Device)
import Element
    exposing
        ( Element
        , alignBottom
        , alignRight
        , alignTop
        , centerX
        , centerY
        , column
        , el
        , fill
        , height
        , inFront
        , link
        , padding
        , paddingXY
        , px
        , row
        , shrink
        , spacing
        , text
        , width
        )
import Element.Font as Font
import Utility.Exit as Exit
import Utility.Glass as Glass
import Utility.Image as Image


toUrl : String
toUrl =
    "#connect"


view : { modal | device : Device, backdrop : Backdrop } -> Element msg
view { device, backdrop } =
    column
        ([ padding 40
         , spacing 32
         , centerX
         , centerY
         , inFront exit
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
        []


exit : Element msg
exit =
    el
        [ width shrink
        , height shrink
        , padding 20
        , alignRight
        , alignTop
        ]
        (link
            [ width shrink
            , height shrink
            ]
            { url = Exit.toUrl
            , label = Image.close [ width <| px 24 ]
            }
        )
