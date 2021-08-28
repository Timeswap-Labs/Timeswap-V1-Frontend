port module Services.Connect.Main exposing (Msg, toUrl, update, view)

import Data.Backdrop exposing (Backdrop)
import Data.Device as Device exposing (Device)
import Element
    exposing
        ( Element
        , alignBottom
        , alignLeft
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
        , mouseDown
        , mouseOver
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
import Element.Events as Events
import Element.Font as Font
import Json.Encode as Encode exposing (Value)
import Utility.Color as Color
import Utility.Exit as Exit
import Utility.Glass as Glass
import Utility.Image as Image


toUrl : String
toUrl =
    "#connect"


type Msg
    = ConnectMetamask


update : Msg -> Cmd Msg
update msg =
    case msg of
        ConnectMetamask ->
            Encode.object
                [ ( "method", Encode.string "eth_requestAccounts" ) ]
                |> connectMetamask


port connectMetamask : Value -> Cmd msg


view : { modal | device : Device, backdrop : Backdrop } -> Element Msg
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
        [ title
        , content
        ]


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


title : Element msg
title =
    column
        [ width shrink
        , height shrink
        , centerX
        , Font.family [ Font.typeface "Supreme" ]
        ]
        [ el
            [ paddingXY 0 4
            , centerX
            , Font.bold
            , Font.size 24
            , Font.color Color.light100
            ]
            (text "Connect to a Wallet")
        , el
            [ paddingXY 0 3
            , centerX
            , Font.regular
            , Font.size 18
            , Font.color Color.light200
            ]
            (text "To start using Timeswap")
        ]


content : Element Msg
content =
    column
        [ width fill
        , height shrink
        , spacing 20
        ]
        [ metamaskButton
        , termsOfService
        ]


metamaskButton : Element Msg
metamaskButton =
    link
        [ width fill
        , height <| px 64
        , paddingEach
            { top = 16
            , right = 20
            , bottom = 16
            , left = 16
            }
        , Border.solid
        , Border.width 1
        , Border.color Color.primary100
        , Border.rounded 4
        , mouseDown [ Background.color Color.primary300 ]
        , mouseOver [ Background.color Color.primary100 ]
        , Events.onClick ConnectMetamask
        ]
        { url = Exit.toUrl
        , label =
            row
                [ width fill
                , height fill
                , spacing 12
                , Font.family [ Font.typeface "Supreme" ]
                ]
                [ Image.metamask
                    [ width <| px 32
                    , alignLeft
                    , centerY
                    ]
                , el
                    [ alignLeft
                    , centerY
                    , Font.regular
                    , Font.size 18
                    , Font.color Color.transparent500
                    ]
                    (text "Metamask")
                , el
                    [ alignLeft
                    , paddingXY 8 6
                    , centerY
                    , Background.color Color.positive100
                    , Border.rounded 4
                    , Font.bold
                    , Font.size 12
                    , Font.color Color.positive500
                    , Font.letterSpacing 1.28
                    ]
                    (text "RECOMMENDED")
                , Image.arrow
                    [ width <| px 24
                    , alignRight
                    ]
                ]
        }


termsOfService : Element msg
termsOfService =
    row
        [ width shrink
        , height shrink
        , paddingXY 0 3
        , centerX
        , Font.family [ Font.typeface "Supreme" ]
        , Font.regular
        , Font.size 14
        ]
        [ el
            [ Font.color Color.transparent300 ]
            (text "By connecting, I accept Timeswap's ")
        , el
            [ Font.color Color.primary300 ]
            (text "Terms of Service")
        ]
