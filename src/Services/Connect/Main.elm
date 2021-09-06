port module Services.Connect.Main exposing (Msg, update, view)

import Data.Backdrop exposing (Backdrop)
import Data.Device as Device exposing (Device)
import Data.Images exposing (Images)
import Element
    exposing
        ( Element
        , alignBottom
        , alignLeft
        , alignRight
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
        , newTabLink
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
import Utility.Router as Router


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


view :
    { modal
        | device : Device
        , backdrop : Backdrop
        , images : Images
    }
    -> Element Msg
view ({ device, backdrop, images } as model) =
    column
        ([ padding 40
         , spacing 32
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
        [ title
        , content model
        ]


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


content : { model | images : Images } -> Element Msg
content model =
    column
        [ width fill
        , height shrink
        , spacing 20
        ]
        [ metamaskButton model
        , termsOfService
        ]


metamaskButton : { model | images : Images } -> Element Msg
metamaskButton { images } =
    link
        ([ width fill
         , height <| px 64
         , paddingEach
            { top = 16
            , right = 20
            , bottom = 16
            , left = 16
            }
         , mouseDown [ Background.color Color.primary300 ]
         , mouseOver [ Background.color Color.primary100 ]
         , Events.onClick ConnectMetamask
         ]
            ++ Glass.lightWhiteModal 4
        )
        { url = Router.exit
        , label =
            row
                [ width fill
                , height fill
                , spacing 12
                , Font.family [ Font.typeface "Supreme" ]
                ]
                [ Image.metamask images
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
                , Image.arrow images
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
        , newTabLink
            [ Font.color Color.primary300 ]
            { url = "https://timeswap.io/terms/"
            , label = text "Terms of Service"
            }
        ]
