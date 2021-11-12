port module Services.Swap.Main exposing (Msg, Service, init, subscriptions, update, view)

import Browser.Events
import Data.Address exposing (Address)
import Data.Backdrop exposing (Backdrop)
import Data.Device as Device exposing (Device)
import Data.Images exposing (Images)
import Data.Remote exposing (Remote(..))
import Data.TokenImages exposing (TokenImages)
import Data.Tokens exposing (Tokens)
import Element
    exposing
        ( Element
        , alignBottom
        , alignLeft
        , below
        , centerX
        , centerY
        , column
        , el
        , fill
        , height
        , htmlAttribute
        , inFront
        , mouseDown
        , mouseOver
        , moveDown
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
import Element.Input as Input
import Html.Attributes
import Json.Decode as Decode exposing (Decoder)
import Json.Encode exposing (Value)
import Services.Swap.Error as Error exposing (Error)
import Services.Swap.GameToken as GameToken exposing (GameToken)
import Services.Swap.Notification as Notification exposing (Notification)
import Services.Swap.Transaction as Transaction
import Utility.Color as Color
import Utility.Exit as Exit
import Utility.Glass as Glass
import Utility.Image as Image
import Utility.Input as Input


type Service
    = Service
        { inToken : GameToken
        , outToken : GameToken
        , dropdown : Maybe Dropdown
        , input : String
        , output : Remote Error String
        , notification : Maybe (Remote Error Notification)
        }


type Dropdown
    = InToken
    | OutToken


init : Service
init =
    { inToken = GameToken.Token1
    , outToken = GameToken.Token2
    , dropdown = Nothing
    , input = ""
    , output = "" |> Success
    , notification = Nothing
    }
        |> Service


type Msg
    = OpenDropdown Dropdown
    | CloseDropdown
    | Input String
    | Swap
    | NotificationMsg Value


update : Msg -> Service -> ( Service, Cmd Msg )
update msg (Service service) =
    case msg of
        OpenDropdown dropdown ->
            ( { service | dropdown = Just dropdown }
                |> Service
            , Cmd.none
            )

        CloseDropdown ->
            ( { service | dropdown = Nothing }
                |> Service
            , Cmd.none
            )

        Input input ->
            ( (if input |> Input.isFloat then
                { service | input = input }

               else
                service
              )
                |> Service
            , Cmd.none
            )

        Swap ->
            ( { service
                | input = ""
                , output = "" |> Success
                , notification = Loading |> Just
              }
                |> Service
            , { inToken = service.inToken
              , outToken = service.outToken
              , amount = service.input
              }
                |> Transaction.encode
                |> swap
            )

        NotificationMsg value ->
            ( value
                |> Decode.decodeValue
                    (Decode.oneOf
                        [ Notification.decoder
                            |> Decode.map Success
                        , Error.decoder
                            |> Decode.map Failure
                        ]
                    )
                |> (\result ->
                        case result of
                            Ok notification ->
                                { service
                                    | notification =
                                        notification
                                            |> Just
                                }
                                    |> Service

                            Err _ ->
                                Service service
                   )
            , Cmd.none
            )


port swap : Value -> Cmd msg


port notificationMsg : (Value -> msg) -> Sub msg


subscriptions : Service -> Sub Msg
subscriptions service =
    Sub.batch
        [ notificationMsg NotificationMsg ]


onClickOutsideDropdown : Service -> Sub Msg
onClickOutsideDropdown (Service { dropdown }) =
    case dropdown of
        Just _ ->
            Browser.Events.onClick
                (Decode.at [ "target", "id" ] decoderOutsideDropdown)

        Nothing ->
            Sub.none


decoderOutsideDropdown : Decoder Msg
decoderOutsideDropdown =
    Decode.string
        |> Decode.andThen
            (\string ->
                if string /= "swap-dropdown" then
                    Decode.succeed CloseDropdown
                        |> Debug.log "check"

                else
                    Decode.fail "Its the dropdown"
            )


view :
    { model
        | device : Device
        , backdrop : Backdrop
        , tokens : Tokens
        , images : Images
        , tokenImages : TokenImages
    }
    -> { user | address : Address }
    -> Service
    -> Element Msg
view ({ device, backdrop, images } as model) user service =
    column
        ([ padding 40
         , spacing 24
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
        , content model service
        , swapButton model
        ]


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
        (text "Swap Your Token")


content :
    { model
        | device : Device
        , backdrop : Backdrop
        , tokens : Tokens
        , images : Images
        , tokenImages : TokenImages
    }
    -> Service
    -> Element Msg
content model service =
    column
        [ width fill
        , height shrink
        , spacing 14
        ]
        [ inputContent model service
        , outputContent
        ]


inputContent :
    { model
        | device : Device
        , backdrop : Backdrop
        , images : Images
    }
    -> Service
    -> Element Msg
inputContent model service =
    row
        [ width fill
        , height shrink
        , spacing 12
        ]
        [ inputToken model service
        , inputAmount model service
        ]


inputToken :
    { model
        | device : Device
        , backdrop : Backdrop
        , images : Images
    }
    -> Service
    -> Element Msg
inputToken ({ device, images } as model) (Service { dropdown }) =
    Input.button
        ([ width <| px 100
         , padding 12
         , (case dropdown of
                Just InToken ->
                    tokenDropdown model

                _ ->
                    none
           )
            |> below
         ]
            ++ (if Device.isPhoneOrTablet device then
                    [ height <| px 35 ]

                else
                    [ height <| px 44 ]
               )
            ++ Glass.lightWhiteModal 4
        )
        { onPress =
            (case dropdown of
                Just InToken ->
                    CloseDropdown

                _ ->
                    OpenDropdown InToken
            )
                |> Just
        , label =
            row
                [ width shrink
                , height shrink
                , alignLeft
                , centerY
                ]
                [ Image.discloser images
                    [ width <| px 12
                    , centerY
                    ]
                ]
        }


tokenDropdown : { model | backdrop : Backdrop } -> Element msg
tokenDropdown { backdrop } =
    column
        ([ width fill
         , height <| px 100
         , padding 8
         , spacing 8
         , moveDown 8
         , Font.regular
         , Font.size 14
         , Font.color Color.transparent400
         , Html.Attributes.id "swap-dropdown"
            |> htmlAttribute
         ]
            ++ Glass.lightPrimaryModal backdrop 0
        )
        []


inputAmount : { model | device : Device } -> Service -> Element Msg
inputAmount { device } (Service { input }) =
    Input.text
        ([ width fill
         ]
            ++ (if Device.isPhoneOrTablet device then
                    [ height <| px 35 ]

                else
                    [ height <| px 44 ]
               )
        )
        { onChange = Input
        , text = input
        , placeholder = Nothing
        , label = Input.labelHidden "input amount"
        }


outputContent : Element msg
outputContent =
    row
        [ width fill
        , height shrink
        , spacing 12
        ]
        []


swapButton : { model | device : Device, images : Images } -> Element Msg
swapButton { device, images } =
    Input.button
        ([ width fill
         , paddingEach
            { top = 0
            , right = 16
            , bottom = 0
            , left = 10
            }
         , centerY
         , Background.color Color.primary500
         , Border.rounded 4
         , Font.size 16
         , Font.color Color.light100
         , mouseDown [ Background.color Color.primary400 ]
         , mouseOver [ Background.color Color.primary300 ]
         ]
            ++ (if Device.isPhoneOrTablet device then
                    [ height <| px 35 ]

                else
                    [ height <| px 44 ]
               )
        )
        { onPress = Just Swap
        , label =
            row
                [ width shrink
                , height fill
                , spacing 6
                , centerX
                ]
                (Image.wallet images
                    [ width <| px 24
                    , centerY
                    ]
                    :: (if Device.isPhone device then
                            []

                        else
                            [ el [ centerY, Font.regular ]
                                (if Device.isTablet device then
                                    text "Swap"

                                 else
                                    text "Swap tokens"
                                )
                            ]
                       )
                )
        }
