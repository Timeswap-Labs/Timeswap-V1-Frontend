port module Services.Swap.Main exposing (Msg, Service, init, subscriptions, update, view)

import Browser.Events
import Data.Address exposing (Address)
import Data.Backdrop exposing (Backdrop)
import Data.Balances exposing (Balances)
import Data.Device as Device exposing (Device)
import Data.ERC20 as ERC20
import Data.Images exposing (Images)
import Data.Remote exposing (Remote(..))
import Data.Token as Token exposing (Token(..))
import Data.TokenImages exposing (TokenImages)
import Data.Tokens exposing (Tokens)
import Data.Uint as Uint
import Element
    exposing
        ( Element
        , alignBottom
        , alignLeft
        , alignRight
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
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode exposing (Value)
import Services.Swap.Error as Error exposing (Error)
import Services.Swap.GameToken as GameToken exposing (GameToken)
import Services.Swap.Notification as Notification exposing (Notification)
import Services.Swap.Query as Query exposing (Query, Return)
import Services.Swap.Transaction as Transaction
import Utility.Color as Color
import Utility.Exit as Exit
import Utility.Glass as Glass
import Utility.Image as Image
import Utility.Input as Input
import Utility.Loading as Loading
import Utility.TokenImage as TokenImage


type Service
    = Service
        { inToken : GameToken
        , outToken : GameToken
        , dropdown : Maybe Dropdown
        , options : List GameToken
        , input : String
        , output : Remote Error String
        , notification : Maybe (Remote Error Notification)
        }


type Dropdown
    = InToken
    | OutToken


init : Service
init =
    { inToken = GameToken.Shiba
    , outToken = GameToken.Doge
    , dropdown = Nothing
    , options = [ GameToken.Shiba, GameToken.Doge, GameToken.Token3 ]
    , input = ""
    , output = "" |> Success
    , notification = Nothing
    }
        |> Service


type Msg
    = OpenDropdown Dropdown
    | CloseDropdown
    | SelectInToken GameToken
    | SelectOutToken GameToken
    | Input String
    | Swap
    | NotificationMsg Value
    | ReceiveQuery (Result Http.Error Return)


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

        SelectInToken gameToken ->
            ( { service
                | outToken =
                    if gameToken == service.outToken then
                        service.inToken

                    else
                        service.outToken
                , inToken = gameToken
              }
                |> Service
            , Cmd.none
            )

        SelectOutToken gameToken ->
            ( { service
                | inToken =
                    if gameToken == service.inToken then
                        service.outToken

                    else
                        service.inToken
                , outToken = gameToken
              }
                |> Service
            , Cmd.none
            )

        Input input ->
            ( (if
                input
                    |> Uint.isAmount (service.inToken |> GameToken.toERC20 |> Token.ERC20)
               then
                { service
                    | input = input
                    , output =
                        if input |> Input.isZero then
                            Success ""

                        else
                            Loading
                }

               else
                service
              )
                |> Service
            , if input |> Input.isZero then
                Cmd.none

              else
                input
                    |> Uint.fromAmount (service.inToken |> GameToken.toERC20 |> Token.ERC20)
                    |> Maybe.map
                        (\amount ->
                            Http.post
                                { url = "https://api.timeswap.io/swap"
                                , body =
                                    { token1 = service.inToken
                                    , token2 = service.outToken
                                    , amount = amount
                                    }
                                        |> Query.encode
                                        |> Http.jsonBody
                                , expect = Query.decoder |> Http.expectJson ReceiveQuery
                                }
                        )
                    |> Maybe.withDefault Cmd.none
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

        ReceiveQuery (Ok { token1, token2, amount, result }) ->
            ( if
                token1
                    == service.inToken
                    && token2
                    == service.outToken
                    && Just amount
                    == (service.input |> Uint.fromAmount (service.inToken |> GameToken.toERC20 |> Token.ERC20))
              then
                { service | output = result |> Uint.toAmount (service.outToken |> GameToken.toERC20 |> Token.ERC20) |> Success } |> Service

              else
                Service service
            , Cmd.none
            )

        ReceiveQuery (Err error) ->
            ( Service service, Cmd.none )


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
        , spacing 20
        ]
        [ inputContent model service
        , outputContent model service
        ]


inputContent :
    { model
        | device : Device
        , images : Images
        , tokenImages : TokenImages
    }
    -> Service
    -> Element Msg
inputContent model (Service { dropdown, options, inToken, input }) =
    row
        [ width fill
        , height shrink
        ]
        [ inTokenDropdownButton model inToken dropdown options
        , inputAmount model input
        ]


inTokenDropdownButton :
    { model
        | device : Device
        , images : Images
        , tokenImages : TokenImages
    }
    -> GameToken
    -> Maybe Dropdown
    -> List GameToken
    -> Element Msg
inTokenDropdownButton ({ device, images, tokenImages } as model) gameToken dropdown options =
    Input.button
        ([ width <| px 130
         , height <| px 44
         , padding 12
         , (if dropdown == Just InToken then
                inTokenDropdown model options

            else
                none
           )
            |> below
         ]
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
                [ width fill
                , height shrink
                , alignLeft
                , centerY
                , spacing 8
                , Font.color Color.transparent400
                ]
                [ gameToken
                    |> GameToken.toERC20
                    |> Token.ERC20
                    |> TokenImage.icon tokenImages [ width (px 20), height (px 20) ]
                , gameToken |> GameToken.toERC20 |> ERC20.toSymbol |> text
                , Image.discloser images
                    [ width <| px 12
                    , centerY
                    , alignRight
                    ]
                ]
        }


inTokenDropdown : { model | tokenImages : TokenImages } -> List GameToken -> Element Msg
inTokenDropdown { tokenImages } options =
    column
        [ width fill
        , height shrink
        , padding 0
        , spacing 0
        , moveDown 4
        , Font.regular
        , Font.size 14
        , Font.color Color.transparent400
        , Background.color Color.darkModal
        , Border.widthEach
            { bottom = 1
            , left = 1
            , right = 1
            , top = 0
            }
        , Color.transparent100
            |> Border.color
        , Border.shadow
            { offset = ( 3, 3 )
            , size = 2
            , blur = 7
            , color = Color.primary100
            }
        , Html.Attributes.id "swap-dropdown"
            |> htmlAttribute
        ]
        (options |> List.map (inTokenOptions tokenImages))


inTokenOptions : TokenImages -> GameToken -> Element Msg
inTokenOptions tokenImages gameToken =
    Input.button
        [ width fill
        , height <| px 36
        , paddingEach { top = 6, right = 6, bottom = 6, left = 8 }
        , mouseDown [ Background.color Color.primary400 ]
        , mouseOver [ Background.color Color.primary300 ]
        ]
        { onPress =
            SelectInToken gameToken
                |> Just
        , label =
            row
                [ width shrink
                , height shrink
                , alignLeft
                , centerY
                , spacing 8
                ]
                [ gameToken
                    |> GameToken.toERC20
                    |> Token.ERC20
                    |> TokenImage.icon tokenImages [ width (px 18), height (px 18) ]
                , el [ Font.size 16 ]
                    (gameToken
                        |> GameToken.toERC20
                        |> ERC20.toName
                        |> text
                    )
                ]
        }


outTokenDropdownButton :
    { model
        | device : Device
        , images : Images
        , tokenImages : TokenImages
    }
    -> GameToken
    -> Maybe Dropdown
    -> List GameToken
    -> Element Msg
outTokenDropdownButton ({ device, images, tokenImages } as model) gameToken dropdown options =
    Input.button
        ([ width <| px 130
         , height <| px 44
         , padding 12
         , (if dropdown == Just OutToken then
                outTokenDropdown model options

            else
                none
           )
            |> below
         ]
            ++ Glass.lightWhiteModal 4
        )
        { onPress =
            (case dropdown of
                Just OutToken ->
                    CloseDropdown

                _ ->
                    OpenDropdown OutToken
            )
                |> Just
        , label =
            row
                [ width fill
                , height shrink
                , alignLeft
                , centerY
                , spacing 8
                , Font.color Color.transparent400
                ]
                [ gameToken
                    |> GameToken.toERC20
                    |> Token.ERC20
                    |> TokenImage.icon tokenImages [ width (px 20), height (px 20) ]
                , gameToken |> GameToken.toERC20 |> ERC20.toSymbol |> text
                , Image.discloser images
                    [ width <| px 12
                    , centerY
                    , alignRight
                    ]
                ]
        }


outTokenDropdown : { model | tokenImages : TokenImages } -> List GameToken -> Element Msg
outTokenDropdown { tokenImages } options =
    column
        [ width fill
        , height shrink
        , padding 0
        , spacing 0
        , moveDown 4
        , Font.regular
        , Font.size 14
        , Font.color Color.transparent400
        , Background.color Color.darkModal
        , Border.widthEach
            { bottom = 1
            , left = 1
            , right = 1
            , top = 0
            }
        , Color.transparent100
            |> Border.color
        , Border.shadow
            { offset = ( 3, 3 )
            , size = 2
            , blur = 7
            , color = Color.primary100
            }
        , Html.Attributes.id "swap-dropdown"
            |> htmlAttribute
        ]
        (options |> List.map (outTokenOptions tokenImages))


outTokenOptions : TokenImages -> GameToken -> Element Msg
outTokenOptions tokenImages gameToken =
    Input.button
        [ width fill
        , height <| px 36
        , paddingEach { top = 6, right = 6, bottom = 6, left = 8 }
        , mouseDown [ Background.color Color.primary400 ]
        , mouseOver [ Background.color Color.primary300 ]
        ]
        { onPress =
            SelectOutToken gameToken
                |> Just
        , label =
            row
                [ width shrink
                , height shrink
                , alignLeft
                , centerY
                , spacing 8
                ]
                [ gameToken
                    |> GameToken.toERC20
                    |> Token.ERC20
                    |> TokenImage.icon tokenImages [ width (px 18), height (px 18) ]
                , el [ Font.size 16 ]
                    (gameToken
                        |> GameToken.toERC20
                        |> ERC20.toName
                        |> text
                    )
                ]
        }


inputAmount : { model | device : Device } -> String -> Element Msg
inputAmount { device } input =
    Input.text
        [ width fill
        , height <| px 44
        , alignLeft
        , centerY
        , Background.color Color.none
        , Font.regular
        , Font.size 16
        , Color.transparent500 |> Font.color
        , paddingEach
            { top = 12
            , right = 12
            , bottom = 0
            , left = 12
            }
        , Border.widthEach
            { top = 1
            , right = 1
            , bottom = 1
            , left = 0
            }
        , Border.solid
        , Color.transparent100
            |> Border.color
        , Border.roundEach
            { topLeft = 0
            , topRight = 4
            , bottomRight = 4
            , bottomLeft = 0
            }
        ]
        { onChange = Input
        , text = input
        , placeholder =
            Input.placeholder
                [ Font.color Color.transparent100 ]
                (text "Enter input token amount")
                |> Just
        , label = Input.labelHidden "input amount"
        }


outputContent :
    { model
        | device : Device
        , images : Images
        , tokenImages : TokenImages
    }
    -> Service
    -> Element Msg
outputContent model (Service { dropdown, options, outToken, output }) =
    row
        [ width fill
        , height shrink
        ]
        [ outTokenDropdownButton model outToken dropdown options
        , outputAmount model output
        ]


outputAmount : { model | device : Device } -> Remote Error String -> Element Msg
outputAmount { device } output =
    el
        [ width fill
        , height <| px 44
        , alignLeft
        , centerY
        , Background.color Color.none
        , Font.regular
        , Font.size 16
        , Color.transparent500 |> Font.color
        , paddingEach
            { top = 12
            , right = 12
            , bottom = 0
            , left = 12
            }
        , Border.widthEach
            { top = 1
            , right = 1
            , bottom = 1
            , left = 0
            }
        , Border.solid
        , Border.color Color.transparent100
        , Border.roundEach
            { topLeft = 0
            , topRight = 4
            , bottomRight = 4
            , bottomLeft = 0
            }
        ]
        (case output of
            Loading ->
                el
                    [ width <| px 50
                    , height shrink
                    ]
                    Loading.viewSmall

            Success outputValue ->
                text outputValue

            _ ->
                text ""
        )


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
