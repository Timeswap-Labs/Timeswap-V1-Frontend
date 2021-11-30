module Services.Swap.Main exposing (Msg, Service, init, subscriptions, update, view)

import Browser.Events
import Data.Address exposing (Address, toString)
import Data.Backdrop exposing (Backdrop)
import Data.Balances exposing (Balances, get, hasEnough)
import Data.Device as Device exposing (Device)
import Data.ERC20 as ERC20
import Data.Images exposing (Images)
import Data.Remote exposing (Remote(..))
import Data.Token exposing (Token(..))
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
import Modals.Lend.Error as LendError
import Services.Swap.GameToken as GameToken exposing (GameToken)
import Services.Swap.Query as Query exposing (Return)
import Services.Swap.Transaction as Transaction
import Time exposing (Posix)
import User exposing (User)
import Utility.Color as Color
import Utility.Exit as Exit
import Utility.Glass as Glass
import Utility.Image as Image
import Utility.Input as InputUtil
import Utility.Loading as Loading
import Utility.Millis as Millis
import Utility.TokenImage as TokenImage


type Service
    = Service
        { inToken : GameToken
        , outToken : GameToken
        , dropdown : Maybe Dropdown
        , options : List GameToken
        , input : String
        , notification : Maybe (Remote Http.Error Notification)
        , cache : Remote Error PriceCache
        }


type Dropdown
    = InToken
    | OutToken


type alias PriceCache =
    { value : Return
    , lastFetched : Posix
    }


type Notification
    = Successful
    | Failed


type alias Error =
    { httpError : Http.Error
    , timeToQuery : Posix
    }


init : ( Service, Cmd Msg )
init =
    ( { inToken = GameToken.Uniswap
      , outToken = GameToken.Balancer
      , dropdown = Nothing
      , options = [ GameToken.Uniswap, GameToken.Balancer, GameToken.Shiba, GameToken.Doge, GameToken.USDC ]
      , input = ""
      , notification = Nothing
      , cache = Loading
      }
        |> Service
    , fetchPrice { inToken = GameToken.Uniswap, outToken = GameToken.Balancer }
    )


type Msg
    = OpenDropdown Dropdown
    | CloseDropdown
    | SelectInToken GameToken
    | SelectOutToken GameToken
    | Input String
    | InputMax
    | Swap
    | ReceivePrice (Result Http.Error Return)
    | ReceiveTxnNotification (Result Http.Error String)
    | ReceiveTime Posix


update :
    { model | time : Posix, user : Remote User.Error User }
    -> Msg
    -> Service
    -> ( Service, Cmd Msg )
update { time, user } msg (Service service) =
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

        SelectInToken selectedToken ->
            if selectedToken == service.inToken then
                ( Service service, Cmd.none )

            else
                { service
                    | outToken =
                        if selectedToken == service.outToken then
                            service.inToken

                        else
                            service.outToken
                    , inToken = selectedToken
                    , cache = Loading
                    , notification = Nothing
                }
                    |> (\updatedService ->
                            ( updatedService |> Service
                            , fetchPrice updatedService
                            )
                       )

        SelectOutToken selectedToken ->
            if selectedToken == service.outToken then
                ( Service service, Cmd.none )

            else
                { service
                    | inToken =
                        if selectedToken == service.inToken then
                            service.outToken

                        else
                            service.inToken
                    , outToken = selectedToken
                    , cache = Loading
                    , notification = Nothing
                }
                    |> (\updatedService ->
                            ( updatedService |> Service
                            , fetchPrice updatedService
                            )
                       )

        ReceiveTime currentTime ->
            ( service |> Service
            , case service.cache of
                Success { lastFetched } ->
                    if (lastFetched |> Time.posixToMillis) + 60000 <= (currentTime |> Time.posixToMillis) then
                        fetchPrice service

                    else
                        Cmd.none

                Failure { timeToQuery } ->
                    if (timeToQuery |> Time.posixToMillis) <= (currentTime |> Time.posixToMillis) then
                        fetchPrice service

                    else
                        Cmd.none

                _ ->
                    Cmd.none
            )

        Input input ->
            ( (if
                input
                    |> Uint.isAmount (service.inToken |> GameToken.toToken)
               then
                { service
                    | input = input
                }

               else
                service
              )
                |> Service
            , Cmd.none
            )

        InputMax ->
            case user of
                Success { balances } ->
                    case balances of
                        Success successBalances ->
                            ( successBalances
                                |> get (service.inToken |> GameToken.toToken)
                                |> (\string ->
                                        { service | input = string } |> Service
                                   )
                            , Cmd.none
                            )

                        _ ->
                            ( Service service, Cmd.none )

                _ ->
                    ( Service service, Cmd.none )

        Swap ->
            ( { service
                | notification = Loading |> Just
              }
                |> Service
            , swapApi service user
            )

        ReceivePrice (Ok price) ->
            ( { service
                | cache =
                    if
                        (price.incomingTokenId == (service.inToken |> GameToken.toApiTokenId))
                            && (price.outgoingTokenId == (service.outToken |> GameToken.toApiTokenId))
                    then
                        Success { value = price, lastFetched = time }

                    else
                        service.cache
                , notification = Nothing
              }
                |> Service
            , Cmd.none
            )

        ReceivePrice (Err error) ->
            ( { service
                | cache = Failure { httpError = error, timeToQuery = time |> Millis.add 10000 }
                , notification = Just (Failure error)
              }
                |> Service
            , Cmd.none
            )

        ReceiveTxnNotification (Ok txn) ->
            ( { service
                | input = ""
                , notification = Just (Success Successful)
              }
                |> Service
            , Cmd.none
            )

        ReceiveTxnNotification (Err error) ->
            ( { service
                | notification = Just (Failure error)
              }
                |> Service
            , Cmd.none
            )


subscriptions : Service -> Sub Msg
subscriptions service =
    Sub.batch
        [ Time.every 10000 ReceiveTime

        -- , onClickOutsideDropdown service
        ]


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

                else
                    Decode.fail "Its the dropdown"
            )


fetchPrice :
    { service | inToken : GameToken, outToken : GameToken }
    -> Cmd Msg
fetchPrice service =
    Http.post
        { url = "https://api.timeswap.io/price"
        , body =
            { token1 = service.inToken
            , token2 = service.outToken
            }
                |> Query.encode
                |> Http.jsonBody
        , expect = Query.decoder |> Http.expectJson ReceivePrice
        }


swapApi :
    { service | inToken : GameToken, outToken : GameToken, input : String }
    -> Remote User.Error User
    -> Cmd Msg
swapApi service user =
    case user of
        Success userData ->
            Http.post
                { url = "https://api.timeswap.io/swap"
                , body =
                    { incomingTokenId = service.inToken
                    , outgoingTokenId = service.outToken
                    , incomingTokenQty =
                        service.input
                            |> String.toFloat
                            |> Maybe.map (\float -> float)
                            |> Maybe.withDefault 0
                    , userAddress = userData.address |> toString
                    }
                        |> Transaction.encode
                        |> Http.jsonBody
                , expect = Http.expectString ReceiveTxnNotification
                }

        _ ->
            Cmd.none


view :
    { model
        | device : Device
        , backdrop : Backdrop
        , tokens : Tokens
        , images : Images
        , tokenImages : TokenImages
    }
    -> { user | address : Address, balances : Remote () Balances }
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
        , swapButton model user service
        , notificationInfo service
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
        , inputAmount input
        ]


inTokenDropdownButton :
    { model
        | images : Images
        , tokenImages : TokenImages
    }
    -> GameToken
    -> Maybe Dropdown
    -> List GameToken
    -> Element Msg
inTokenDropdownButton ({ images, tokenImages } as model) gameToken dropdown options =
    Input.button
        ([ width <| px 150
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
                    |> GameToken.toToken
                    |> TokenImage.icon tokenImages [ width (px 20), height (px 20) ]
                , el [ Font.size 18 ] (gameToken |> GameToken.toERC20 |> ERC20.toSymbol |> text)
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
                    |> GameToken.toToken
                    |> TokenImage.icon tokenImages [ width (px 18), height (px 18) ]
                , el [ Font.size 16 ]
                    (gameToken
                        |> GameToken.toERC20
                        |> ERC20.toName
                        |> text
                    )
                ]
        }


inputAmount : String -> Element Msg
inputAmount input =
    row
        [ width fill
        , height <| px 44
        , paddingEach
            { top = 0
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
        [ Input.text
            [ width fill
            , height shrink
            , alignLeft
            , centerY
            , paddingEach
                { top = 0
                , right = 10
                , bottom = 0
                , left = 0
                }
            , Background.color Color.none
            , Border.color Color.none
            , Font.regular
            , Font.size 16
            , Color.transparent500 |> Font.color
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
        , maxButton
        ]


maxButton : Element Msg
maxButton =
    Input.button
        [ width shrink
        , height shrink
        , centerY
        , Font.regular
        , Font.size 16
        , Font.color Color.primary500
        ]
        { onPress = InputMax |> Just
        , label = text "MAX"
        }


outTokenDropdownButton :
    { model
        | images : Images
        , tokenImages : TokenImages
    }
    -> GameToken
    -> Maybe Dropdown
    -> List GameToken
    -> Element Msg
outTokenDropdownButton ({ images, tokenImages } as model) gameToken dropdown options =
    Input.button
        ([ width <| px 150
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
                    |> GameToken.toToken
                    |> TokenImage.icon tokenImages [ width (px 20), height (px 20) ]
                , el [ Font.size 18 ] (gameToken |> GameToken.toERC20 |> ERC20.toSymbol |> text)
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
                    |> GameToken.toToken
                    |> TokenImage.icon tokenImages [ width (px 18), height (px 18) ]
                , el [ Font.size 16 ]
                    (gameToken
                        |> GameToken.toERC20
                        |> ERC20.toName
                        |> text
                    )
                ]
        }


outputContent :
    { model
        | device : Device
        , images : Images
        , tokenImages : TokenImages
    }
    -> Service
    -> Element Msg
outputContent model (Service { dropdown, options, outToken, input, cache }) =
    column [ width fill ]
        [ row
            [ width fill
            , height shrink
            ]
            [ outTokenDropdownButton model outToken dropdown options
            , outputAmount input cache
            ]
        , priceDisclaimer
        ]


outputAmount :
    String
    -> Remote Error PriceCache
    -> Element Msg
outputAmount input cache =
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
        (case cache of
            Loading ->
                el
                    [ width <| px 50
                    , height shrink
                    ]
                    Loading.viewSmall

            Success cacheData ->
                text
                    ((cacheData.value.relativeTokenPrice
                        * (input
                            |> String.toFloat
                            |> Maybe.map (\inputFloat -> inputFloat)
                            |> Maybe.withDefault 0
                          )
                     )
                        |> String.fromFloat
                    )

            _ ->
                text ""
        )


priceDisclaimer : Element Msg
priceDisclaimer =
    row
        [ width fill
        , alignRight
        , Font.size 12
        , Font.color Color.transparent300
        , paddingEach
            { top = 6
            , bottom = 0
            , left = 0
            , right = 0
            }
        ]
        [ el [ alignRight ] (text "*The swapped token amount is approximate") ]


swapButton :
    { model | device : Device }
    -> { user | balances : Remote () Balances }
    -> Service
    -> Element Msg
swapButton { device } user (Service service) =
    if (service.input == "") || (service.input |> InputUtil.isZero) then
        disabledSwapBtn

    else
        case user.balances of
            Success successbalances ->
                if successbalances |> hasEnough (service.inToken |> GameToken.toToken) service.input then
                    if service.notification == Just Loading then
                        loadingSwapBtn

                    else
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
                             , Font.bold
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
                                    (if Device.isPhone device then
                                        []

                                     else
                                        [ el [ centerY ]
                                            (if Device.isTablet device then
                                                text "Swap"

                                             else
                                                text "Swap tokens"
                                            )
                                        ]
                                    )
                            }

                else
                    LendError.insufficientAsset

            _ ->
                none


disabledSwapBtn : Element msg
disabledSwapBtn =
    el
        [ width fill
        , height <| px 44
        , paddingEach
            { top = 0
            , right = 16
            , bottom = 0
            , left = 10
            }
        , centerX
        , centerY
        , Background.color Color.primary100
        , Border.rounded 4
        , Font.size 16
        , Font.color Color.light100
        ]
        (el
            [ width shrink
            , height shrink
            , centerX
            , centerY
            , Font.bold
            , Font.size 16
            , Font.color Color.transparent100
            ]
            (text "Swap tokens")
        )


loadingSwapBtn : Element msg
loadingSwapBtn =
    el
        [ width fill
        , height <| px 44
        , padding 5
        , centerX
        , centerY
        , Background.color Color.primary100
        , Border.rounded 4
        ]
        (el
            [ width shrink
            , height shrink
            , centerX
            , centerY
            , Font.color Color.transparent100
            ]
            Loading.viewSmall
        )


notificationInfo : Service -> Element Msg
notificationInfo (Service { notification }) =
    el
        [ width fill
        , height shrink
        , padding 0
        , centerY
        , Font.center
        , Font.size 14
        , Font.color
            (case notification of
                Just notif ->
                    case notif of
                        Failure error ->
                            Color.negative500

                        Success a ->
                            Color.positive500

                        _ ->
                            Color.light100

                _ ->
                    Color.light100
            )
        ]
        (text
            (case notification of
                Just notif ->
                    case notif of
                        Failure error ->
                            case error of
                                Http.BadStatus statusCode ->
                                    statusCode
                                        |> String.fromInt
                                        |> String.append "API Error "

                                Http.Timeout ->
                                    "Price fetch timeout"

                                Http.NetworkError ->
                                    "Price fetch : Network Error"

                                _ ->
                                    "Error occured"

                        Success a ->
                            "Swap successful"

                        _ ->
                            ""

                _ ->
                    ""
            )
        )
