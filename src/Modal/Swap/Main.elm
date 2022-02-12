port module Modal.Swap.Main exposing (Modal, Msg, init, subscriptions, update, view)

import Animator exposing (Timeline)
import Blockchain.Main as Blockchain exposing (Blockchain)
import Blockchain.User.Main as User exposing (User)
import Browser.Events
import Data.Address exposing (toString)
import Data.Backdrop exposing (Backdrop)
import Data.Chains as Chains exposing (Chains)
import Data.Device as Device exposing (Device)
import Data.Images exposing (Images)
import Data.Remote as Remote exposing (Remote(..))
import Data.Theme as Theme exposing (Theme)
import Data.Token as Token exposing (Token(..))
import Data.Uint as Uint
import Dict
import Element
    exposing
        ( Element
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
        , minimum
        , mouseDown
        , mouseOver
        , moveDown
        , none
        , padding
        , paddingEach
        , paddingXY
        , pointer
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
import Html.Events
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode exposing (Value)
import Modal.Outside as Outside
import Modal.Swap.Query as Query exposing (Return)
import Modal.Swap.Transaction as Transaction
import Page.Transaction.Button as Button
import Time exposing (Posix)
import Utility.Color as Color
import Utility.Glass as Glass
import Utility.IconButton as IconButton
import Utility.Image as Image
import Utility.Input as InputUtil
import Utility.Loading as Loading
import Utility.ThemeColor as ThemeColor


type Modal
    = Modal
        { inToken : Maybe Token
        , outToken : Maybe Token
        , dropdown : Maybe Dropdown
        , options : List Token
        , input : String
        , notification : Maybe (Remote ErrorDetailed String)
        , cache : Remote Error PriceCache
        }


type Dropdown
    = InToken
    | OutToken


type alias PriceCache =
    { value : Return
    , lastFetched : Posix
    }


type alias Error =
    { httpError : Http.Error
    , timeToQuery : Posix
    }


type ErrorDetailed
    = BadUrl String
    | Timeout
    | NetworkError
    | BadStatus Http.Metadata String
    | BadBody String


init :
    { model
        | chains : Chains
        , blockchain : Blockchain
    }
    -> ( Modal, Cmd Msg )
init { chains, blockchain } =
    let
        tokenList =
            chains |> Chains.toTokenList (blockchain |> Blockchain.toChain)

        inToken =
            tokenList |> List.head

        outToken =
            tokenList |> List.drop 1 |> List.head
    in
    ( { inToken = inToken
      , outToken = outToken
      , dropdown = Nothing
      , options = tokenList
      , input = "0"
      , notification = Nothing
      , cache = Remote.loading
      }
        |> Modal
    , fetchPrice { inToken = inToken, outToken = outToken }
    )


type Msg
    = OpenDropdown Dropdown
    | CloseDropdown
    | SelectInToken Token
    | SelectOutToken Token
    | Input String
    | InputMax
    | SignMsg
    | SwapSignatureMsg Value
    | ReceivePrice (Result Http.Error Return)
    | ReceiveTxnNotification (Result ErrorDetailed String)
    | ReceiveTime Posix
    | Exit


update :
    { model | time : Posix, user : User }
    -> Msg
    -> Modal
    -> ( Maybe Modal, Cmd Msg )
update { time, user } msg (Modal modal) =
    case msg of
        SelectInToken selectedToken ->
            case ( modal.inToken, modal.outToken ) of
                ( Just inToken, Just outToken ) ->
                    if selectedToken == inToken then
                        ( { modal | dropdown = Nothing } |> Modal |> Just, Cmd.none )

                    else
                        { modal
                            | outToken =
                                if selectedToken == outToken then
                                    modal.inToken

                                else
                                    modal.outToken
                            , inToken = selectedToken |> Just
                            , cache = Remote.loading
                            , notification = Nothing
                            , dropdown = Nothing
                        }
                            |> Debug.log "select token"
                            |> (\updatedService ->
                                    ( updatedService |> Modal |> Just
                                    , fetchPrice updatedService
                                    )
                               )

                _ ->
                    ( modal |> Modal |> Just, Cmd.none )

        SelectOutToken selectedToken ->
            case ( modal.inToken, modal.outToken ) of
                ( Just inToken, Just outToken ) ->
                    if selectedToken == outToken then
                        ( { modal | dropdown = Nothing } |> Modal |> Just, Cmd.none )

                    else
                        { modal
                            | inToken =
                                if selectedToken == inToken then
                                    modal.outToken

                                else
                                    modal.inToken
                            , outToken = selectedToken |> Just
                            , cache = Remote.loading
                            , notification = Nothing
                            , dropdown = Nothing
                        }
                            |> (\updatedService ->
                                    ( updatedService |> Modal |> Just
                                    , fetchPrice updatedService
                                    )
                               )

                _ ->
                    ( modal |> Modal |> Just, Cmd.none )

        OpenDropdown dropdown ->
            ( { modal | dropdown = Just dropdown }
                |> Modal
                |> Just
            , Cmd.none
            )

        CloseDropdown ->
            ( { modal | dropdown = Nothing }
                |> Debug.log "close dropdown"
                |> Modal
                |> Just
            , Cmd.none
            )

        ReceiveTime currentTime ->
            ( modal |> Modal |> Just
            , case modal.cache of
                Success { lastFetched } ->
                    if (lastFetched |> Time.posixToMillis) + 60000 <= (currentTime |> Time.posixToMillis) then
                        fetchPrice modal

                    else
                        Cmd.none

                Failure { timeToQuery } ->
                    if (timeToQuery |> Time.posixToMillis) <= (currentTime |> Time.posixToMillis) then
                        fetchPrice modal

                    else
                        Cmd.none

                _ ->
                    Cmd.none
            )

        Input input ->
            case modal.inToken of
                Just inToken ->
                    ( (if
                        input
                            |> Uint.isAmount inToken
                       then
                        { modal
                            | input = input
                        }

                       else
                        modal
                      )
                        |> Modal
                        |> Just
                    , Cmd.none
                    )

                _ ->
                    ( modal |> Modal |> Just, Cmd.none )

        InputMax ->
            case modal.inToken of
                Just inToken ->
                    case user |> User.getBalance inToken of
                        Just (Success balance) ->
                            ( { modal | input = balance |> Uint.toAmount inToken } |> Modal |> Just
                            , Cmd.none
                            )

                        _ ->
                            ( modal |> Modal |> Just, Cmd.none )

                _ ->
                    ( modal |> Modal |> Just, Cmd.none )

        SignMsg ->
            ( modal |> Modal |> Just, signSwapTxn () )

        SwapSignatureMsg signature ->
            case Decode.decodeValue Decode.string signature of
                Ok signString ->
                    ( { modal
                        | notification = Remote.loading |> Just
                      }
                        |> Modal
                        |> Just
                    , swapApi modal user signString
                    )

                _ ->
                    ( { modal
                        | notification = Nothing
                      }
                        |> Modal
                        |> Just
                    , Cmd.none
                    )

        ReceivePrice (Ok price) ->
            ( { modal
                | cache =
                    case ( modal.inToken, modal.outToken ) of
                        ( Just inToken, Just outToken ) ->
                            if
                                (price.incomingTokenId == (inToken |> Token.toApiTokenId))
                                    && (price.outgoingTokenId == (outToken |> Token.toApiTokenId))
                            then
                                Success { value = price, lastFetched = time }

                            else
                                modal.cache

                        ( _, _ ) ->
                            modal.cache
                , notification = Nothing
              }
                |> Modal
                |> Just
            , Cmd.none
            )

        ReceivePrice (Err error) ->
            ( { modal
                | cache =
                    Failure
                        { httpError = error
                        , timeToQuery = ((time |> Time.posixToMillis) + 10000) |> Time.millisToPosix
                        }
                , notification = Just (Failure (error |> convertHttpError))
              }
                |> Modal
                |> Just
            , Cmd.none
            )

        ReceiveTxnNotification (Ok txn) ->
            ( { modal
                | input = ""
                , notification = Just (Success "Swap successful")
              }
                |> Modal
                |> Just
            , Cmd.none
            )

        ReceiveTxnNotification (Err error) ->
            ( { modal
                | notification = Just (Failure error)
              }
                |> Modal
                |> Just
            , Cmd.none
            )

        Exit ->
            ( Nothing
            , Cmd.none
            )


port signSwapTxn : () -> Cmd msg


port swapSignatureMsg : (Value -> msg) -> Sub msg


subscriptions : Modal -> Sub Msg
subscriptions modal =
    Sub.batch
        [ Time.every 10000 ReceiveTime
        , swapSignatureMsg SwapSignatureMsg
        , onClick modal
        ]


onClick : Modal -> Sub Msg
onClick (Modal { dropdown }) =
    case dropdown of
        Just _ ->
            Browser.Events.onClick (Decode.succeed CloseDropdown)

        Nothing ->
            Sub.none


fetchPrice :
    { modal | inToken : Maybe Token, outToken : Maybe Token }
    -> Cmd Msg
fetchPrice modal =
    case ( modal.inToken, modal.outToken ) of
        ( Just inToken, Just outToken ) ->
            Http.post
                { url = "https://ts-gamification-api.herokuapp.com/price"
                , body =
                    { token1 = inToken
                    , token2 = outToken
                    }
                        |> Query.encode
                        |> Http.jsonBody
                , expect = Query.decoder |> Http.expectJson ReceivePrice
                }

        ( _, _ ) ->
            Cmd.none


swapApi :
    { modal | inToken : Maybe Token, outToken : Maybe Token, input : String }
    -> User
    -> String
    -> Cmd Msg
swapApi modal user signature =
    case ( modal.inToken, modal.outToken ) of
        ( Just inToken, Just outToken ) ->
            Http.post
                { url = "https://ts-gamification-api.herokuapp.com/swap"
                , body =
                    { incomingTokenId = inToken
                    , outgoingTokenId = outToken
                    , incomingTokenQty = modal.input
                    , userAddress = user |> User.toAddress |> toString
                    , signature = signature
                    }
                        |> Transaction.encode
                        |> Http.jsonBody
                , expect = expectStringDetailed ReceiveTxnNotification
                }

        _ ->
            Cmd.none


view :
    { model
        | device : Device
        , backdrop : Backdrop
        , images : Images
        , blockchain : Blockchain
        , theme : Theme
    }
    -> Modal
    -> Element Msg
view ({ device, backdrop, blockchain, theme } as model) modal =
    Outside.view model
        { onClick = Exit
        , modal =
            column
                ([ if device |> Device.isPhoneOrTablet then
                    width <| px 375

                   else
                    width <| px 500
                 , height shrink
                 , padding 24
                 , centerX
                 , centerY
                 , spacing 16
                 , Border.rounded 8
                 , theme |> ThemeColor.border |> Border.color
                 , Border.width 1
                 ]
                    ++ Glass.background backdrop theme
                )
                [ row
                    [ width fill
                    , height shrink
                    , spacing 16
                    , paddingEach
                        { top = 4
                        , right = 0
                        , bottom = 12
                        , left = 0
                        }
                    ]
                    [ title theme
                    , IconButton.exit model Exit
                    ]
                , content model modal
                , swapButton (blockchain |> Blockchain.toUser) theme modal
                , notificationInfo modal
                ]
        }


title : Theme -> Element msg
title theme =
    el
        [ width shrink
        , height shrink
        , paddingXY 0 4
        , Font.family [ Font.typeface "Supreme" ]
        , Font.bold
        , Font.size 16
        , theme |> ThemeColor.text |> Font.color
        , Font.center
        ]
        (text "Swap Your Token")


content :
    { model
        | device : Device
        , images : Images
        , theme : Theme
    }
    -> Modal
    -> Element Msg
content model modal =
    column
        [ width fill
        , height shrink
        , spacing 20
        ]
        [ inputContent model modal
        , outputContent model modal
        ]


inputContent :
    { model
        | device : Device
        , images : Images
        , theme : Theme
    }
    -> Modal
    -> Element Msg
inputContent model (Modal { dropdown, options, inToken, input }) =
    row
        [ width fill
        , height shrink
        ]
        [ inTokenDropdownButton model inToken dropdown options
        , inputAmount input model.theme
        ]


inTokenDropdownButton :
    { model
        | images : Images
        , theme : Theme
    }
    -> Maybe Token
    -> Maybe Dropdown
    -> List Token
    -> Element Msg
inTokenDropdownButton ({ images, theme } as model) inToken dropdown options =
    el
        [ width <| px 150
        , height <| px 44
        , (if dropdown == Just InToken then
            inTokenDropdown model options

           else
            none
          )
            |> below
        , Input.button
            [ width fill
            , height fill
            , padding 12
            , theme |> ThemeColor.border |> Border.color
            , Border.rounded 4
            , Border.width 1
            , theme |> ThemeColor.btnBackground |> Background.color

            -- , el
            --     [ width fill
            --     , height fill
            --     , Html.Attributes.id "swap-dropdown"
            --         |> htmlAttribute
            --     ]
            --     none
            --     |> inFront
            ]
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
                    , theme |> ThemeColor.text |> Font.color
                    ]
                    (case inToken of
                        Just token ->
                            [ images
                                |> Image.viewToken
                                    [ width (px 20)
                                    , height (px 20)
                                    ]
                                    token
                            , el [ Font.size 18 ] (token |> Token.toSymbol |> text)
                            , images
                                |> (case theme of
                                        Theme.Dark ->
                                            Image.discloser

                                        Theme.Light ->
                                            Image.arrowDownDark
                                   )
                                    [ width <| px 12
                                    , centerY
                                    , alignRight
                                    ]
                            ]

                        _ ->
                            [ el [ Font.size 14 ] ("Select token" |> text) ]
                    )
            }
            |> inFront
        ]
        none


inTokenDropdown : { model | images : Images, theme : Theme } -> List Token -> Element Msg
inTokenDropdown { images, theme } options =
    column
        [ width fill
        , height shrink
        , moveDown 4
        , Font.regular
        , Font.size 14
        , theme |> ThemeColor.text |> Font.color
        , theme |> ThemeColor.dropdownBG |> Background.color
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
        ]
        (options |> List.map (inTokenOptions images))


inTokenOptions : Images -> Token -> Element Msg
inTokenOptions images gameToken =
    el
        [ width fill
        , height <| px 36
        , paddingEach { top = 6, right = 6, bottom = 6, left = 8 }
        , mouseDown [ Background.color Color.primary400 ]
        , mouseOver [ Background.color Color.primary300 ]
        , Html.Events.custom "click" (inDecoderMsg gameToken) |> htmlAttribute
        , pointer
        ]
        (row
            [ width shrink
            , height shrink
            , alignLeft
            , centerY
            , spacing 8
            ]
            [ images
                |> Image.viewToken [ width (px 18), height (px 18) ] gameToken
            , el [ Font.size 16 ]
                (gameToken
                    |> Token.toName
                    |> text
                )
            ]
        )


inDecoderMsg : Token -> Decoder { message : Msg, stopPropagation : Bool, preventDefault : Bool }
inDecoderMsg gameToken =
    { message = SelectInToken gameToken
    , stopPropagation = True
    , preventDefault = False
    }
        |> Decode.succeed


inputAmount : String -> Theme -> Element Msg
inputAmount input theme =
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
        , theme |> ThemeColor.textboxBorder |> Border.color
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
            , theme |> ThemeColor.text |> Font.color
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


outputContent :
    { model
        | device : Device
        , images : Images
        , theme : Theme
    }
    -> Modal
    -> Element Msg
outputContent model (Modal { dropdown, options, outToken, input, cache }) =
    column [ width fill ]
        [ row
            [ width fill
            , height shrink
            ]
            [ outTokenDropdownButton model outToken dropdown options
            , outputAmount input model.theme cache
            ]
        , priceDisclaimer model.theme
        ]


outTokenDropdownButton :
    { model
        | images : Images
        , theme : Theme
    }
    -> Maybe Token
    -> Maybe Dropdown
    -> List Token
    -> Element Msg
outTokenDropdownButton ({ images, theme } as model) outToken dropdown options =
    Input.button
        [ width <| px 150
        , height <| px 44
        , padding 12
        , theme |> ThemeColor.border |> Border.color
        , Border.rounded 4
        , Border.width 1
        , theme |> ThemeColor.btnBackground |> Background.color
        , (if dropdown == Just OutToken then
            outTokenDropdown model options

           else
            none
          )
            |> below
        , el
            [ width fill
            , height fill
            , Html.Attributes.id "swap-dropdown"
                |> htmlAttribute
            ]
            none
            |> inFront
        ]
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
                , theme |> ThemeColor.text |> Font.color
                ]
                (case outToken of
                    Just token ->
                        [ images
                            |> Image.viewToken
                                [ width (px 20)
                                , height (px 20)
                                ]
                                token
                        , el [ Font.size 18 ] (token |> Token.toSymbol |> text)
                        , images
                            |> (case theme of
                                    Theme.Dark ->
                                        Image.discloser

                                    Theme.Light ->
                                        Image.arrowDownDark
                               )
                                [ width <| px 12
                                , centerY
                                , alignRight
                                ]
                        ]

                    _ ->
                        [ el [ Font.size 14 ] ("Select token" |> text) ]
                )
        }


outTokenDropdown : { model | images : Images, theme : Theme } -> List Token -> Element Msg
outTokenDropdown { images, theme } options =
    column
        [ width fill
        , height shrink
        , padding 0
        , spacing 0
        , moveDown 4
        , Font.regular
        , Font.size 14
        , theme |> ThemeColor.text |> Font.color
        , theme |> ThemeColor.positionBG |> Background.color
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
        ]
        (options |> List.map (outTokenOptions images))


outTokenOptions : Images -> Token -> Element Msg
outTokenOptions images outToken =
    el
        [ width fill
        , height <| px 36
        , paddingEach { top = 6, right = 6, bottom = 6, left = 8 }
        , mouseDown [ Background.color Color.primary400 ]
        , mouseOver [ Background.color Color.primary300 ]
        , Html.Events.custom "click" (outDecoderMsg outToken) |> htmlAttribute
        , pointer
        ]
        (row
            [ width shrink
            , height shrink
            , alignLeft
            , centerY
            , spacing 8
            ]
            [ images
                |> Image.viewToken [ width (px 18), height (px 18) ] outToken
            , el [ Font.size 16 ]
                (outToken
                    |> Token.toName
                    |> text
                )
            ]
        )


outDecoderMsg : Token -> Decoder { message : Msg, stopPropagation : Bool, preventDefault : Bool }
outDecoderMsg gameToken =
    { message = SelectOutToken gameToken
    , stopPropagation = True
    , preventDefault = False
    }
        |> Decode.succeed


outputAmount :
    String
    -> Theme
    -> Remote Error PriceCache
    -> Element Msg
outputAmount input theme cache =
    el
        [ width fill
        , height <| px 44
        , alignLeft
        , centerY
        , Background.color Color.none
        , Font.regular
        , Font.size 16
        , theme |> ThemeColor.text |> Font.color
        , paddingEach
            { top = 12
            , right = 12
            , bottom = 0
            , left = 12
            }
        ]
        (case cache of
            Loading timeline ->
                el
                    [ width <| px 50
                    , height shrink
                    ]
                    (Loading.view timeline theme)

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


priceDisclaimer : Theme -> Element Msg
priceDisclaimer theme =
    row
        [ width fill
        , alignRight
        , Font.size 12
        , theme |> ThemeColor.textLight |> Font.color
        , paddingEach
            { top = 6
            , bottom = 0
            , left = 0
            , right = 0
            }
        ]
        [ el [ alignRight ] (text "*The swapped token amount is approximate") ]


swapButton :
    Maybe User
    -> Theme
    -> Modal
    -> Element Msg
swapButton maybeUser theme (Modal modal) =
    if (modal.input == "") || (modal.input |> InputUtil.isZero) then
        disabledSwapBtn

    else
        case ( maybeUser, modal.inToken ) of
            ( Just user, Just inToken ) ->
                case ( modal.input |> Uint.fromAmount inToken, modal.notification ) of
                    ( Just uintAmount, Just (Success _) ) ->
                        if user |> User.hasEnoughBalance inToken uintAmount then
                            Input.button
                                [ width fill
                                , height <| px 44
                                , paddingEach
                                    { top = 0
                                    , right = 16
                                    , bottom = 0
                                    , left = 10
                                    }
                                , centerY
                                , theme |> ThemeColor.primaryBtn |> Background.color
                                , Border.rounded 4
                                , Font.size 16
                                , Font.color Color.light100
                                , Font.bold
                                , mouseDown [ theme |> ThemeColor.btnPressBG |> Background.color ]
                                , mouseOver [ theme |> ThemeColor.btnHoverBG |> Background.color ]
                                ]
                                { onPress = Just SignMsg
                                , label =
                                    row
                                        [ width shrink
                                        , height fill
                                        , spacing 6
                                        , centerX
                                        ]
                                        [ el [ centerY ]
                                            (text "Swap tokens")
                                        ]
                                }

                        else
                            Button.notEnoughBalance

                    ( Just _, Just (Loading timeline) ) ->
                        loadingSwapBtn timeline theme

                    ( _, _ ) ->
                        disabledSwapBtn

            ( _, _ ) ->
                disabledSwapBtn


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


loadingSwapBtn :
    Timeline ()
    -> Theme
    -> Element msg
loadingSwapBtn timeline theme =
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
            (Loading.view timeline theme)
        )


notificationInfo : Modal -> Element Msg
notificationInfo (Modal { notification }) =
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
                                BadStatus metadata body ->
                                    body

                                Timeout ->
                                    "API Timeout"

                                NetworkError ->
                                    "Network Error"

                                _ ->
                                    "Error occured"

                        Success successMsg ->
                            successMsg

                        _ ->
                            ""

                _ ->
                    ""
            )
        )


expectStringDetailed : (Result ErrorDetailed String -> msg) -> Http.Expect msg
expectStringDetailed msg =
    Http.expectStringResponse msg convertResponseString


convertResponseString : Http.Response String -> Result ErrorDetailed String
convertResponseString httpResponse =
    case httpResponse of
        Http.BadUrl_ url ->
            Err (BadUrl url)

        Http.Timeout_ ->
            Err Timeout

        Http.NetworkError_ ->
            Err NetworkError

        Http.BadStatus_ metadata body ->
            Err (BadStatus metadata body)

        Http.GoodStatus_ metadata body ->
            Ok body


convertHttpError : Http.Error -> ErrorDetailed
convertHttpError httpError =
    case httpError of
        Http.BadUrl url ->
            BadUrl url

        Http.Timeout ->
            Timeout

        Http.NetworkError ->
            NetworkError

        Http.BadStatus int ->
            BadStatus { statusCode = int, url = "", statusText = "", headers = Dict.empty } "Error in fetching the price!"

        Http.BadBody decodeError ->
            BadBody decodeError
