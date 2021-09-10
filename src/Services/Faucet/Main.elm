port module Services.Faucet.Main exposing (Msg, update, view)

import Data.Backdrop exposing (Backdrop)
import Data.Device as Device exposing (Device)
import Data.ERC20 as ERC20 exposing (ERC20)
import Data.Images exposing (Images)
import Data.Token as Token
import Data.TokenImages exposing (TokenImages)
import Data.Tokens as Tokens exposing (Tokens)
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
import Element.Keyed as Keyed
import Json.Encode exposing (Value)
import Sort.Set as Set
import Utility.Color as Color
import Utility.Exit as Exit
import Utility.Glass as Glass
import Utility.Image as Image
import Utility.Router as Router
import Utility.TokenImage as TokenImage


type Msg
    = FaucetMint Value


update : Msg -> Cmd Msg
update msg =
    case msg of
        FaucetMint value ->
            faucetMint value


port faucetMint : Value -> Cmd msg


view :
    { model
        | device : Device
        , backdrop : Backdrop
        , tokens : Tokens
        , images : Images
        , tokenImages : TokenImages
        , user : Maybe user
    }
    -> Element Msg
view ({ device, backdrop, images, user } as model) =
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
        ([ title
         , content model
         ]
            ++ (user
                    |> Maybe.map (\_ -> [])
                    |> Maybe.withDefault [ connectButton model ]
               )
        )


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


content : { model | tokens : Tokens, images : Images, tokenImages : TokenImages } -> Element Msg
content ({ tokens } as model) =
    Keyed.column
        [ width fill
        , height shrink
        , spacing 14
        ]
        (tokens
            |> Tokens.toERC20s
            |> Set.toList
            |> List.map
                (\erc20 ->
                    ( erc20 |> ERC20.toKey
                    , singleFaucet model erc20
                    )
                )
        )


singleFaucet : { model | images : Images, tokenImages : TokenImages } -> ERC20 -> Element Msg
singleFaucet { images, tokenImages } erc20 =
    Input.button
        ([ width fill
         , height shrink
         , mouseDown [ Background.color Color.primary300 ]
         , mouseOver [ Background.color Color.primary100 ]
         ]
            ++ Glass.lightWhiteModal 4
        )
        { onPress =
            erc20
                |> ERC20.encode
                |> FaucetMint
                |> Just
        , label =
            row
                [ width fill
                , height <| px 64
                , paddingEach
                    { top = 0
                    , right = 24
                    , bottom = 0
                    , left = 16
                    }
                ]
                [ row
                    [ width shrink
                    , height shrink
                    , spacing 12
                    , alignLeft
                    , centerY
                    ]
                    [ TokenImage.icon tokenImages
                        [ width <| px 32
                        , centerY
                        ]
                        (Token.ERC20 erc20)
                    , el
                        [ width shrink
                        , height shrink
                        , centerY
                        , Font.bold
                        , Font.size 18
                        , Font.color Color.transparent400
                        ]
                        (erc20
                            |> ERC20.toSymbol
                            |> text
                        )
                    ]
                , row
                    [ width shrink
                    , height shrink
                    , spacing 16
                    , alignRight
                    ]
                    [ el
                        [ width shrink
                        , height shrink
                        , centerY
                        , Font.regular
                        , Font.size 16
                        , Font.color Color.transparent400
                        ]
                        ([ "Mint"
                         , "1000"
                         , erc20 |> ERC20.toSymbol
                         ]
                            |> String.join " "
                            |> text
                        )
                    , Image.arrow images
                        [ width <| px 16
                        , centerY
                        ]
                    ]
                ]
        }


connectButton : { model | device : Device, images : Images } -> Element msg
connectButton { device, images } =
    link
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
        { url = Router.toConnect
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
                                    text "Wallet"

                                 else
                                    text "Connect to a Wallet"
                                )
                            ]
                       )
                    ++ [ rinkebyLabel ]
                )
        }


rinkebyLabel : Element msg
rinkebyLabel =
    el
        [ width shrink
        , height <| px 24
        , centerX
        , centerY
        , paddingXY 6 2
        , spacing 6
        , Background.color Color.warning400
        , Border.rounded 999
        , Font.size 12
        , Font.color Color.dark500
        , Font.letterSpacing 1.28
        ]
        (el
            [ centerX
            , centerY
            , Font.bold
            ]
            (text "RINKEBY")
        )
