port module Services.Wallet.Main exposing (Msg, Service, init, update, view)

import Data.Address as Address exposing (Address)
import Data.Backdrop exposing (Backdrop)
import Data.Balances as Balances exposing (BalanceInfo, Balances)
import Data.Device as Device exposing (Device)
import Data.Images exposing (Images)
import Data.Remote exposing (Remote(..))
import Data.Token as Token
import Data.TokenImages exposing (TokenImages)
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
        , inFront
        , newTabLink
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
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Keyed as Keyed
import Services.Wallet.Tooltip as Tooltip exposing (Tooltip)
import Utility.Color as Color
import Utility.Exit as Exit
import Utility.Glass as Glass
import Utility.Image as Image
import Utility.Loading as Loading
import Utility.TokenImage as TokenImage
import Utility.Truncate as Truncate


type Service
    = Service (Maybe Tooltip)


init : Service
init =
    Service Nothing


type Msg
    = Disconnect
    | OnMouseEnter Tooltip
    | OnMouseLeave


update : Msg -> Service -> ( Service, Cmd Msg )
update msg service =
    case msg of
        Disconnect ->
            ( service
            , disconnect ()
            )

        OnMouseEnter tooltip ->
            ( Just tooltip |> Service
            , Cmd.none
            )

        OnMouseLeave ->
            ( Service Nothing
            , Cmd.none
            )


port disconnect : () -> Cmd msg


view :
    { model
        | device : Device
        , backdrop : Backdrop
        , images : Images
        , tokenImages : TokenImages
    }
    -> { user | address : Address, balances : Remote Balances }
    -> Service
    -> Element Msg
view ({ device, backdrop, images } as model) user (Service tooltip) =
    column
        ([ padding 40
         , spacing 20
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
        , userAddress model user
        , multipleBalances model user tooltip
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
        (text "Wallet Balance")


userAddress :
    { model | images : Images }
    -> { user | address : Address }
    -> Element Msg
userAddress { images } { address } =
    row
        [ width fill
        , height shrink
        , spacing 6
        ]
        [ Image.metamask images
            [ width <| px 24
            , alignLeft
            , centerY
            ]
        , el
            [ width shrink
            , height shrink
            , paddingXY 0 4
            , alignLeft
            , centerY
            , Font.regular
            , Font.size 16
            , Font.color Color.primary500
            ]
            (address
                |> Address.toStringShort
                |> text
            )
        , newTabLink
            [ width shrink
            , height shrink
            , alignLeft
            , centerY
            ]
            { url =
                [ "https://rinkeby.etherscan.io/address/"
                , address |> Address.toString
                ]
                    |> String.concat
            , label = Image.link images [ width <| px 14 ]
            }
        , Input.button
            [ width shrink
            , height shrink
            , alignRight
            , centerY
            ]
            { onPress = Just Disconnect
            , label =
                el
                    [ width shrink
                    , height shrink
                    , paddingXY 0 4
                    , Font.regular
                    , Font.size 16
                    , Font.color Color.primary500
                    ]
                    (text "Disconnect Wallet")
            }
        ]


multipleBalances :
    { model | tokenImages : TokenImages }
    -> { user | balances : Remote Balances }
    -> Maybe Tooltip
    -> Element Msg
multipleBalances model { balances } tooltip =
    column
        [ width fill
        , height shrink
        ]
        [ row
            ([ width fill
             , height <| px 60
             , paddingXY 16 0
             , Font.regular
             , Font.size 14
             , Font.color Color.transparent300
             ]
                ++ Glass.lightWhiteModal 4
            )
            [ el
                [ alignLeft
                , centerY
                , Font.bold
                ]
                (text "ASSET")
            , el
                [ alignRight
                , centerY
                , Font.bold
                ]
                (text "BALANCE")
            ]
        , case balances of
            Loading ->
                loading

            Failure ->
                none

            Success successBalances ->
                Keyed.column
                    [ width fill
                    , height shrink
                    ]
                    (successBalances
                        |> Balances.toList
                        |> List.map
                            (\({ token } as balanceInfo) ->
                                ( token |> Token.toKey
                                , singleBalance model balanceInfo tooltip
                                )
                            )
                    )
        ]


loading : Element msg
loading =
    el
        [ width fill
        , height <| px 72
        , Border.width 1
        , Border.solid
        , Border.color Color.transparent100
        ]
        (el
            [ centerX
            , centerY
            ]
            Loading.view
        )


singleBalance :
    { model | tokenImages : TokenImages }
    -> BalanceInfo
    -> Maybe Tooltip
    -> Element Msg
singleBalance { tokenImages } ({ token } as balanceInfo) tooltip =
    row
        [ width fill
        , height <| px 72
        , paddingXY 16 0
        , spacing 12
        , Border.width 1
        , Border.solid
        , Border.color Color.transparent100
        ]
        [ TokenImage.icon tokenImages
            [ width <| px 32
            , alignLeft
            , centerY
            ]
            token
        , el
            [ width shrink
            , height shrink
            , paddingXY 0 3
            , alignLeft
            , centerY
            , Font.bold
            , Font.size 14
            , Font.color Color.transparent500
            ]
            (token
                |> Token.toName
                |> text
            )
        , tokenBalance balanceInfo tooltip
        ]


tokenBalance :
    BalanceInfo
    -> Maybe Tooltip
    -> Element Msg
tokenBalance { token, balance } tooltip =
    balance
        |> Truncate.amount
        |> (\{ full, truncated } ->
                truncated
                    |> Maybe.map
                        (\short ->
                            el
                                [ width shrink
                                , height shrink
                                , alignRight
                                , centerY
                                ]
                                (token
                                    |> Token.toSymbol
                                    |> (\symbol ->
                                            row
                                                [ paddingEach
                                                    { top = 3
                                                    , right = 0
                                                    , bottom = 2
                                                    , left = 0
                                                    }
                                                , spacing 4
                                                , Font.bold
                                                , Border.widthEach
                                                    { top = 0
                                                    , right = 0
                                                    , bottom = 1
                                                    , left = 0
                                                    }
                                                , Border.dashed
                                                , Border.color Color.transparent200
                                                , Font.bold
                                                , Font.size 14
                                                , Events.onMouseEnter (OnMouseEnter (Tooltip.Balance token))
                                                , Events.onMouseLeave OnMouseLeave
                                                , (case tooltip of
                                                    Just (Tooltip.Balance chosenToken) ->
                                                        if chosenToken == token then
                                                            [ full
                                                            , symbol
                                                            ]
                                                                |> String.join " "
                                                                |> Tooltip.balance

                                                        else
                                                            none

                                                    _ ->
                                                        none
                                                  )
                                                    |> below
                                                ]
                                                [ el
                                                    [ Font.color Color.transparent500 ]
                                                    (text short)
                                                , el
                                                    [ Font.color Color.transparent300 ]
                                                    (text symbol)
                                                ]
                                       )
                                )
                        )
                    |> Maybe.withDefault
                        (row
                            [ width shrink
                            , height shrink
                            , alignRight
                            , centerY
                            , paddingXY 0 3
                            , spacing 4
                            , Font.bold
                            , Font.size 14
                            ]
                            [ el
                                [ Font.color Color.transparent500 ]
                                (text full)
                            , el
                                [ Font.color Color.transparent300 ]
                                (token
                                    |> Token.toSymbol
                                    |> text
                                )
                            ]
                        )
           )
