module Header exposing (view)

import Aside
import Data.Address as Address exposing (Address)
import Data.Backdrop exposing (Backdrop)
import Data.Chain exposing (Chain(..))
import Data.Device as Device exposing (Device)
import Data.Images exposing (Images)
import Data.Pools exposing (Pools)
import Data.Remote exposing (Remote(..))
import Data.Tab as Tab exposing (Tab)
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
        , link
        , mouseDown
        , mouseOver
        , moveDown
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
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html exposing (address)
import Html.Attributes
import List exposing (member)
import Page as Page exposing (Page)
import User
import Utility.Color as Color
import Utility.Glass as Glass
import Utility.Image as Image
import Utility.Router as Router
import Utility.Typography as Typography


view :
    { msgs | openDropdown : msg, closeDropdown : msg }
    ->
        { model
            | device : Device
            , backdrop : Backdrop
            , images : Images
            , pools : Pools
            , user : Remote userError { user | chain : Chain, address : Address }
            , page : Page
            , dropdown : Maybe ()
        }
    -> Element msg
view msgs ({ device } as model) =
    if Device.isPhoneOrTablet device then
        column
            [ Region.navigation
            , width fill
            , height <| px 131
            , Font.family Typography.supreme
            ]
            [ row
                [ width fill
                , height <| px 75
                , paddingXY 20 0
                , spacing 10
                , Border.solid
                , Border.widthEach
                    { top = 0
                    , right = 0
                    , bottom = 1
                    , left = 0
                    }
                , Border.color Color.transparent100
                ]
                [ logoContainer model
                , buttons msgs model
                ]
            , row
                [ width fill
                , height <| px 56
                , paddingEach
                    { top = 0
                    , right = 0
                    , bottom = 0
                    , left = 20
                    }
                , spacing 12
                , Border.solid
                , Border.widthEach
                    { top = 0
                    , right = 0
                    , bottom = 1
                    , left = 0
                    }
                , Border.color Color.transparent100
                ]
                [ openAside model
                , tabs model
                ]
            ]

    else
        row
            [ Region.navigation
            , width fill
            , height <| px 80
            , paddingEach
                { top = 0
                , right = 44
                , bottom = 0
                , left = 0
                }
            , spacing 10
            , Border.solid
            , Border.widthEach
                { top = 0
                , right = 0
                , bottom = 1
                , left = 0
                }
            , Border.color Color.transparent100
            , Font.family Typography.supreme
            ]
            [ logoContainer model
            , tabs model
            , buttons msgs model
            ]


logoContainer : { model | device : Device, images : Images } -> Element msg
logoContainer ({ device } as model) =
    if Device.isPhoneOrTablet device then
        row
            [ width shrink
            , height fill
            , spacing 5
            , alignLeft
            ]
            [ logo model
            , alpha model
            ]

    else
        row
            [ width <| px 278
            , height fill
            , spacing 6
            , alignLeft
            ]
            [ logo model
            , alpha model
            , line
            ]


logo : { model | device : Device, images : Images } -> Element msg
logo { device, images } =
    if Device.isPhone device then
        Image.logoPure images
            [ alignLeft
            , centerY
            , height <| px 34
            ]

    else if Device.isTablet device then
        Image.logo images
            [ centerX
            , centerY
            , height <| px 34
            ]

    else
        Image.logo images
            [ centerX
            , centerY
            , height <| px 40
            ]


alpha : { model | device : Device } -> Element msg
alpha { device } =
    el
        ([ width shrink
         , height shrink
         , padding 5
         , centerY
         , Background.color Color.primary500
         , Border.rounded 4
         , Font.bold
         , Font.color Color.light100
         , Font.letterSpacing 1.28
         ]
            ++ (if Device.isPhoneOrTablet device then
                    [ Font.size 10
                    , alignLeft
                    ]

                else
                    [ Font.size 12
                    , centerX
                    ]
               )
        )
        (text "ALPHA")


line : Element msg
line =
    el
        [ width shrink
        , height <| px 44
        , alignRight
        , centerY
        , Border.solid
        , Border.widthEach
            { top = 0
            , right = 1
            , bottom = 0
            , left = 0
            }
        , Border.color Color.transparent100
        ]
        none


tabs :
    { model
        | device : Device
        , pools : Pools
        , user : Remote userError { user | chain : Chain }
        , page : Page
    }
    -> Element msg
tabs ({ device, page } as model) =
    row
        ([ height fill
         , alignLeft
         ]
            ++ (if Device.isPhoneOrTablet device then
                    [ width fill ]

                else
                    [ width shrink ]
               )
        )
        (case page |> Page.toTab of
            Tab.Market ->
                [ selected model Tab.Market
                , unselected model Tab.Dashboard
                , unselected model Tab.Liquidity
                ]

            Tab.Dashboard ->
                [ unselected model Tab.Market
                , selected model Tab.Dashboard
                , unselected model Tab.Liquidity
                ]

            Tab.Liquidity ->
                [ unselected model Tab.Market
                , unselected model Tab.Dashboard
                , selected model Tab.Liquidity
                ]
        )


selected : { model | device : Device } -> Tab -> Element msg
selected { device } tab =
    el
        (height fill
            :: (if Device.isPhoneOrTablet device then
                    [ width fill
                    ]

                else
                    [ width <| px 108
                    ]
               )
        )
        (container tab)


container : Tab -> Element msg
container tab =
    column
        [ width shrink
        , height fill
        , centerX
        ]
        [ pageName tab
        , el
            [ width fill
            , height shrink
            , alignBottom
            , Border.solid
            , Border.widthEach
                { top = 0
                , right = 0
                , bottom = 2
                , left = 0
                }
            , Border.color Color.primary500
            ]
            none
        ]


pageName : Tab -> Element msg
pageName tab =
    el
        [ width shrink
        , height shrink
        , centerY
        , Font.regular
        , Font.size 16
        , Font.color Color.light100
        ]
        (tab |> Tab.toName |> text)


unselected : { model | device : Device } -> Tab -> Element msg
unselected { device } tab =
    el
        ([ height fill
         , paddingEach
            { top = 0
            , right = 0
            , bottom = 2
            , left = 0
            }
         ]
            ++ (if Device.isPhoneOrTablet device then
                    [ width fill
                    ]

                else
                    [ width <| px 108
                    ]
               )
        )
        (pageLink tab)


pageLink : Tab -> Element msg
pageLink tab =
    link
        [ width shrink
        , height shrink
        , centerX
        , centerY
        , Font.regular
        , Font.size 16
        , Font.color Color.transparent300
        ]
        { url = tab |> Tab.toUrl
        , label = tab |> Tab.toName |> text
        }


buttons :
    { msgs | openDropdown : msg, closeDropdown : msg }
    ->
        { model
            | device : Device
            , images : Images
            , backdrop : Backdrop
            , user : Remote userError { user | chain : Chain, address : Address }
            , dropdown : Maybe ()
        }
    -> Element msg
buttons msgs ({ user } as model) =
    row
        [ alignRight
        , centerY
        , spacing 10
        ]
        ((user
            |> User.toChain
            |> (\chain ->
                    case chain of
                        Mainnet ->
                            []

                        Rinkeby ->
                            [ case user of
                                Success { address } ->
                                    if
                                        Address.participantAddresses
                                            |> List.member address
                                    then
                                        swapButton model

                                    else
                                        none

                                _ ->
                                    none
                            , faucetButton model
                            ]
               )
         )
            ++ [ case user of
                    Success successUser ->
                        walletButton model successUser

                    _ ->
                        connectButton model
               , settingsButton model
               , linksButton msgs model
               ]
        )


swapButton :
    { model | device : Device, images : Images }
    -> Element msg
swapButton { device, images } =
    link
        ([ width shrink
         , paddingEach
            { top = 0
            , right =
                if Device.isPhone device then
                    8

                else
                    16
            , bottom = 0
            , left =
                if Device.isPhone device then
                    8

                else
                    16
            }
         , Background.color Color.primary500
         , Border.rounded 4
         , Font.bold
         , Font.size 16
         , Font.color Color.light100
         , mouseDown [ Background.color Color.primary400 ]
         , mouseOver [ Background.color Color.primary300 ]
         ]
            ++ (if Device.isPhoneOrTablet device then
                    [ height <| px 35
                    ]

                else
                    [ height <| px 44
                    ]
               )
        )
        { url = Router.toSwap
        , label =
            row
                [ width shrink
                , height fill
                , spacing 6
                ]
                [ el
                    [ centerY
                    ]
                    (if Device.isPhone device then
                        Image.hourglassPrimarySmall images
                            [ width <| px 19
                            , centerX
                            , centerY
                            ]

                     else if Device.isTablet device then
                        text "Swap"

                     else
                        text "Swap"
                    )
                ]
        }


faucetButton :
    { model | device : Device, images : Images }
    -> Element msg
faucetButton { device, images } =
    link
        ([ width shrink
         , paddingEach
            { top = 0
            , right =
                if Device.isPhone device then
                    8

                else
                    16
            , bottom = 0
            , left =
                if Device.isPhone device then
                    8

                else
                    16
            }
         , Background.color Color.primary500
         , Border.rounded 4
         , Font.bold
         , Font.size 16
         , Font.color Color.light100
         , mouseDown [ Background.color Color.primary400 ]
         , mouseOver [ Background.color Color.primary300 ]
         ]
            ++ (if Device.isPhoneOrTablet device then
                    [ height <| px 35
                    ]

                else
                    [ height <| px 44
                    ]
               )
        )
        { url = Router.toFaucet
        , label =
            row
                [ width shrink
                , height fill
                , spacing 6
                ]
                [ el
                    [ centerY
                    ]
                    (if Device.isPhone device then
                        Image.faucet images
                            [ width <| px 19
                            , centerX
                            , centerY
                            ]

                     else if Device.isTablet device then
                        text "Faucet"

                     else
                        text "Test Faucet"
                    )
                ]
        }


connectButton : { model | device : Device, images : Images } -> Element msg
connectButton { device, images } =
    link
        ([ width shrink
         , paddingEach
            { top = 0
            , right = 16
            , bottom = 0
            , left = 10
            }
         , centerX
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


walletButton :
    { model
        | device : Device
        , images : Images
    }
    -> { user | chain : Chain, address : Address }
    -> Element msg
walletButton { device, images } { chain, address } =
    link
        ([ width shrink
         , paddingEach
            { top = 0
            , right = 16
            , bottom = 0
            , left = 10
            }
         , Background.color Color.primary100
         , Border.rounded 4
         , Font.size 16
         , Font.color Color.light100
         , mouseDown [ Background.color Color.primary500 ]
         , mouseOver [ Background.color Color.primary400 ]
         ]
            ++ (if Device.isPhoneOrTablet device then
                    [ height <| px 35 ]

                else
                    [ height <| px 44 ]
               )
        )
        { url = Router.toWallet
        , label =
            row
                [ width shrink
                , height fill
                , spacing 6
                ]
                (Image.metamask images
                    [ width <| px 24
                    , centerY
                    ]
                    :: (if Device.isPhone device then
                            []

                        else
                            [ el [ centerY ] (text <| Address.toStringShort address) ]
                       )
                    ++ (case chain of
                            Mainnet ->
                                []

                            Rinkeby ->
                                [ rinkebyLabel ]
                       )
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


settingsButton :
    { model | device : Device, images : Images }
    -> Element msg
settingsButton { device, images } =
    link
        ([ Background.color Color.primary100
         , Border.rounded 4
         , mouseDown [ Background.color Color.primary400 ]
         , mouseOver [ Background.color Color.primary300 ]
         ]
            ++ (if Device.isPhoneOrTablet device then
                    [ width <| px 35
                    , height <| px 35
                    ]

                else
                    [ width <| px 44
                    , height <| px 44
                    ]
               )
        )
        { url = Router.toSettings
        , label =
            Image.option images
                [ (if device |> Device.isPhone then
                    19

                   else
                    24
                  )
                    |> px
                    |> width
                , centerX
                , centerY
                ]
        }


linksButton :
    { msgs | openDropdown : msg, closeDropdown : msg }
    -> { model | device : Device, backdrop : Backdrop, images : Images, dropdown : Maybe () }
    -> Element msg
linksButton msgs ({ device, images, dropdown } as model) =
    Input.button
        ([ Background.color Color.primary100
         , Border.rounded 4
         , mouseDown [ Background.color Color.primary400 ]
         , mouseOver [ Background.color Color.primary300 ]
         , dropdown
            |> Maybe.map (\_ -> linksDropdown model)
            |> Maybe.withDefault none
            |> below
         , htmlAttribute <| Html.Attributes.id "dropdown"
         ]
            ++ (if Device.isPhoneOrTablet device then
                    [ width <| px 35
                    , height <| px 35
                    ]

                else
                    [ width <| px 44
                    , height <| px 44
                    ]
               )
        )
        { onPress = dropdown |> Maybe.map (always msgs.closeDropdown) |> Maybe.withDefault msgs.openDropdown |> Just
        , label =
            Image.tripleDots images
                [ (if device |> Device.isPhone then
                    19

                   else
                    24
                  )
                    |> px
                    |> width
                , centerX
                , centerY
                ]
        }


linksDropdown : { model | backdrop : Backdrop, images : Images } -> Element msg
linksDropdown { backdrop, images } =
    column
        ([ width <| px 150
         , height shrink
         , padding 16
         , spacing 16
         , alignRight
         , moveDown 8
         , Font.regular
         , Font.size 14
         , Font.color Color.transparent400
         ]
            ++ Glass.lightPrimaryModal backdrop 0
        )
        [ newTabLink
            [ width fill
            , height shrink
            ]
            { url = "https://app.gitbook.com/o/-MIPYaWtcli1K3j7TRV7/s/-MIPZkDBEvwedF77USHq/"
            , label =
                row
                    [ width fill
                    , height shrink
                    , spacing 8
                    ]
                    [ Image.gitbook images
                        [ width <| px 20
                        , centerY
                        ]
                    , el
                        [ centerY ]
                        (text "Gitbook")
                    ]
            }
        , newTabLink
            [ width fill
            , height shrink
            ]
            { url = "https://github.com/Timeswap-Labs"
            , label =
                row
                    [ width fill
                    , height shrink
                    , spacing 8
                    ]
                    [ Image.github images
                        [ width <| px 20
                        , centerY
                        ]
                    , el
                        [ centerY ]
                        (text "Github")
                    ]
            }
        , newTabLink
            [ width fill
            , height shrink
            ]
            { url = "https://discord.com/channels/756135419359395930/824922502228738079"
            , label =
                row
                    [ width fill
                    , height shrink
                    , spacing 8
                    ]
                    [ Image.discord images
                        [ width <| px 20
                        , centerY
                        ]
                    , el
                        [ centerY ]
                        (text "Discord")
                    ]
            }
        , newTabLink
            [ width fill
            , height shrink
            ]
            { url = "https://twitter.com/TimeswapLabs"
            , label =
                row
                    [ width fill
                    , height shrink
                    , spacing 8
                    ]
                    [ Image.twitter images
                        [ width <| px 20
                        , centerY
                        ]
                    , el
                        [ centerY ]
                        (text "Twitter")
                    ]
            }
        , newTabLink
            [ width fill
            , height shrink
            ]
            { url = "http://t.me/timeswap"
            , label =
                row
                    [ width fill
                    , height shrink
                    , spacing 8
                    ]
                    [ Image.telegram images
                        [ width <| px 20
                        , centerY
                        ]
                    , el
                        [ centerY ]
                        (text "Telegram")
                    ]
            }
        , newTabLink
            [ width fill
            , height shrink
            ]
            { url = "https://timeswap.medium.com"
            , label =
                row
                    [ width fill
                    , height shrink
                    , spacing 8
                    ]
                    [ Image.medium images
                        [ width <| px 20
                        , centerY
                        ]
                    , el
                        [ centerY ]
                        (text "Medium")
                    ]
            }
        ]


openAside : { model | images : Images } -> Element msg
openAside { images } =
    link
        [ width shrink
        , height shrink
        , alignLeft
        ]
        { url = Aside.toUrl
        , label =
            Image.allPairs images
                [ width <| px 20
                , centerY
                ]
        }
