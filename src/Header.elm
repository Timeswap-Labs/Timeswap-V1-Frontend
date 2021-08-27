module Header exposing (view)

import Data.Address as Address exposing (Address)
import Data.Chain exposing (Chain(..))
import Data.Device as Device exposing (Device)
import Element
    exposing
        ( Element
        , alignBottom
        , alignLeft
        , alignRight
        , centerX
        , centerY
        , clipY
        , column
        , el
        , fill
        , height
        , link
        , mouseDown
        , mouseOver
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
import Page exposing (Page)
import Pages.AllMarket.Main as AllMarket
import Pages.LendDashboard.Main as LendDashboard
import Route
import Service
import Utility.Color as Color
import Utility.Image as Image
import Utility.Typography as Typography


view :
    msg
    ->
        { model
            | device : Device
            , page : Page
            , user : Maybe { user | chain : Chain, address : Address }
        }
    -> Element msg
view msg ({ device } as model) =
    if Device.isPhoneOrTablet device then
        column
            [ Region.navigation
            , height <| px 164
            , width fill
            , clipY
            , Font.family Typography.supreme
            ]
            [ row
                [ height <| px 108
                , width fill
                , paddingEach
                    { top = 33
                    , right = 20
                    , bottom = 0
                    , left = 20
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
                ]
                [ logoContainer model
                , buttons model
                ]
            , row
                [ height <| px 56
                , width fill
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
                [ openAside msg
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
            , clipY
            , Font.family Typography.supreme
            ]
            [ logoContainer model
            , tabs model
            , buttons model
            ]


logoContainer : { model | device : Device } -> Element msg
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


logo : { model | device : Device } -> Element msg
logo { device } =
    if Device.isPhone device then
        Image.logoPure
            [ alignLeft
            , centerY
            , height <| px 34
            ]

    else if Device.isTablet device then
        Image.logo
            [ centerX
            , centerY
            , height <| px 34
            ]

    else
        Image.logo
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

         -- , Font.family [ Font.typeface "Supreme" ]
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


tabs : { model | device : Device, page : Page } -> Element msg
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
        (case page of
            Page.AllMarket _ ->
                [ selected model
                , unselected model (Page.LendDashboard LendDashboard.init)
                , unselected model Page.Liquidity
                ]

            Page.PairMarket _ ->
                [ selected model
                , unselected model (Page.LendDashboard LendDashboard.init)
                , unselected model Page.Liquidity
                ]

            Page.LendDashboard _ ->
                [ unselected model (Page.AllMarket AllMarket.init)
                , selected model
                , unselected model Page.Liquidity
                ]

            Page.BorrowDashboard _ ->
                [ unselected model (Page.AllMarket AllMarket.init)
                , selected model
                , unselected model Page.Liquidity
                ]

            Page.Liquidity ->
                [ unselected model (Page.AllMarket AllMarket.init)
                , unselected model (Page.LendDashboard LendDashboard.init)
                , selected model
                ]
        )


selected : { model | device : Device, page : Page } -> Element msg
selected { device, page } =
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
        (container page)


container : Page -> Element msg
container page =
    column
        [ width shrink
        , height fill
        , centerX
        ]
        [ pageName page
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


pageName : Page -> Element msg
pageName page =
    el
        [ width shrink
        , height shrink
        , centerY

        -- , Font.family [ Font.typeface "Supreme" ]
        , Font.regular
        , Font.size 16
        , Font.color Color.light100
        ]
        (Page.toName page |> text)


unselected : { model | device : Device } -> Page -> Element msg
unselected { device } page =
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
        (pageLink page)


pageLink : Page -> Element msg
pageLink page =
    link
        [ width shrink
        , height shrink
        , centerX
        , centerY

        -- , Font.family [ Font.typeface "Supreme" ]
        , Font.regular
        , Font.size 16
        , Font.color Color.transparent300
        ]
        { url = page |> Route.Page |> Route.toUrl
        , label = Page.toName page |> text
        }


buttons :
    { model
        | device : Device
        , user : Maybe { user | chain : Chain, address : Address }
    }
    -> Element msg
buttons ({ user } as model) =
    row
        [ alignRight
        , centerY
        , spacing 10
        ]
        ((user
            |> Maybe.map
                (\{ chain } ->
                    case chain of
                        Mainnet ->
                            []

                        Rinkeby ->
                            [ faucetButton model ]
                )
            |> Maybe.withDefault [ faucetButton model ]
         )
            ++ [ walletButton model
               , settingsButton model
               ]
        )


faucetButton :
    { model | device : Device }
    -> Element msg
faucetButton { device } =
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
         , Font.family [ Font.typeface "Supreme" ]
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
        { url = Service.Faucet |> Route.Service |> Route.toUrl
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
                        Image.faucet
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


walletButton :
    { model
        | device : Device
        , user : Maybe { user | chain : Chain, address : Address }
    }
    -> Element msg
walletButton { device, user } =
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

         --, Font.family [ Font.typeface "Supreme" ]
         , Font.regular
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
        { url =
            user
                |> Maybe.map
                    (\{ chain } ->
                        case chain of
                            Mainnet ->
                                Route.Exit |> Route.toUrl

                            Rinkeby ->
                                Service.Wallet |> Route.Service |> Route.toUrl
                    )
                |> Maybe.withDefault (Service.Connect |> Route.Service |> Route.toUrl)
        , label =
            row
                [ width shrink
                , height fill
                , spacing 6
                ]
                (user
                    |> Maybe.map
                        (\{ chain, address } ->
                            Image.metamask
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
                    |> Maybe.withDefault
                        (Image.wallet
                            [ width <| px 24
                            , centerY
                            ]
                            :: (if Device.isPhone device then
                                    []

                                else
                                    [ el [ centerY ]
                                        (if Device.isTablet device then
                                            text "Wallet"

                                         else
                                            text "Connect to a Wallet"
                                        )
                                    ]
                               )
                            ++ [ rinkebyLabel ]
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
        , Font.family [ Font.typeface "Supreme" ]
        , Font.bold
        , Font.size 12
        , Font.color Color.dark500
        , Font.letterSpacing 1.28
        ]
        (el
            [ centerX
            , centerY
            ]
            (text "RINKEBY")
        )


settingsButton :
    { model | device : Device }
    -> Element msg
settingsButton { device } =
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
        { url = Service.Faucet |> Route.Service |> Route.toUrl
        , label =
            Image.option
                [ width <| px 24
                , centerX
                , centerY
                ]
        }


openAside : msg -> Element msg
openAside msg =
    Input.button []
        { onPress = Just msg
        , label =
            Image.allPairs
                [ width <| px 20
                , centerY
                ]
        }
