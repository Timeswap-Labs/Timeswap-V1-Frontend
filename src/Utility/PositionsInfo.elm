module Utility.PositionsInfo exposing
    ( duration
    , loading
    , noBorrowPosition
    , noLendPosition
    , noUser
    )

import Data.Device as Device exposing (Device)
import Data.Images exposing (Images)
import Data.Maturity as Maturity exposing (Maturity)
import Data.Status exposing (Status(..))
import Element
    exposing
        ( Element
        , alignLeft
        , alignRight
        , centerX
        , centerY
        , column
        , el
        , fill
        , height
        , link
        , mouseDown
        , mouseOver
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
import Time exposing (Posix)
import Utility.Color as Color
import Utility.Image as Image
import Utility.Loading as Loading
import Utility.Router as Router


loading : Element msg
loading =
    column
        [ width fill
        , height <| px 185
        , spacing 20
        , Border.width 1
        , Border.color Color.transparent100
        ]
        [ el
            [ paddingXY 0 3
            , centerX
            , centerY
            , Font.regular
            , Font.size 14
            , Font.color Color.transparent200
            ]
            (text "Loading")
        , el
            [ width shrink
            , height shrink
            , centerX
            , centerY
            ]
            Loading.view
        ]


noLendPosition : { model | images : Images } -> Element msg
noLendPosition model =
    column
        [ width fill
        , height <| px 185
        , spacing 20
        , Border.width 1
        , Border.color Color.transparent100
        ]
        [ el
            [ paddingXY 0 3
            , centerX
            , centerY
            , Font.regular
            , Font.size 14
            , Font.color Color.transparent200
            ]
            (text "No lend positions")
        , goToMarketButton model
        ]


noBorrowPosition : { model | images : Images } -> Element msg
noBorrowPosition model =
    column
        [ width fill
        , height <| px 185
        , spacing 20
        , Border.width 1
        , Border.color Color.transparent100
        ]
        [ el
            [ paddingXY 0 3
            , centerX
            , centerY
            , Font.regular
            , Font.size 14
            , Font.color Color.transparent200
            ]
            (text "No borrow positions")
        , goToMarketButton model
        ]


goToMarketButton : { model | images : Images } -> Element msg
goToMarketButton { images } =
    link
        [ width shrink
        , height <| px 44
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
        , mouseDown [ Background.color Color.primary300 ]
        , mouseOver [ Background.color Color.primary400 ]
        ]
        { url = Router.toAllMarket
        , label =
            row
                [ width shrink
                , height fill
                , spacing 6
                ]
                [ Image.plus images
                    [ height <| px 18
                    , centerX
                    , centerY
                    ]
                , el
                    [ centerX
                    , centerY
                    , Font.bold
                    ]
                    (text "Go to Market")
                ]
        }


noUser : { model | device : Device, images : Images } -> Element msg
noUser model =
    column
        [ width fill
        , height <| px 185
        , spacing 20
        , Border.width 1
        , Border.color Color.transparent100
        ]
        [ el
            [ paddingXY 0 3
            , centerX
            , centerY
            , Font.regular
            , Font.size 14
            , Font.color Color.transparent200
            ]
            (text "Not connected to a wallet")
        , connectButton model
        ]


connectButton : { model | device : Device, images : Images } -> Element msg
connectButton { device, images } =
    link
        [ width shrink
        , height <| px 44
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


duration :
    { model | device : Device, time : Posix, images : Images }
    -> Maturity
    -> Element msg
duration { device, time, images } maturity =
    row
        [ width shrink
        , height <| px 44
        , if device |> Device.isPhoneOrTablet then
            alignLeft

          else
            alignRight
        , centerY
        , spacing 12
        , Font.size 14
        , Font.color Color.light100
        ]
        (case maturity |> Maturity.toDuration time of
            Active string ->
                [ Image.hourglassPrimary images
                    [ height <| px 22
                    , centerY
                    ]
                , el
                    [ width <| px 220
                    , height shrink
                    , centerY
                    , Font.regular
                    ]
                    (text ("Matures in " ++ string))
                ]

            Matured string ->
                [ el
                    [ width <| px 44
                    , height <| px 44
                    , centerY
                    , Background.color Color.transparent100
                    , Border.rounded 999
                    ]
                    (Image.matured images
                        [ height <| px 25
                        , centerX
                        , centerY
                        ]
                    )
                , el
                    [ width <| px 220
                    , height shrink
                    , centerY
                    , Font.regular
                    ]
                    (text ("Matured " ++ string ++ " ago"))
                ]
        )
