module Pages.PairMarket.ListPools exposing (view)

import Data.Device as Device exposing (Device)
import Data.Maturity as Maturity exposing (Maturity)
import Data.Pair as Pair exposing (Pair)
import Data.Pools exposing (PoolInfo)
import Data.Remote exposing (Remote(..))
import Data.Token as Token
import Data.ZoneInfo exposing (ZoneInfo)
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
        , none
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
import Element.Keyed as Keyed
import Modals.Borrow.Main as Borrow
import Modals.Lend.Main as Lend
import Time exposing (Posix)
import Utility.Color as Color
import Utility.Duration as Duration
import Utility.Image as Image
import Utility.Loading as Loading


view :
    { model | device : Device, time : Posix, zoneInfo : Maybe ZoneInfo }
    -> Pair
    -> List PoolInfo
    -> Element msg
view ({ time } as model) pair list =
    column
        [ width fill
        , height shrink
        ]
        [ row
            [ width fill
            , height shrink
            , paddingXY 24 20
            , spacing 20
            , Background.color Color.list
            , Border.solid
            , Border.width 1
            , Border.color Color.transparent100
            , Font.bold
            , Font.size 12
            , Font.color Color.transparent300
            ]
            [ el [ alignLeft ] (text "MATURITY TIME")
            , el [ width <| px 170, alignRight, Font.center ] (text "LIQUIDITY")
            , el [ width <| px 130, alignRight, Font.center ] (text "ESTIMATED APR")
            , el [ width <| px 170, alignRight, Font.center ] (text "COLLATERAL FACTOR")
            , el [ width <| px 177, alignRight ] none
            ]
        , Keyed.column
            [ width fill
            , height shrink
            ]
            (list
                |> List.filter (\{ maturity } -> maturity |> Maturity.isActive time)
                |> List.map
                    (\({ maturity } as poolInfo) ->
                        ( maturity |> Maturity.toKey
                        , singlePool model pair poolInfo
                        )
                    )
            )
        ]


singlePool :
    { model | device : Device, time : Posix, zoneInfo : Maybe ZoneInfo }
    -> Pair
    -> PoolInfo
    -> Element msg
singlePool ({ device } as model) pair poolInfo =
    if device |> Device.isPhoneOrTablet then
        none

    else
        row
            [ width fill
            , height <| px 72
            , paddingXY 24 0
            , spacing 20
            , Border.solid
            , Border.widthEach
                { top = 0
                , right = 1
                , bottom = 1
                , left = 1
                }
            , Border.color Color.transparent100
            ]
            [ maturityInfo model poolInfo
            , liquidities model pair poolInfo
            , estimatedAPR model poolInfo
            , collateralFactor model pair poolInfo
            , buttons model pair poolInfo
            ]


maturityInfo : { model | time : Posix, zoneInfo : Maybe ZoneInfo } -> { poolInfo | maturity : Maturity } -> Element msg
maturityInfo { time, zoneInfo } { maturity } =
    row
        [ width shrink
        , height shrink
        , alignLeft
        , centerY
        , spacing 12
        ]
        [ Image.hourglassPrimary
            [ width <| px 16
            , centerY
            ]
        , column
            [ width shrink
            , height shrink
            , spacing 1
            ]
            [ el
                [ width shrink
                , height shrink
                , paddingXY 0 3
                , spacing 4
                , Font.size 14
                , Font.bold
                , Font.color Color.transparent500
                ]
                (maturity
                    |> Maturity.toString zoneInfo
                    |> text
                )
            , el
                [ width shrink
                , height shrink
                , paddingXY 0 3
                , Font.size 12
                , Font.regular
                , Font.color Color.transparent300
                ]
                (maturity
                    |> Maturity.toPosix
                    |> Duration.toString time
                    |> text
                )
            ]
        ]


liquidities :
    { model | device : Device }
    -> Pair
    -> { poolInfo | pool : Remote { pool | assetLiquidity : String, collateralLiquidity : String } }
    -> Element msg
liquidities { device } pair { pool } =
    row
        ([ height shrink
         , Font.bold
         , Font.size 14
         ]
            ++ (if Device.isPhoneOrTablet device then
                    []

                else
                    [ width <| px 170
                    , paddingXY 0 3
                    , alignRight
                    ]
               )
        )
        [ row
            [ width shrink
            , centerX
            ]
            (case pool of
                Loading ->
                    [ Loading.view ]

                Failure ->
                    []

                Success { assetLiquidity, collateralLiquidity } ->
                    [ el
                        [ centerX
                        , Font.color Color.transparent500
                        ]
                        (text assetLiquidity)
                    , el [ centerX ] <| text " "
                    , el
                        [ centerX
                        , Font.color Color.transparent300
                        ]
                        (pair
                            |> Pair.toAsset
                            |> Token.toSymbol
                            |> text
                        )
                    , el [ centerX, Font.color Color.transparent500 ] (text " + ")
                    , el
                        [ centerX
                        , Font.color Color.transparent500
                        ]
                        (text collateralLiquidity)
                    , el [ centerX ] <| text " "
                    , el
                        [ centerX
                        , Font.color Color.transparent300
                        ]
                        (pair
                            |> Pair.toCollateral
                            |> Token.toSymbol
                            |> text
                        )
                    ]
            )
        ]


estimatedAPR :
    { model | device : Device }
    -> { poolInfo | pool : Remote { pool | apr : String } }
    -> Element msg
estimatedAPR { device } { pool } =
    el
        [ (if Device.isPhoneOrTablet device then
            shrink

           else
            px 130
          )
            |> width
        , height shrink
        , alignRight
        ]
        (case pool of
            Loading ->
                Loading.view

            Failure ->
                none

            Success { apr } ->
                el
                    [ width shrink
                    , height shrink
                    , paddingXY 10 8
                    , centerX
                    , Background.color Color.positive100
                    , Border.rounded 28
                    , Font.bold
                    , Font.size 14
                    , Font.color Color.positive500
                    , Font.center
                    ]
                    (text apr)
        )


collateralFactor :
    { model | device : Device }
    -> Pair
    -> { poolInfo | pool : Remote { pool | cf : String } }
    -> Element msg
collateralFactor { device } pair { pool } =
    column
        [ (if Device.isPhoneOrTablet device then
            shrink

           else
            px 170
          )
            |> width
        , height shrink
        , alignRight
        ]
        (case pool of
            Loading ->
                [ Loading.view ]

            Failure ->
                []

            Success { cf } ->
                [ el
                    [ paddingXY 3 0
                    , centerX
                    , Font.bold
                    , Font.size 14
                    , Font.color Color.transparent500
                    ]
                    (text cf)
                , el
                    [ paddingXY 4 0
                    , centerX
                    , Font.bold
                    , Font.size 12
                    , Font.color Color.transparent300
                    ]
                    ([ pair
                        |> Pair.toAsset
                        |> Token.toSymbol
                     , "PER"
                     , pair
                        |> Pair.toCollateral
                        |> Token.toSymbol
                     ]
                        |> String.join " "
                        |> text
                    )
                ]
        )


buttons : { model | device : Device } -> Pair -> { poolInfo | maturity : Maturity } -> Element msg
buttons { device } pair { maturity } =
    row
        (if device |> Device.isPhoneOrTablet then
            [ width fill
            , spacing 7
            ]

         else
            [ width <| px 177
            , alignRight
            , spacing 7
            ]
        )
        [ link
            [ width fill
            , height <| px 44
            , Background.color Color.primary100
            , Border.rounded 4
            , mouseDown [ Background.color Color.primary400 ]
            , mouseOver [ Background.color Color.primary300 ]
            , Font.bold
            , Font.size 16
            , Font.color Color.primary500
            ]
            { url = Lend.init { pair = pair, maturity = maturity } |> Lend.toUrl
            , label =
                el
                    [ centerX
                    , centerY
                    ]
                    (text "Lend")
            }
        , link
            [ width fill
            , height <| px 44
            , Background.color Color.primary100
            , Border.rounded 4
            , mouseDown [ Background.color Color.primary400 ]
            , mouseOver [ Background.color Color.primary300 ]
            , Font.bold
            , Font.size 16
            , Font.color Color.primary500
            ]
            { url = Borrow.init { pair = pair, maturity = maturity } |> Borrow.toUrl
            , label =
                el
                    [ centerX
                    , centerY
                    ]
                    (text "Borrow")
            }
        ]
