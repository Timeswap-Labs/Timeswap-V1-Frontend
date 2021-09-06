module Pages.PairMarket.ListPools exposing (view)

import Data.Device as Device exposing (Device)
import Data.Images exposing (Images)
import Data.Maturity as Maturity exposing (Maturity)
import Data.Pair as Pair exposing (Pair)
import Data.Pools exposing (PoolInfo)
import Data.Remote exposing (Remote(..))
import Data.Status exposing (Status(..))
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
import Time exposing (Posix)
import Utility.Color as Color
import Utility.Image as Image
import Utility.Loading as Loading
import Utility.Router as Router


view :
    { model | device : Device, time : Posix, zoneInfo : Maybe ZoneInfo, images : Images }
    -> Pair
    -> List ( Maturity, Remote PoolInfo )
    -> Element msg
view model pair list =
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
                |> List.map
                    (\( maturity, poolInfo ) ->
                        ( maturity |> Maturity.toKey
                        , singlePool model pair ( maturity, poolInfo )
                        )
                    )
            )
        ]


singlePool :
    { model | device : Device, time : Posix, zoneInfo : Maybe ZoneInfo, images : Images }
    -> Pair
    -> ( Maturity, Remote PoolInfo )
    -> Element msg
singlePool ({ device } as model) pair ( maturity, poolInfo ) =
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
            [ maturityInfo model maturity
            , liquidities model pair poolInfo
            , estimatedAPR model poolInfo
            , collateralFactor model pair poolInfo
            , buttons model pair maturity
            ]


maturityInfo :
    { model | time : Posix, zoneInfo : Maybe ZoneInfo, images : Images }
    -> Maturity
    -> Element msg
maturityInfo { time, zoneInfo, images } maturity =
    row
        [ width shrink
        , height shrink
        , alignLeft
        , centerY
        , spacing 12
        ]
        [ Image.hourglassPrimary images
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
                ((case maturity |> Maturity.toDuration time of
                    Active string ->
                        "Matures in " ++ string

                    Matured string ->
                        "Matured " ++ string ++ " ago"
                 )
                    |> text
                )
            ]
        ]


liquidities :
    { model | device : Device }
    -> Pair
    -> Remote { poolInfo | assetLiquidity : String, collateralLiquidity : String }
    -> Element msg
liquidities { device } pair poolInfo =
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
            (case poolInfo of
                Loading ->
                    [ Loading.view ]

                Failure ->
                    []

                Success { assetLiquidity, collateralLiquidity } ->
                    [ el
                        [ centerX
                        , Font.bold
                        , Font.color Color.transparent500
                        ]
                        (text assetLiquidity)
                    , el [ centerX, Font.bold ] <| text " "
                    , el
                        [ centerX
                        , Font.bold
                        , Font.color Color.transparent300
                        ]
                        (pair
                            |> Pair.toAsset
                            |> Token.toSymbol
                            |> text
                        )
                    , el
                        [ centerX
                        , Font.bold
                        , Font.color Color.transparent500
                        ]
                        (text " + ")
                    , el
                        [ centerX
                        , Font.bold
                        , Font.color Color.transparent500
                        ]
                        (text collateralLiquidity)
                    , el [ centerX, Font.bold ] <| text " "
                    , el
                        [ centerX
                        , Font.bold
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
    -> Remote { poolInfo | apr : String }
    -> Element msg
estimatedAPR { device } poolInfo =
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
        (case poolInfo of
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
    -> Remote { poolInfo | cf : String }
    -> Element msg
collateralFactor { device } pair poolInfo =
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
        (case poolInfo of
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


buttons :
    { model | device : Device }
    -> Pair
    -> Maturity
    -> Element msg
buttons { device } pair maturity =
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
        [ el
            [ width fill
            , height shrink
            ]
            (link
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
                { url = Router.toLend { pair = pair, maturity = maturity }
                , label =
                    el
                        [ centerX
                        , centerY
                        ]
                        (text "Lend")
                }
            )
        , el
            [ width fill
            , height shrink
            ]
            (link
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
                { url = Router.toBorrow { pair = pair, maturity = maturity }
                , label =
                    el
                        [ centerX
                        , centerY
                        ]
                        (text "Borrow")
                }
            )
        ]
