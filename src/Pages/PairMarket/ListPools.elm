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
        , onRight
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
import Element.Events as Events
import Element.Font as Font
import Element.Keyed as Keyed
import Pages.PairMarket.Tooltip as Tooltip exposing (Tooltip)
import Time exposing (Posix)
import Utility.Color as Color
import Utility.Image as Image
import Utility.Loading as Loading
import Utility.Router as Router
import Utility.Truncate as Truncate


view :
    { msgs
        | onMouseEnter : Tooltip -> msg
        , onMouseLeave : msg
    }
    ->
        { model
            | device : Device
            , time : Posix
            , zoneInfo : Maybe ZoneInfo
            , images : Images
        }
    -> { page | pair : Pair, tooltip : Maybe Tooltip }
    -> List ( Maturity, Remote () PoolInfo )
    -> Element msg
view msgs model page list =
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
                        , singlePool msgs model page ( maturity, poolInfo )
                        )
                    )
            )
        ]


singlePool :
    { msgs
        | onMouseEnter : Tooltip -> msg
        , onMouseLeave : msg
    }
    -> { model | device : Device, time : Posix, zoneInfo : Maybe ZoneInfo, images : Images }
    -> { page | pair : Pair, tooltip : Maybe Tooltip }
    -> ( Maturity, Remote () PoolInfo )
    -> Element msg
singlePool msgs ({ device } as model) page (( maturity, poolInfo ) as info) =
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
            , liquidities msgs model page info
            , estimatedAPR model poolInfo
            , collateralFactor msgs model page info
            , buttons model page maturity
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
    { msgs
        | onMouseEnter : Tooltip -> msg
        , onMouseLeave : msg
    }
    -> { model | device : Device }
    -> { page | pair : Pair, tooltip : Maybe Tooltip }
    ->
        ( Maturity
        , Remote () { poolInfo | assetLiquidity : String, collateralLiquidity : String }
        )
    -> Element msg
liquidities msgs { device } page ( maturity, poolInfo ) =
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

                Failure _ ->
                    []

                Success { assetLiquidity, collateralLiquidity } ->
                    [ assetBalance msgs page maturity assetLiquidity
                    , el
                        [ centerX
                        , Font.bold
                        , Font.color Color.transparent500
                        ]
                        (text " + ")
                    , collateralBalance msgs page maturity collateralLiquidity
                    ]
            )
        ]


estimatedAPR :
    { model | device : Device }
    -> Remote () { poolInfo | apr : String }
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
                el
                    [ centerX ]
                    Loading.view

            Failure _ ->
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
                    ([ apr
                     , "%"
                     ]
                        |> String.join " "
                        |> text
                    )
        )


collateralFactor :
    { msgs
        | onMouseEnter : Tooltip -> msg
        , onMouseLeave : msg
    }
    -> { model | device : Device }
    -> { page | pair : Pair, tooltip : Maybe Tooltip }
    -> ( Maturity, Remote () { poolInfo | cf : String } )
    -> Element msg
collateralFactor msgs { device } page ( maturity, poolInfo ) =
    el
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
                el
                    [ centerX ]
                    Loading.view

            Failure _ ->
                none

            Success { cf } ->
                el
                    [ centerX ]
                    (collateralFactorAmount msgs page maturity cf)
        )


buttons :
    { model | device : Device }
    -> { page | pair : Pair }
    -> Maturity
    -> Element msg
buttons { device } { pair } maturity =
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


assetBalance :
    { msgs
        | onMouseEnter : Tooltip -> msg
        , onMouseLeave : msg
    }
    -> { page | pair : Pair, tooltip : Maybe Tooltip }
    -> Maturity
    -> String
    -> Element msg
assetBalance msgs { pair, tooltip } maturity assetLiquidity =
    assetLiquidity
        |> Truncate.amount
        |> (\{ full, truncated } ->
                truncated
                    |> Maybe.map
                        (\short ->
                            row
                                [ alignLeft
                                , centerY
                                , paddingEach
                                    { top = 3
                                    , right = 0
                                    , bottom = 2
                                    , left = 0
                                    }
                                , spacing 4
                                , Font.regular
                                , Border.widthEach
                                    { top = 0
                                    , right = 0
                                    , bottom = 1
                                    , left = 0
                                    }
                                , Border.dashed
                                , Border.color Color.transparent200
                                , Font.size 14
                                , Events.onMouseEnter (msgs.onMouseEnter (Tooltip.AssetLiquidity pair maturity))
                                , Events.onMouseLeave msgs.onMouseLeave
                                , (case tooltip of
                                    Just (Tooltip.AssetLiquidity chosenPair chosenMaturity) ->
                                        if chosenPair == pair && chosenMaturity == maturity then
                                            [ full
                                            , pair
                                                |> Pair.toAsset
                                                |> Token.toSymbol
                                            ]
                                                |> String.join " "
                                                |> Tooltip.amount

                                        else
                                            none

                                    _ ->
                                        none
                                  )
                                    |> onRight
                                ]
                                [ el
                                    [ Font.bold
                                    , Font.color Color.transparent500
                                    ]
                                    (text short)
                                , el
                                    [ Font.bold
                                    , Font.color Color.transparent300
                                    ]
                                    (pair
                                        |> Pair.toAsset
                                        |> Token.toSymbol
                                        |> text
                                    )
                                ]
                        )
                    |> Maybe.withDefault
                        (row
                            [ width shrink
                            , height shrink
                            , alignLeft
                            , centerY
                            , spacing 4
                            , Font.size 14
                            , Font.color Color.transparent300
                            ]
                            [ el
                                [ Font.bold
                                , Font.color Color.transparent500
                                ]
                                (text full)
                            , el
                                [ Font.bold
                                , Font.color Color.transparent300
                                ]
                                (pair
                                    |> Pair.toAsset
                                    |> Token.toSymbol
                                    |> text
                                )
                            ]
                        )
           )


collateralBalance :
    { msgs
        | onMouseEnter : Tooltip -> msg
        , onMouseLeave : msg
    }
    -> { page | pair : Pair, tooltip : Maybe Tooltip }
    -> Maturity
    -> String
    -> Element msg
collateralBalance msgs { pair, tooltip } maturity collateralLiquidity =
    collateralLiquidity
        |> Truncate.amount
        |> (\{ full, truncated } ->
                truncated
                    |> Maybe.map
                        (\short ->
                            row
                                [ alignLeft
                                , centerY
                                , paddingEach
                                    { top = 3
                                    , right = 0
                                    , bottom = 2
                                    , left = 0
                                    }
                                , spacing 4
                                , Font.regular
                                , Border.widthEach
                                    { top = 0
                                    , right = 0
                                    , bottom = 1
                                    , left = 0
                                    }
                                , Border.dashed
                                , Border.color Color.transparent200
                                , Font.size 14
                                , Events.onMouseEnter (msgs.onMouseEnter (Tooltip.CollateralLiquidity pair maturity))
                                , Events.onMouseLeave msgs.onMouseLeave
                                , (case tooltip of
                                    Just (Tooltip.CollateralLiquidity chosenPair chosenMaturity) ->
                                        if chosenPair == pair && chosenMaturity == maturity then
                                            [ full
                                            , pair
                                                |> Pair.toCollateral
                                                |> Token.toSymbol
                                            ]
                                                |> String.join " "
                                                |> Tooltip.amount

                                        else
                                            none

                                    _ ->
                                        none
                                  )
                                    |> onRight
                                ]
                                [ el
                                    [ Font.bold
                                    , Font.color Color.transparent500
                                    ]
                                    (text short)
                                , el
                                    [ Font.bold
                                    , Font.color Color.transparent300
                                    ]
                                    (pair
                                        |> Pair.toCollateral
                                        |> Token.toSymbol
                                        |> text
                                    )
                                ]
                        )
                    |> Maybe.withDefault
                        (row
                            [ width shrink
                            , height shrink
                            , alignLeft
                            , centerY
                            , spacing 4
                            , Font.size 14
                            , Font.color Color.transparent300
                            ]
                            [ el
                                [ Font.bold
                                , Font.color Color.transparent500
                                ]
                                (text full)
                            , el
                                [ Font.bold
                                , Font.color Color.transparent300
                                ]
                                (pair
                                    |> Pair.toCollateral
                                    |> Token.toSymbol
                                    |> text
                                )
                            ]
                        )
           )


collateralFactorAmount :
    { msgs
        | onMouseEnter : Tooltip -> msg
        , onMouseLeave : msg
    }
    -> { page | pair : Pair, tooltip : Maybe Tooltip }
    -> Maturity
    -> String
    -> Element msg
collateralFactorAmount msgs { pair, tooltip } maturity assetLiquidity =
    assetLiquidity
        |> Truncate.amount
        |> (\{ full, truncated } ->
                truncated
                    |> Maybe.map
                        (\short ->
                            row
                                [ alignLeft
                                , centerY
                                , spacing 4
                                , Font.regular
                                , Border.widthEach
                                    { top = 0
                                    , right = 0
                                    , bottom = 1
                                    , left = 0
                                    }
                                , Border.dashed
                                , Border.color Color.transparent200
                                , Font.size 14
                                , Events.onMouseEnter (msgs.onMouseEnter (Tooltip.CollateralFactor pair maturity))
                                , Events.onMouseLeave msgs.onMouseLeave
                                , (case tooltip of
                                    Just (Tooltip.CollateralFactor chosenPair chosenMaturity) ->
                                        if chosenPair == pair && chosenMaturity == maturity then
                                            [ full
                                            , pair
                                                |> Pair.toAsset
                                                |> Token.toSymbol
                                            , "PER"
                                            , pair
                                                |> Pair.toCollateral
                                                |> Token.toSymbol
                                            ]
                                                |> String.join " "
                                                |> Tooltip.amount

                                        else
                                            none

                                    _ ->
                                        none
                                  )
                                    |> onRight
                                ]
                                [ el
                                    [ paddingEach
                                        { top = 3
                                        , right = 0
                                        , bottom = 2
                                        , left = 0
                                        }
                                    , Font.bold
                                    , Font.size 14
                                    , Font.color Color.transparent500
                                    ]
                                    (text short)
                                , el
                                    [ paddingEach
                                        { top = 4
                                        , right = 0
                                        , bottom = 3
                                        , left = 0
                                        }
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
                    |> Maybe.withDefault
                        (row
                            [ width shrink
                            , height shrink
                            , alignLeft
                            , centerY
                            , spacing 4
                            ]
                            [ el
                                [ paddingXY 3 0
                                , Font.bold
                                , Font.size 14
                                , Font.color Color.transparent500
                                ]
                                (text full)
                            , el
                                [ paddingXY 4 0
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
           )
