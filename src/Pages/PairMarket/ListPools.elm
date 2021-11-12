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
        , below
        , centerX
        , centerY
        , clipX
        , column
        , el
        , fill
        , height
        , link
        , mouseDown
        , mouseOver
        , none
        , onRight
        , padding
        , paddingEach
        , paddingXY
        , px
        , row
        , shrink
        , spacing
        , table
        , text
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
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
    if Device.isPhoneOrTablet model.device then
        column
            [ width fill
            , height shrink
            ]
            (el
                [ width fill
                , height shrink
                , Border.solid
                , Border.widthEach
                    { top = 1
                    , right = 0
                    , bottom = 0
                    , left = 0
                    }
                , Border.color Color.transparent100
                ]
                none
                :: (list
                        |> List.map
                            (\( maturity, poolInfo ) ->
                                { maturity = maturity
                                , poolInfo = poolInfo
                                }
                            )
                        |> List.map
                            (\({ maturity, poolInfo } as info) ->
                                column
                                    [ width fill
                                    , height shrink
                                    , padding 16
                                    , spacing 16
                                    , Border.solid
                                    , Border.widthEach
                                        { top = 0
                                        , right = 1
                                        , bottom = 1
                                        , left = 1
                                        }
                                    , Border.color Color.transparent100
                                    ]
                                    [ column
                                        [ width shrink
                                        , height shrink
                                        , spacing 12
                                        ]
                                        [ el
                                            [ width shrink
                                            , height shrink
                                            , paddingXY 0 3
                                            , Font.bold
                                            , Font.size 12
                                            , Font.color Color.transparent300
                                            ]
                                            (text "MATURITY TIME")
                                        , el
                                            [ width shrink
                                            , height shrink
                                            ]
                                            (maturityInfo model maturity)
                                        ]
                                    , column
                                        [ width shrink
                                        , height shrink
                                        , spacing 6
                                        , alignLeft
                                        ]
                                        [ el
                                            [ width shrink
                                            , height shrink
                                            , paddingXY 0 3
                                            , Font.bold
                                            , Font.size 12
                                            , Font.color Color.transparent300
                                            ]
                                            (text "LIQUIDITY")
                                        , el
                                            [ width shrink
                                            , height shrink
                                            ]
                                            (liquidities msgs model page info)
                                        ]
                                    , row
                                        [ width fill
                                        , height shrink
                                        ]
                                        [ column
                                            [ width shrink
                                            , height shrink
                                            , spacing 6
                                            ]
                                            [ el
                                                [ width shrink
                                                , height shrink
                                                , paddingXY 0 3
                                                , Font.bold
                                                , Font.size 12
                                                , Font.color Color.transparent300
                                                ]
                                                (text "APR")
                                            , el
                                                [ width shrink
                                                , height shrink
                                                ]
                                                (estimatedAPR model poolInfo)
                                            ]
                                        , column
                                            [ width shrink
                                            , height shrink
                                            , spacing 6
                                            , alignRight
                                            ]
                                            [ el
                                                [ width shrink
                                                , height shrink
                                                , paddingXY 0 3
                                                , alignRight
                                                , Font.bold
                                                , Font.size 12
                                                , Font.color Color.transparent300
                                                ]
                                                (text "COLLATERAL FACTOR")
                                            , el
                                                [ width shrink
                                                , height shrink
                                                , alignRight
                                                ]
                                                (collateralFactor msgs model page info)
                                            ]
                                        ]
                                    , el
                                        [ width fill
                                        , height shrink
                                        ]
                                        (buttons model page maturity)
                                    ]
                            )
                   )
            )

    else
        table
            [ width fill
            , height shrink
            ]
            { data =
                list
                    |> List.map
                        (\( maturity, poolInfo ) ->
                            { maturity = maturity
                            , poolInfo = poolInfo
                            }
                        )
            , columns =
                [ { header =
                        el
                            [ width fill
                            , height shrink
                            , paddingXY 24 20
                            , Background.color Color.list
                            , Border.solid
                            , Border.widthEach
                                { top = 1
                                , right = 0
                                , bottom = 1
                                , left = 1
                                }
                            , Border.color Color.transparent100
                            , Font.bold
                            , Font.size 12
                            , Font.color Color.transparent300
                            ]
                            (text "MATURITY TIME")
                  , width = fill
                  , view =
                        \{ maturity } ->
                            el
                                [ height <| px 72
                                , paddingEach
                                    { top = 0
                                    , right = 0
                                    , bottom = 0
                                    , left = 24
                                    }
                                , clipX
                                , Border.solid
                                , Border.widthEach
                                    { top = 0
                                    , right = 0
                                    , bottom = 1
                                    , left = 1
                                    }
                                , Border.color Color.transparent100
                                ]
                                (maturityInfo model maturity)
                  }
                , { header =
                        el
                            [ width fill
                            , height shrink
                            , paddingXY 0 20
                            , Background.color Color.list
                            , Border.solid
                            , Border.widthEach
                                { top = 1
                                , right = 0
                                , bottom = 1
                                , left = 0
                                }
                            , Border.color Color.transparent100
                            , Font.bold
                            , Font.size 12
                            , Font.color Color.transparent300
                            , Font.center
                            ]
                            (text "LIQUIDITY")
                  , width = px 170
                  , view =
                        \info ->
                            el
                                [ width fill
                                , height <| px 72
                                , Border.solid
                                , Border.widthEach
                                    { top = 0
                                    , right = 0
                                    , bottom = 1
                                    , left = 0
                                    }
                                , Border.color Color.transparent100
                                ]
                                (liquidities msgs model page info)
                  }
                , { header =
                        el
                            [ width fill
                            , height shrink
                            , paddingXY 0 20
                            , Background.color Color.list
                            , Border.solid
                            , Border.widthEach
                                { top = 1
                                , right = 0
                                , bottom = 1
                                , left = 0
                                }
                            , Border.color Color.transparent100
                            , Font.bold
                            , Font.size 12
                            , Font.color Color.transparent300
                            , Font.center
                            ]
                            (text "MAX APR")
                  , width = px 130
                  , view =
                        \{ poolInfo } ->
                            el
                                [ height <| px 72
                                , Border.solid
                                , Border.widthEach
                                    { top = 0
                                    , right = 0
                                    , bottom = 1
                                    , left = 0
                                    }
                                , Border.color Color.transparent100
                                ]
                                (estimatedAPR model poolInfo)
                  }
                , { header =
                        el
                            [ width fill
                            , height shrink
                            , paddingXY 0 20
                            , Background.color Color.list
                            , Border.solid
                            , Border.widthEach
                                { top = 1
                                , right = 0
                                , bottom = 1
                                , left = 0
                                }
                            , Border.color Color.transparent100
                            , Font.bold
                            , Font.size 12
                            , Font.color Color.transparent300
                            , Font.center
                            ]
                            (text "MIN CDP")
                  , width = px 190
                  , view =
                        \info ->
                            el
                                [ height <| px 72
                                , Border.solid
                                , Border.widthEach
                                    { top = 0
                                    , right = 0
                                    , bottom = 1
                                    , left = 0
                                    }
                                , Border.color Color.transparent100
                                ]
                                (collateralFactor msgs model page info)
                  }
                , { header =
                        el
                            [ width fill
                            , height fill
                            , Background.color Color.list
                            , Border.solid
                            , Border.widthEach
                                { top = 1
                                , right = 1
                                , bottom = 1
                                , left = 0
                                }
                            , Border.color Color.transparent100
                            ]
                            none
                  , width = px 211
                  , view =
                        \{ maturity } ->
                            el
                                [ height <| px 72
                                , paddingEach
                                    { top = 0
                                    , right = 24
                                    , bottom = 0
                                    , left = 0
                                    }
                                , Border.solid
                                , Border.widthEach
                                    { top = 0
                                    , right = 1
                                    , bottom = 1
                                    , left = 0
                                    }
                                , Border.color Color.transparent100
                                ]
                                (buttons model page maturity)
                  }
                ]
            }


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
        { maturity : Maturity
        , poolInfo : Remote () { poolInfo | assetLiquidity : String, collateralLiquidity : String }
        }
    -> Element msg
liquidities msgs ({ device } as model) page { maturity, poolInfo } =
    (if device |> Device.isPhoneOrTablet then
        row

     else
        column
    )
        [ width shrink
        , if device |> Device.isPhoneOrTablet then
            alignLeft

          else
            centerX
        , centerY
        , spacing 4
        , Font.bold
        , Font.size 14
        ]
        (case poolInfo of
            Loading ->
                [ el
                    [ width <| px 50 ]
                    Loading.view
                ]

            Failure _ ->
                []

            Success { assetLiquidity, collateralLiquidity } ->
                [ assetBalance msgs model page maturity assetLiquidity
                , if device |> Device.isPhoneOrTablet then
                    el
                        [ alignLeft
                        , Font.bold
                        , Font.color Color.transparent500
                        ]
                        (text "+")

                  else
                    none
                , collateralBalance msgs model page maturity collateralLiquidity
                ]
        )


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
        , paddingXY 0 3
        , centerX
        , centerY
        ]
        (case poolInfo of
            Loading ->
                el
                    [ width <| px 50
                    , centerX
                    ]
                    Loading.view

            Failure _ ->
                none

            Success { apr } ->
                (if device |> Device.isPhoneOrTablet then
                    el
                        [ width shrink
                        , height shrink
                        , paddingXY 0 3
                        , Font.bold
                        , Font.size 14
                        , Font.color Color.positive500
                        ]

                 else
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
                )
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
    ->
        { maturity : Maturity
        , poolInfo : Remote () { poolInfo | cf : String }
        }
    -> Element msg
collateralFactor msgs ({ device } as model) page { maturity, poolInfo } =
    el
        [ (if Device.isPhoneOrTablet device then
            shrink

           else
            px 170
          )
            |> width
        , height shrink
        , paddingXY 0 3
        , centerX
        , centerY
        ]
        (case poolInfo of
            Loading ->
                el
                    [ width <| px 50
                    , centerX
                    ]
                    Loading.view

            Failure _ ->
                none

            Success { cf } ->
                el
                    [ centerX ]
                    (collateralFactorAmount msgs model page maturity cf)
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
            , alignRight
            , centerY
            ]

         else
            [ width <| px 177
            , centerY
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
    -> { model | device : Device }
    -> { page | pair : Pair, tooltip : Maybe Tooltip }
    -> Maturity
    -> String
    -> Element msg
assetBalance msgs { device } { pair, tooltip } maturity assetLiquidity =
    assetLiquidity
        |> Truncate.amount
        |> (\{ full, truncated } ->
                truncated
                    |> Maybe.map
                        (\short ->
                            row
                                [ width shrink
                                , height shrink
                                , if device |> Device.isPhoneOrTablet then
                                    alignLeft

                                  else
                                    centerX
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
                                                |> Tooltip.amount device

                                        else
                                            none

                                    _ ->
                                        none
                                  )
                                    |> (if device |> Device.isPhoneOrTablet then
                                            below

                                        else
                                            onRight
                                       )
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
                            , if device |> Device.isPhoneOrTablet then
                                alignLeft

                              else
                                centerX
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
    -> { model | device : Device }
    -> { page | pair : Pair, tooltip : Maybe Tooltip }
    -> Maturity
    -> String
    -> Element msg
collateralBalance msgs { device } { pair, tooltip } maturity collateralLiquidity =
    collateralLiquidity
        |> Truncate.amount
        |> (\{ full, truncated } ->
                truncated
                    |> Maybe.map
                        (\short ->
                            row
                                [ if device |> Device.isPhoneOrTablet then
                                    alignLeft

                                  else
                                    centerX
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
                                                |> Tooltip.amount device

                                        else
                                            none

                                    _ ->
                                        none
                                  )
                                    |> (if device |> Device.isPhoneOrTablet then
                                            below

                                        else
                                            onRight
                                       )
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
                            , if device |> Device.isPhoneOrTablet then
                                alignLeft

                              else
                                centerX
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
    -> { model | device : Device }
    -> { page | pair : Pair, tooltip : Maybe Tooltip }
    -> Maturity
    -> String
    -> Element msg
collateralFactorAmount msgs { device } { pair, tooltip } maturity factorAmount =
    factorAmount
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
                                                |> Tooltip.amountRight device

                                        else
                                            none

                                    _ ->
                                        none
                                  )
                                    |> (if device |> Device.isPhoneOrTablet then
                                            below

                                        else
                                            onRight
                                       )
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
