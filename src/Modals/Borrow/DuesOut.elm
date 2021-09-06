module Modals.Borrow.DuesOut exposing
    ( DuesOut(..)
    , hasFailure
    , hasTransaction
    , hasZeroInput
    , init
    , isCorrect
    , isDefault
    , slide
    , slideZero
    , switchBorrowSetting
    , switchBorrowSettingZero
    , updateAssetOut
    , updateAssetOutZero
    , updateCollateralIn
    , updateCollateralInZero
    , updateDebtIn
    , updateDebtInZero
    , view
    )

import Data.Balances as Balances exposing (Balances)
import Data.Images exposing (Images)
import Data.Maturity as Maturity exposing (Maturity)
import Data.Pair as Pair exposing (Pair)
import Data.Percent as Percent exposing (Percent)
import Data.Pool exposing (Pool)
import Data.Remote exposing (Remote(..))
import Data.Status exposing (Status(..))
import Data.Token as Token
import Data.TokenImages exposing (TokenImages)
import Data.Uint as Uint
import Element
    exposing
        ( Element
        , alignLeft
        , alignRight
        , alpha
        , behindContent
        , below
        , centerX
        , centerY
        , clip
        , column
        , el
        , fill
        , height
        , newTabLink
        , none
        , onRight
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
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Modals.Borrow.Tooltip as Tooltip exposing (Tooltip)
import Time exposing (Posix)
import Utility.Color as Color
import Utility.Glass as Glass
import Utility.Image as Image
import Utility.Input as Input
import Utility.Loading as Loading
import Utility.TokenImage as TokenImage
import Utility.Truncate as Truncate


type DuesOut
    = Default (Remote DuesGivenPercent)
    | Slider SliderInput
    | Debt DebtInput
    | Collateral CollateralInput


type alias DuesGivenPercent =
    { debt : String
    , collateral : String
    , maxDebt : String
    , maxCollateral : String
    }


type alias DuesGivenDebt =
    { collateral : String
    , maxCollateral : String
    }


type alias DuesGivenCollateral =
    { debt : String
    , maxDebt : String
    }


type alias SliderInput =
    { percent : Percent
    , dues : Remote DuesGivenPercent
    }


type alias DebtInput =
    { percent : Percent
    , debt : String
    , dues : Remote DuesGivenDebt
    }


type alias CollateralInput =
    { percent : Percent
    , dues : Remote DuesGivenCollateral
    , collateral : String
    }


init : DuesOut
init =
    { debt = ""
    , collateral = ""
    , maxDebt = ""
    , maxCollateral = ""
    }
        |> Success
        |> Default


hasFailure : { modal | duesOut : DuesOut } -> Bool
hasFailure { duesOut } =
    case duesOut of
        Default Failure ->
            True

        Slider { dues } ->
            case dues of
                Failure ->
                    True

                _ ->
                    False

        Debt { dues } ->
            case dues of
                Failure ->
                    True

                _ ->
                    False

        Collateral { dues } ->
            case dues of
                Failure ->
                    True

                _ ->
                    False

        _ ->
            False


isAmount : { modal | pool : { pool | pair : Pair }, assetOut : String, duesOut : DuesOut } -> Bool
isAmount { pool, assetOut, duesOut } =
    (assetOut |> Uint.isAmount (pool.pair |> Pair.toAsset))
        && (case duesOut of
                Default _ ->
                    True

                Slider _ ->
                    True

                Debt { debt } ->
                    debt |> Uint.isAmount (pool.pair |> Pair.toAsset)

                Collateral { collateral } ->
                    collateral |> Uint.isAmount (pool.pair |> Pair.toCollateral)
           )


hasBalanceIfUser :
    { model | user : Maybe { user | balances : Remote Balances } }
    -> { modal | pool : { pool | pair : Pair }, duesOut : DuesOut }
    -> Bool
hasBalanceIfUser { user } { pool, duesOut } =
    user
        |> Maybe.map
            (\{ balances } ->
                case balances of
                    Loading ->
                        True

                    Failure ->
                        False

                    Success successBalances ->
                        (case duesOut of
                            Default (Success { maxCollateral }) ->
                                Just maxCollateral

                            Slider { dues } ->
                                case dues of
                                    Success { maxCollateral } ->
                                        Just maxCollateral

                                    _ ->
                                        Nothing

                            Debt { dues } ->
                                case dues of
                                    Success { maxCollateral } ->
                                        Just maxCollateral

                                    _ ->
                                        Nothing

                            Collateral { collateral } ->
                                Just collateral

                            _ ->
                                Nothing
                        )
                            |> (\maybeCollateral ->
                                    maybeCollateral
                                        |> Maybe.map
                                            (\collateralIn ->
                                                successBalances
                                                    |> Balances.hasEnough
                                                        (pool.pair |> Pair.toCollateral)
                                                        collateralIn
                                            )
                                        |> Maybe.withDefault True
                               )
            )
        |> Maybe.withDefault True


isCorrect :
    { model | user : Maybe { user | balances : Remote Balances } }
    -> { modal | pool : { pool | pair : Pair }, assetOut : String, duesOut : DuesOut }
    -> Bool
isCorrect model modal =
    (hasFailure modal |> not)
        && isAmount modal
        && hasBalanceIfUser model modal


hasZeroInput : DuesOut -> Bool
hasZeroInput duesOut =
    case duesOut of
        Debt { debt } ->
            debt |> Input.isZero

        Collateral { collateral } ->
            collateral |> Input.isZero

        _ ->
            False


isDefault : { modal | duesOut : DuesOut } -> Bool
isDefault { duesOut } =
    case duesOut of
        Default _ ->
            True

        Slider { percent } ->
            (percent |> Percent.toFloat) == 64

        _ ->
            False


hasTransaction :
    { model
        | time : Posix
        , user : Maybe { user | balances : Remote Balances }
    }
    ->
        { modal
            | pool : Pool
            , assetOut : String
            , duesOut : DuesOut
        }
    -> Bool
hasTransaction { time, user } ({ pool, duesOut } as modal) =
    (pool.maturity |> Maturity.isActive time)
        && (modal |> isAmount)
        && (user
                |> Maybe.map
                    (\{ balances } ->
                        case balances of
                            Success successBalances ->
                                (case duesOut of
                                    Default (Success { maxCollateral }) ->
                                        Just maxCollateral

                                    Slider { dues } ->
                                        case dues of
                                            Success { maxCollateral } ->
                                                Just maxCollateral

                                            _ ->
                                                Nothing

                                    Debt { dues } ->
                                        case dues of
                                            Success { maxCollateral } ->
                                                Just maxCollateral

                                            _ ->
                                                Nothing

                                    Collateral { collateral } ->
                                        Just collateral

                                    _ ->
                                        Nothing
                                )
                                    |> (\maybeCollateral ->
                                            maybeCollateral
                                                |> Maybe.map
                                                    (\collateralOut ->
                                                        successBalances
                                                            |> Balances.hasEnough
                                                                (pool.pair |> Pair.toAsset)
                                                                collateralOut
                                                    )
                                                |> Maybe.withDefault False
                                       )

                            _ ->
                                False
                    )
                |> Maybe.withDefault False
           )
        && (case duesOut of
                Default (Success { debt, collateral, maxDebt, maxCollateral }) ->
                    (debt |> Input.isZero |> not)
                        && (collateral |> Input.isZero |> not)
                        && (maxDebt |> Input.isZero |> not)
                        && (maxCollateral |> Input.isZero |> not)

                Slider { dues } ->
                    case dues of
                        Success { debt, collateral, maxDebt, maxCollateral } ->
                            (debt |> Input.isZero |> not)
                                && (collateral |> Input.isZero |> not)
                                && (maxDebt |> Input.isZero |> not)
                                && (maxCollateral |> Input.isZero |> not)

                        _ ->
                            False

                Debt { dues } ->
                    case dues of
                        Success { collateral, maxCollateral } ->
                            (collateral |> Input.isZero |> not)
                                && (maxCollateral |> Input.isZero |> not)

                        _ ->
                            False

                Collateral { dues } ->
                    case dues of
                        Success { debt, maxDebt } ->
                            (debt |> Input.isZero |> not)
                                && (maxDebt |> Input.isZero |> not)

                        _ ->
                            False

                _ ->
                    False
           )


updateAssetOutZero : DuesOut -> DuesOut
updateAssetOutZero duesOut =
    case duesOut of
        Default _ ->
            { debt = ""
            , collateral = ""
            , maxDebt = ""
            , maxCollateral = ""
            }
                |> Success
                |> Default

        Slider sliderInput ->
            { sliderInput
                | dues =
                    { debt = ""
                    , collateral = ""
                    , maxDebt = ""
                    , maxCollateral = ""
                    }
                        |> Success
            }
                |> Slider

        Debt debtInput ->
            { debtInput
                | dues =
                    { collateral = ""
                    , maxCollateral = ""
                    }
                        |> Success
            }
                |> Debt

        Collateral collateralInput ->
            { collateralInput
                | dues =
                    { debt = ""
                    , maxDebt = ""
                    }
                        |> Success
            }
                |> Collateral


updateAssetOut : DuesOut -> DuesOut
updateAssetOut duesOut =
    case duesOut of
        Default _ ->
            Default Loading

        Slider sliderInput ->
            { sliderInput | dues = Loading }
                |> Slider

        Debt ({ debt } as debtInput) ->
            { debtInput
                | dues =
                    if debt |> Input.isZero then
                        { collateral = ""
                        , maxCollateral = ""
                        }
                            |> Success

                    else
                        Loading
            }
                |> Debt

        Collateral ({ collateral } as collateralInput) ->
            { collateralInput
                | dues =
                    if collateral |> Input.isZero then
                        { debt = ""
                        , maxDebt = ""
                        }
                            |> Success

                    else
                        Loading
            }
                |> Collateral


switchBorrowSettingZero : Bool -> DuesOut -> DuesOut
switchBorrowSettingZero checked duesOut =
    if checked then
        case duesOut of
            Default _ ->
                { percent = Percent.init
                , dues =
                    { debt = ""
                    , collateral = ""
                    , maxDebt = ""
                    , maxCollateral = ""
                    }
                        |> Success
                }
                    |> Slider

            _ ->
                duesOut

    else
        { debt = ""
        , collateral = ""
        , maxDebt = ""
        , maxCollateral = ""
        }
            |> Success
            |> Default


switchBorrowSetting : Bool -> DuesOut -> DuesOut
switchBorrowSetting checked duesOut =
    if checked then
        case duesOut of
            Default dues ->
                Slider
                    { percent = Percent.init
                    , dues = dues
                    }

            _ ->
                duesOut

    else
        case duesOut of
            Slider { percent, dues } ->
                if (percent |> Percent.toFloat) == 64 then
                    Default dues

                else
                    Default Loading

            Default _ ->
                duesOut

            _ ->
                Default Loading


slideZero : Float -> DuesOut
slideZero float =
    { percent = float |> Percent.fromFloat
    , dues =
        { debt = ""
        , collateral = ""
        , maxDebt = ""
        , maxCollateral = ""
        }
            |> Success
    }
        |> Slider


slide : Float -> DuesOut
slide float =
    { percent = float |> Percent.fromFloat
    , dues = Loading
    }
        |> Slider


updateDebtInZero : String -> DuesOut -> DuesOut
updateDebtInZero string duesOut =
    (case duesOut of
        Slider { percent } ->
            Just percent

        Debt { percent } ->
            Just percent

        Collateral { percent } ->
            Just percent

        _ ->
            Nothing
    )
        |> Maybe.map
            (\percent ->
                { percent = percent
                , debt = string
                , dues =
                    { collateral = ""
                    , maxCollateral = ""
                    }
                        |> Success
                }
                    |> Debt
            )
        |> Maybe.withDefault duesOut


updateDebtIn : String -> DuesOut -> DuesOut
updateDebtIn string duesOut =
    (case duesOut of
        Slider { percent } ->
            Just percent

        Debt { percent } ->
            Just percent

        Collateral { percent } ->
            Just percent

        _ ->
            Nothing
    )
        |> Maybe.map
            (\percent ->
                { percent = percent
                , debt = string
                , dues =
                    if string |> Input.isZero then
                        { collateral = ""
                        , maxCollateral = ""
                        }
                            |> Success

                    else
                        Loading
                }
                    |> Debt
            )
        |> Maybe.withDefault duesOut


updateCollateralInZero : String -> DuesOut -> DuesOut
updateCollateralInZero string duesOut =
    (case duesOut of
        Slider { percent } ->
            Just percent

        Debt { percent } ->
            Just percent

        Collateral { percent } ->
            Just percent

        _ ->
            Nothing
    )
        |> Maybe.map
            (\percent ->
                { percent = percent
                , dues =
                    { debt = ""
                    , maxDebt = ""
                    }
                        |> Success
                , collateral = string
                }
                    |> Collateral
            )
        |> Maybe.withDefault duesOut


updateCollateralIn : String -> DuesOut -> DuesOut
updateCollateralIn string duesOut =
    (case duesOut of
        Slider { percent } ->
            Just percent

        Debt { percent } ->
            Just percent

        Collateral { percent } ->
            Just percent

        _ ->
            Nothing
    )
        |> Maybe.map
            (\percent ->
                { percent = percent
                , dues =
                    if string |> Input.isZero then
                        { debt = ""
                        , maxDebt = ""
                        }
                            |> Success

                    else
                        Loading
                , collateral = string
                }
                    |> Collateral
            )
        |> Maybe.withDefault duesOut


view :
    { msgs
        | switchBorrowSetting : Bool -> msg
        , slide : Float -> msg
        , inputDebtIn : String -> msg
        , inputCollateralIn : String -> msg
        , inputMax : msg
        , onMouseEnter : Tooltip -> msg
        , onMouseLeave : msg
    }
    ->
        { model
            | time : Posix
            , images : Images
            , tokenImages : TokenImages
            , user : Maybe { user | balances : Remote Balances }
        }
    ->
        { modal
            | pool : Pool
            , assetOut : String
            , duesOut : DuesOut
            , apr : Remote String
            , cf : Remote String
            , tooltip : Maybe Tooltip
        }
    -> Element msg
view msgs model modal =
    column
        [ width fill
        , height shrink
        ]
        [ title msgs model modal
        , position msgs model modal
        , maturityInfo model modal
        ]


title :
    { msgs
        | switchBorrowSetting : Bool -> msg
        , onMouseEnter : Tooltip -> msg
        , onMouseLeave : msg
    }
    -> { model | images : Images }
    -> { modal | duesOut : DuesOut, tooltip : Maybe Tooltip }
    -> Element msg
title msgs { images } ({ tooltip } as modal) =
    row
        [ width fill
        , height shrink
        , paddingXY 20 12
        , spacing 9
        , Border.width 1
        , Border.solid
        , Border.color Color.transparent100
        , Border.roundEach
            { topLeft = 4
            , topRight = 4
            , bottomRight = 0
            , bottomLeft = 0
            }
        , Font.size 14
        ]
        [ el
            [ alignLeft
            , paddingXY 0 4
            , Font.size 16
            , Font.bold
            , Font.color Color.transparent500
            ]
            (text "Your Collateralized Debt")
        , Image.info images
            [ width <| px 16
            , alignLeft
            , centerY
            , Events.onMouseEnter (msgs.onMouseEnter Tooltip.Dues)
            , Events.onMouseLeave msgs.onMouseLeave
            , (case tooltip of
                Just Tooltip.Dues ->
                    Tooltip.dues

                _ ->
                    none
              )
                |> onRight
            ]
        , el
            [ alignRight
            , centerY
            , paddingXY 0 3
            , Font.size 14
            , Font.regular
            , Font.color Color.transparent300
            ]
            (text "Customize CDP")
        , switch msgs modal
        ]


switch :
    { msgs | switchBorrowSetting : Bool -> msg }
    -> { modal | duesOut : DuesOut }
    -> Element msg
switch msgs { duesOut } =
    Input.checkbox
        [ width shrink
        , height shrink
        , alignRight
        , centerY
        ]
        { onChange = msgs.switchBorrowSetting
        , icon =
            \checked ->
                if checked then
                    el
                        [ width <| px 40
                        , height <| px 20
                        , padding 2
                        , Background.color Color.primary400
                        , Border.rounded 999
                        ]
                        (el
                            [ width <| px 16
                            , height <| px 16
                            , alignRight
                            , Background.color Color.light100
                            , Border.rounded 999
                            ]
                            none
                        )

                else
                    el
                        [ width <| px 40
                        , height <| px 20
                        , padding 2
                        , Background.color Color.transparent200
                        , Border.rounded 999
                        ]
                        (el
                            [ width <| px 16
                            , height <| px 16
                            , alignLeft
                            , Background.color Color.dark300
                            , Border.rounded 999
                            ]
                            none
                        )
        , checked =
            case duesOut of
                Default _ ->
                    False

                _ ->
                    True
        , label = Input.labelHidden "SwitchSetting"
        }


position :
    { msgs
        | slide : Float -> msg
        , inputDebtIn : String -> msg
        , inputCollateralIn : String -> msg
        , inputMax : msg
        , onMouseEnter : Tooltip -> msg
        , onMouseLeave : msg
    }
    ->
        { model
            | images : Images
            , tokenImages : TokenImages
            , user : Maybe { user | balances : Remote Balances }
        }
    ->
        { modal
            | pool : { pool | pair : Pair }
            , assetOut : String
            , duesOut : DuesOut
            , apr : Remote String
            , cf : Remote String
            , tooltip : Maybe Tooltip
        }
    -> Element msg
position msgs model ({ duesOut } as modal) =
    column
        [ width fill
        , height shrink
        , padding 20
        , spacing 16
        , Border.widthEach
            { top = 0
            , right = 1
            , bottom = 1
            , left = 1
            }
        , Border.solid
        , Border.color Color.transparent100
        ]
        [ (case duesOut of
            Slider { percent } ->
                Just percent

            Debt { percent } ->
                Just percent

            Collateral { percent } ->
                Just percent

            _ ->
                Nothing
          )
            |> Maybe.map (sliderSection msgs model modal)
            |> Maybe.withDefault none
        , column
            [ width fill
            , height shrink
            , spacing 12
            ]
            [ estimatedAPR modal
            , collateralFactor modal
            ]
        , debtInSection msgs model modal
        , collateralInSection msgs model modal
        ]


sliderSection :
    { msgs | slide : Float -> msg }
    -> { model | user : Maybe { user | balances : Remote Balances } }
    -> { modal | pool : { pool | pair : Pair }, assetOut : String, duesOut : DuesOut }
    -> Percent
    -> Element msg
sliderSection msgs model modal percent =
    column
        [ width fill
        , height shrink
        , spacing 10
        ]
        [ row
            [ width fill
            , height shrink
            , paddingXY 0 3
            , Font.size 14
            ]
            [ el
                [ alignLeft
                , Font.regular
                , Font.color Color.transparent500
                ]
                (text "Adjust your collateral factor")
            , newTabLink
                [ alignRight
                , Font.regular
                , Font.color Color.primary500
                ]
                { url = "https://timeswap.gitbook.io/timeswap/deep-dive/borrowing"
                , label = text "Learn more"
                }
            ]
        , column
            [ width fill
            , height shrink
            , spacing 6
            ]
            [ slider msgs model modal percent
            , row
                [ width fill
                , height shrink
                , paddingXY 0 2
                , Font.size 12
                , Font.color Color.transparent300
                ]
                [ el [ alignLeft, Font.regular ] (text "Low")
                , el [ alignRight, Font.regular ] (text "High")
                ]
            ]
        ]


slider :
    { msgs | slide : Float -> msg }
    -> { model | user : Maybe { user | balances : Remote Balances } }
    -> { modal | pool : { pool | pair : Pair }, assetOut : String, duesOut : DuesOut }
    -> Percent
    -> Element msg
slider msgs model modal percent =
    Input.slider
        [ width fill
        , el
            [ width fill
            , height <| px 2
            , centerY
            , (if isCorrect model modal then
                Color.transparent100

               else
                Color.negative500
              )
                |> Background.color
            ]
            none
            |> behindContent
        ]
        { onChange = msgs.slide
        , label = Input.labelHidden "Slider"
        , min = 0
        , max = 128
        , value = percent |> Percent.toFloat
        , thumb =
            Input.thumb
                [ width <| px 20
                , height <| px 20
                , Background.color Color.primary500
                , Border.rounded 999
                , Border.width 2
                , Border.color Color.transparent500
                ]
        , step = Just 1
        }


estimatedAPR :
    { modal | apr : Remote String }
    -> Element msg
estimatedAPR { apr } =
    row
        ([ width shrink
         , height <| px 32
         , paddingXY 12 0
         , spacing 5
         , centerX
         ]
            ++ Glass.lightWhiteModal 20
        )
        [ el
            [ width shrink
            , height shrink
            , paddingXY 0 3
            , centerY
            , Font.regular
            , Font.size 14
            , Font.color Color.transparent400
            ]
            (text "Estimated APR:")
        , el
            [ width shrink
            , height shrink
            , paddingXY 0 3
            , centerY
            , Font.bold
            , Font.size 18
            , Font.color Color.negative500
            ]
            (case apr of
                Loading ->
                    el
                        [ width <| px 50
                        , height shrink
                        ]
                        Loading.view

                Failure ->
                    none

                Success successAPR ->
                    text successAPR
            )
        , el
            [ paddingXY 4 0
            , centerY
            , Font.bold
            , Font.size 18
            , Font.color Color.negative500
            ]
            (text "%")
        ]


collateralFactor :
    { modal
        | pool : { pool | pair : Pair }
        , assetOut : String
        , duesOut : DuesOut
        , cf : Remote String
    }
    -> Element msg
collateralFactor { pool, cf } =
    row
        ([ width shrink
         , height <| px 32
         , paddingXY 12 0
         , spacing 5
         , centerX
         ]
            ++ Glass.lightWhiteModal 20
        )
        [ el
            [ width shrink
            , height shrink
            , paddingXY 0 3
            , centerY
            , Font.regular
            , Font.size 14
            , Font.color Color.transparent400
            ]
            (text "Collateral factor:")
        , el
            [ width shrink
            , height shrink
            , paddingXY 0 3
            , centerY
            , Font.bold
            , Font.size 18
            , Font.color Color.transparent500
            ]
            (case cf of
                Loading ->
                    el
                        [ width <| px 50
                        , height shrink
                        ]
                        Loading.view

                Failure ->
                    none

                Success successCF ->
                    text successCF
            )
        , el
            [ paddingXY 4 0
            , centerY
            , Font.bold
            , Font.size 12
            , Font.color Color.transparent300
            ]
            ([ pool.pair
                |> Pair.toAsset
                |> Token.toSymbol
             , "PER"
             , pool.pair
                |> Pair.toCollateral
                |> Token.toSymbol
             ]
                |> String.join " "
                |> text
            )
        ]


debtInSection :
    { msgs
        | inputDebtIn : String -> msg
        , onMouseEnter : Tooltip -> msg
        , onMouseLeave : msg
    }
    ->
        { model
            | images : Images
            , tokenImages : TokenImages
            , user : Maybe { user | balances : Remote Balances }
        }
    ->
        { modal
            | pool : { pool | pair : Pair }
            , assetOut : String
            , duesOut : DuesOut
            , tooltip : Maybe Tooltip
        }
    -> Element msg
debtInSection msgs ({ images } as model) ({ duesOut, tooltip } as modal) =
    column
        [ width fill
        , height shrink
        , spacing 12
        , alignLeft
        ]
        [ row
            [ width shrink
            , height shrink
            , spacing 8
            ]
            [ el
                [ width shrink
                , height shrink
                , paddingXY 0 3
                , alignLeft
                , Font.regular
                , Font.size 14
                , Font.color Color.transparent400
                ]
                (text "Amount at repay")
            , Image.info images
                [ width <| px 16
                , centerY
                , Events.onMouseEnter (msgs.onMouseEnter Tooltip.Debt)
                , Events.onMouseLeave msgs.onMouseLeave
                , (case tooltip of
                    Just Tooltip.Debt ->
                        Tooltip.debt

                    _ ->
                        none
                  )
                    |> onRight
                ]
            , (case duesOut of
                Default Loading ->
                    True

                Slider { dues } ->
                    case dues of
                        Loading ->
                            True

                        _ ->
                            False

                Collateral { dues } ->
                    case dues of
                        Loading ->
                            True

                        _ ->
                            False

                _ ->
                    False
              )
                |> (\isLoading ->
                        if isLoading then
                            el
                                [ width <| px 50
                                , height shrink
                                , centerY
                                ]
                                Loading.view

                        else
                            none
                   )
            ]
        , debtInTextbox msgs model modal
        ]


debtInTextbox :
    { msgs | inputDebtIn : String -> msg }
    ->
        { model
            | tokenImages : TokenImages
            , user : Maybe { user | balances : Remote Balances }
        }
    ->
        { modal
            | pool : { pool | pair : Pair }
            , assetOut : String
            , duesOut : DuesOut
        }
    -> Element msg
debtInTextbox msgs model modal =
    row
        [ width fill
        , height <| px 44
        ]
        [ debtInLogo model modal
        , debtInAmount msgs model modal
        ]


debtInLogo :
    { model
        | tokenImages : TokenImages
        , user : Maybe { user | balances : Remote Balances }
    }
    ->
        { modal
            | pool : { pool | pair : Pair }
            , assetOut : String
            , duesOut : DuesOut
        }
    -> Element msg
debtInLogo ({ tokenImages } as model) ({ pool } as modal) =
    row
        [ width shrink
        , height fill
        , paddingXY 12 0
        , spacing 6
        , Background.color Color.primary100
        , Border.widthEach
            { top = 1
            , right = 0
            , bottom = 1
            , left = 1
            }
        , Border.solid
        , (if isCorrect model modal then
            Color.transparent100

           else
            Color.negative500
          )
            |> Border.color
        , Border.roundEach
            { topLeft = 4
            , topRight = 0
            , bottomRight = 0
            , bottomLeft = 4
            }
        ]
        [ pool.pair
            |> Pair.toAsset
            |> TokenImage.icon tokenImages
                [ width <| px 24
                , alignLeft
                , centerY
                ]
        , el
            [ alignLeft
            , centerY
            , Font.regular
            , Font.size 16
            , Font.color Color.light100
            ]
            (pool.pair
                |> Pair.toAsset
                |> Token.toSymbol
                |> text
            )
        ]


debtInAmount :
    { msgs | inputDebtIn : String -> msg }
    -> { model | user : Maybe { user | balances : Remote Balances } }
    ->
        { modal
            | pool : { pool | pair : Pair }
            , assetOut : String
            , duesOut : DuesOut
        }
    -> Element msg
debtInAmount msgs model ({ duesOut } as modal) =
    el
        ([ width fill
         , height fill
         , paddingEach
            { top = 0
            , right = 12
            , bottom = 0
            , left = 0
            }
         , spacing 8
         , Border.widthEach
            { top = 1
            , right = 1
            , bottom = 1
            , left = 0
            }
         , Border.solid
         , (if isCorrect model modal then
                Color.transparent100

            else
                Color.negative500
           )
            |> Border.color
         , Border.roundEach
            { topLeft = 0
            , topRight = 4
            , bottomRight = 4
            , bottomLeft = 0
            }
         ]
            ++ (case duesOut of
                    Default _ ->
                        [ Background.color Color.primary100
                        , alpha 0.75
                        ]

                    _ ->
                        []
               )
        )
        (debtInInput msgs model modal)


debtInInput :
    { msgs | inputDebtIn : String -> msg }
    -> { model | user : Maybe { user | balances : Remote Balances } }
    ->
        { modal
            | pool : { pool | pair : Pair }
            , assetOut : String
            , duesOut : DuesOut
        }
    -> Element msg
debtInInput msgs model ({ duesOut } as modal) =
    (\debtIn ->
        Input.text
            [ width fill
            , height shrink
            , paddingXY 12 4
            , alignLeft
            , centerY
            , Background.color Color.none
            , Border.color Color.none
            , Font.regular
            , Font.size 16
            , (if isCorrect model modal then
                Color.transparent500

               else
                Color.negative500
              )
                |> Font.color
            ]
            { onChange = msgs.inputDebtIn
            , text = debtIn
            , placeholder =
                Input.placeholder
                    [ Font.color Color.transparent100 ]
                    (text "0.0")
                    |> Just
            , label = Input.labelHidden "Debt Amount"
            }
    )
        |> (\element ->
                case duesOut of
                    Default dues ->
                        el
                            [ width fill
                            , height shrink
                            , spacing 12
                            , alignLeft
                            , centerY
                            , Font.regular
                            , Font.size 16
                            ]
                            (case dues of
                                Success { debt } ->
                                    el
                                        [ paddingXY 12 4
                                        , centerY
                                        , clip
                                        , (if isCorrect model modal then
                                            Color.transparent500

                                           else
                                            Color.negative500
                                          )
                                            |> Font.color
                                        ]
                                        (text debt)

                                _ ->
                                    el
                                        [ paddingXY 12 4
                                        , centerY
                                        , Font.color Color.transparent100
                                        ]
                                        (text "0.0")
                            )

                    Slider { dues } ->
                        case dues of
                            Success { debt } ->
                                element debt

                            _ ->
                                element ""

                    Debt { debt } ->
                        element debt

                    Collateral { dues } ->
                        case dues of
                            Success { debt } ->
                                element debt

                            _ ->
                                element ""
           )


collateralInSection :
    { msgs
        | inputCollateralIn : String -> msg
        , inputMax : msg
        , onMouseEnter : Tooltip -> msg
        , onMouseLeave : msg
    }
    ->
        { model
            | images : Images
            , tokenImages : TokenImages
            , user : Maybe { user | balances : Remote Balances }
        }
    ->
        { modal
            | pool : { pool | pair : Pair }
            , assetOut : String
            , duesOut : DuesOut
            , tooltip : Maybe Tooltip
        }
    -> Element msg
collateralInSection msgs ({ images, user } as model) ({ duesOut, tooltip } as modal) =
    column
        [ width fill
        , height shrink
        , spacing 12
        , alignLeft
        ]
        [ row
            [ width fill
            , height shrink
            , spacing 8
            ]
            [ el
                [ width shrink
                , height shrink
                , paddingXY 0 3
                , alignLeft
                , Font.regular
                , Font.size 14
                , Font.color Color.transparent400
                ]
                (text "Collateral to lock")
            , Image.info images
                [ width <| px 16
                , centerY
                , Events.onMouseEnter (msgs.onMouseEnter Tooltip.Collateral)
                , Events.onMouseLeave msgs.onMouseLeave
                , (case tooltip of
                    Just Tooltip.Collateral ->
                        Tooltip.collateral

                    _ ->
                        none
                  )
                    |> onRight
                ]
            , (case duesOut of
                Default Loading ->
                    True

                Slider { dues } ->
                    case dues of
                        Loading ->
                            True

                        _ ->
                            False

                Debt { dues } ->
                    case dues of
                        Loading ->
                            True

                        _ ->
                            False

                _ ->
                    False
              )
                |> (\isLoading ->
                        if isLoading then
                            el
                                [ width <| px 50
                                , height shrink
                                , centerY
                                ]
                                Loading.view

                        else
                            none
                   )
            , user
                |> Maybe.map
                    (\{ balances } ->
                        case balances of
                            Loading ->
                                el
                                    [ height <| px 20
                                    , alignRight
                                    , centerY
                                    ]
                                    Loading.view

                            Failure ->
                                none

                            Success successBalances ->
                                collateralBalance msgs successBalances modal
                    )
                |> Maybe.withDefault none
            ]
        , collateralInTextbox msgs model modal
        ]


collateralBalance :
    { msgs
        | onMouseEnter : Tooltip -> msg
        , onMouseLeave : msg
    }
    -> Balances
    -> { modal | pool : { pool | pair : Pair }, tooltip : Maybe Tooltip }
    -> Element msg
collateralBalance msgs balances { pool, tooltip } =
    balances
        |> Balances.get (pool.pair |> Pair.toCollateral)
        |> Truncate.amount
        |> (\{ full, truncated } ->
                truncated
                    |> Maybe.map
                        (\short ->
                            row
                                [ height <| px 20
                                , alignRight
                                , centerY
                                , Font.size 14
                                , Font.color Color.transparent300
                                ]
                                [ el
                                    [ paddingXY 0 3
                                    , Font.regular
                                    ]
                                    (text "Your Balance: ")
                                , pool.pair
                                    |> Pair.toCollateral
                                    |> Token.toSymbol
                                    |> (\symbol ->
                                            el
                                                [ paddingEach
                                                    { top = 3
                                                    , right = 0
                                                    , bottom = 2
                                                    , left = 0
                                                    }
                                                , Font.regular
                                                , Border.widthEach
                                                    { top = 0
                                                    , right = 0
                                                    , bottom = 1
                                                    , left = 0
                                                    }
                                                , Border.dashed
                                                , Border.color Color.transparent200
                                                , Events.onMouseEnter (msgs.onMouseEnter Tooltip.CollateralBalance)
                                                , Events.onMouseLeave msgs.onMouseLeave
                                                , (case tooltip of
                                                    Just Tooltip.CollateralBalance ->
                                                        [ full
                                                        , symbol
                                                        ]
                                                            |> String.join " "
                                                            |> Tooltip.collateralBalance

                                                    _ ->
                                                        none
                                                  )
                                                    |> below
                                                ]
                                                ([ short
                                                 , symbol
                                                 ]
                                                    |> String.join " "
                                                    |> text
                                                )
                                       )
                                ]
                        )
                    |> Maybe.withDefault
                        (el
                            [ height <| px 20
                            , alignRight
                            , centerY
                            , paddingXY 0 3
                            , Font.regular
                            , Font.size 14
                            , Font.color Color.transparent300
                            ]
                            ([ "Your Balance:"
                             , full
                             , pool.pair
                                |> Pair.toCollateral
                                |> Token.toSymbol
                             ]
                                |> String.join " "
                                |> text
                            )
                        )
           )


collateralInTextbox :
    { msgs | inputCollateralIn : String -> msg, inputMax : msg }
    ->
        { model
            | tokenImages : TokenImages
            , user : Maybe { user | balances : Remote Balances }
        }
    ->
        { modal
            | pool : { pool | pair : Pair }
            , assetOut : String
            , duesOut : DuesOut
        }
    -> Element msg
collateralInTextbox msgs model modal =
    row
        [ width fill
        , height <| px 44
        ]
        [ collateralInLogo model modal
        , collateralInAmount msgs model modal
        ]


collateralInLogo :
    { model
        | tokenImages : TokenImages
        , user : Maybe { user | balances : Remote Balances }
    }
    ->
        { modal
            | pool : { pool | pair : Pair }
            , assetOut : String
            , duesOut : DuesOut
        }
    -> Element msg
collateralInLogo ({ tokenImages } as model) ({ pool } as modal) =
    row
        [ width shrink
        , height fill
        , paddingXY 12 0
        , spacing 6
        , Background.color Color.primary100
        , Border.widthEach
            { top = 1
            , right = 0
            , bottom = 1
            , left = 1
            }
        , Border.solid
        , (if isCorrect model modal then
            Color.transparent100

           else
            Color.negative500
          )
            |> Border.color
        , Border.roundEach
            { topLeft = 4
            , topRight = 0
            , bottomRight = 0
            , bottomLeft = 4
            }
        ]
        [ pool.pair
            |> Pair.toCollateral
            |> TokenImage.icon tokenImages
                [ width <| px 24
                , alignLeft
                , centerY
                ]
        , el
            [ alignLeft
            , centerY
            , Font.regular
            , Font.size 16
            , Font.color Color.light100
            ]
            (pool.pair
                |> Pair.toCollateral
                |> Token.toSymbol
                |> text
            )
        ]


collateralInAmount :
    { msgs | inputCollateralIn : String -> msg, inputMax : msg }
    -> { model | user : Maybe { user | balances : Remote Balances } }
    ->
        { modal
            | pool : { pool | pair : Pair }
            , assetOut : String
            , duesOut : DuesOut
        }
    -> Element msg
collateralInAmount msgs ({ user } as model) ({ duesOut } as modal) =
    row
        ([ width fill
         , height fill
         , paddingEach
            { top = 0
            , right = 12
            , bottom = 0
            , left = 0
            }
         , spacing 8
         , Border.widthEach
            { top = 1
            , right = 1
            , bottom = 1
            , left = 0
            }
         , Border.solid
         , (if isCorrect model modal then
                Color.transparent100

            else
                Color.negative500
           )
            |> Border.color
         , Border.roundEach
            { topLeft = 0
            , topRight = 4
            , bottomRight = 4
            , bottomLeft = 0
            }
         ]
            ++ (case duesOut of
                    Default _ ->
                        [ Background.color Color.primary100
                        , alpha 0.75
                        ]

                    _ ->
                        []
               )
        )
        [ collateralInInput msgs model modal
        , user
            |> Maybe.map
                (\{ balances } ->
                    case balances of
                        Success _ ->
                            case duesOut of
                                Default _ ->
                                    none

                                _ ->
                                    maxButton msgs

                        _ ->
                            none
                )
            |> Maybe.withDefault none
        ]


collateralInInput :
    { msgs | inputCollateralIn : String -> msg }
    -> { model | user : Maybe { user | balances : Remote Balances } }
    ->
        { modal
            | pool : { pool | pair : Pair }
            , assetOut : String
            , duesOut : DuesOut
        }
    -> Element msg
collateralInInput msgs model ({ duesOut } as modal) =
    (\collateralIn ->
        Input.text
            [ width fill
            , height shrink
            , paddingXY 12 4
            , alignLeft
            , centerY
            , Background.color Color.none
            , Border.color Color.none
            , Font.regular
            , Font.size 16
            , (if isCorrect model modal then
                Color.transparent500

               else
                Color.negative500
              )
                |> Font.color
            ]
            { onChange = msgs.inputCollateralIn
            , text = collateralIn
            , placeholder =
                Input.placeholder
                    [ Font.color Color.transparent100 ]
                    (text "0.0")
                    |> Just
            , label = Input.labelHidden "Collateral Amount"
            }
    )
        |> (\element ->
                case duesOut of
                    Default dues ->
                        el
                            [ width fill
                            , height shrink
                            , spacing 12
                            , alignLeft
                            , centerY
                            , Font.regular
                            , Font.size 16
                            ]
                            (case dues of
                                Success { collateral } ->
                                    el
                                        [ paddingXY 12 4
                                        , centerY
                                        , clip
                                        , (if isCorrect model modal then
                                            Color.transparent500

                                           else
                                            Color.negative500
                                          )
                                            |> Font.color
                                        ]
                                        (text collateral)

                                _ ->
                                    el
                                        [ paddingXY 12 4
                                        , centerY
                                        , Font.color Color.transparent100
                                        ]
                                        (text "0.0")
                            )

                    Slider { dues } ->
                        case dues of
                            Success { collateral } ->
                                element collateral

                            _ ->
                                element ""

                    Debt { dues } ->
                        case dues of
                            Success { collateral } ->
                                element collateral

                            _ ->
                                element ""

                    Collateral { collateral } ->
                        element collateral
           )


maxButton : { msgs | inputMax : msg } -> Element msg
maxButton msgs =
    Input.button
        [ width shrink
        , height shrink
        , centerY
        , Font.regular
        , Font.size 16
        , Font.color Color.negative500
        ]
        { onPress = Just msgs.inputMax
        , label = text "MAX"
        }


maturityInfo :
    { model | time : Posix, images : Images }
    -> { modal | pool : { pool | maturity : Maturity } }
    -> Element msg
maturityInfo { time, images } { pool } =
    row
        [ width fill
        , height <| px 36
        , paddingEach
            { top = 0
            , right = 15
            , bottom = 0
            , left = 20
            }
        , spacing 10
        , Background.color Color.primary100
        , Border.roundEach
            { topLeft = 0
            , topRight = 0
            , bottomRight = 4
            , bottomLeft = 4
            }
        , Font.size 14
        , Font.color Color.light100
        ]
        [ Image.hourglass images
            [ width <| px 20
            , alignLeft
            , centerY
            ]
        , el
            [ paddingXY 0 3
            , alignLeft
            , centerY
            , Font.regular
            ]
            (text "Time to maturity")
        , el
            [ width <| px 144
            , paddingXY 0 3
            , alignRight
            , centerY
            , Font.regular
            ]
            (pool.maturity
                |> Maturity.toDuration time
                |> (\status ->
                        (case status of
                            Active duration ->
                                duration

                            Matured _ ->
                                "Matured"
                        )
                            |> text
                   )
            )
        ]
