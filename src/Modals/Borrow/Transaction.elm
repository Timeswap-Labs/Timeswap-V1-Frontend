module Modals.Borrow.Transaction exposing (view)

import Data.Address as Address exposing (Address)
import Data.Allowances as Allowances exposing (Allowances)
import Data.Balances as Balances exposing (Balances)
import Data.Deadline as Deadline exposing (Deadline)
import Data.Device as Device exposing (Device)
import Data.Images exposing (Images)
import Data.Maturity as Maturity
import Data.Pair as Pair exposing (Pair)
import Data.Percent as Percent exposing (Percent)
import Data.Pool exposing (Pool)
import Data.Remote exposing (Remote(..))
import Data.Slippage exposing (Slippage)
import Data.Token as Token exposing (Token)
import Data.Uint as Uint exposing (Uint)
import Element
    exposing
        ( Element
        , below
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
import Json.Encode as Encode exposing (Value)
import Modals.Borrow.DuesOut as DuesOut exposing (DuesOut)
import Modals.Borrow.Tooltip as Tooltip exposing (Tooltip)
import Time exposing (Posix)
import Utility.Color as Color
import Utility.Image as Image
import Utility.Input as Input
import Utility.Router as Router


type Transaction
    = GivenPercent TransactionPercent
    | GivenDebt TransactionDebt
    | GivenCollateral TransactionCollateral


type alias TransactionPercent =
    { pool : Pool
    , to : Address
    , assetOut : Uint
    , percent : Percent
    , maxDebt : Uint
    , maxCollateral : Uint
    , deadline : Int
    }


type alias TransactionDebt =
    { pool : Pool
    , to : Address
    , assetOut : Uint
    , debtIn : Uint
    , maxCollateral : Uint
    , deadline : Int
    }


type alias TransactionCollateral =
    { pool : Pool
    , to : Address
    , assetOut : Uint
    , collateralIn : Uint
    , maxDebt : Uint
    , deadline : Int
    }


toTransaction :
    { model
        | time : Posix
        , deadline : Deadline
        , user : Remote userError { user | address : Address, balances : Remote () Balances, allowances : Remote () Allowances }
    }
    -> { modal | pool : Pool, assetOut : String, duesOut : DuesOut }
    -> Maybe Transaction
toTransaction ({ time, deadline, user } as model) ({ pool, assetOut, duesOut } as modal) =
    if
        (pool.maturity |> Maturity.isActive time)
            && (assetOut |> Input.isZero |> not)
            && (duesOut |> DuesOut.hasZeroInput |> not)
            && hasAllowance model modal
    then
        case duesOut of
            DuesOut.Default (Success { maxDebt, maxCollateral }) ->
                Maybe.map4
                    (\address uintAssetOut uintMaxDebt uintMaxCollateral ->
                        { pool = pool
                        , to = address
                        , assetOut = uintAssetOut
                        , percent = Percent.init
                        , maxDebt = uintMaxDebt
                        , maxCollateral = uintMaxCollateral
                        , deadline = deadline |> Deadline.toInt time
                        }
                            |> GivenPercent
                    )
                    (case user of
                        Success { address } ->
                            Just address

                        _ ->
                            Nothing
                    )
                    (assetOut |> Uint.fromString)
                    (maxDebt |> Uint.fromString)
                    (maxCollateral |> Uint.fromString)

            DuesOut.Slider { percent, dues } ->
                case dues of
                    Success { maxDebt, maxCollateral } ->
                        Maybe.map4
                            (\address uintAssetOut uintMaxDebt uintMaxCollateral ->
                                { pool = pool
                                , to = address
                                , assetOut = uintAssetOut
                                , percent = percent
                                , maxDebt = uintMaxDebt
                                , maxCollateral = uintMaxCollateral
                                , deadline = deadline |> Deadline.toInt time
                                }
                                    |> GivenPercent
                            )
                            (case user of
                                Success { address } ->
                                    Just address

                                _ ->
                                    Nothing
                            )
                            (assetOut |> Uint.fromString)
                            (maxDebt |> Uint.fromString)
                            (maxCollateral |> Uint.fromString)

                    _ ->
                        Nothing

            DuesOut.Debt { debt, dues } ->
                case dues of
                    Success { maxCollateral } ->
                        Maybe.map4
                            (\address uintAssetOut uintDebtIn uintMaxCollateral ->
                                { pool = pool
                                , to = address
                                , assetOut = uintAssetOut
                                , debtIn = uintDebtIn
                                , maxCollateral = uintMaxCollateral
                                , deadline = deadline |> Deadline.toInt time
                                }
                                    |> GivenDebt
                            )
                            (case user of
                                Success { address } ->
                                    Just address

                                _ ->
                                    Nothing
                            )
                            (assetOut |> Uint.fromString)
                            (debt |> Uint.fromString)
                            (maxCollateral |> Uint.fromString)

                    _ ->
                        Nothing

            DuesOut.Collateral { collateral, dues } ->
                case dues of
                    Success { maxDebt } ->
                        Maybe.map4
                            (\address uintAssetOut uintCollateralIn uintMaxDebt ->
                                { pool = pool
                                , to = address
                                , assetOut = uintAssetOut
                                , collateralIn = uintCollateralIn
                                , maxDebt = uintMaxDebt
                                , deadline = deadline |> Deadline.toInt time
                                }
                                    |> GivenCollateral
                            )
                            (case user of
                                Success { address } ->
                                    Just address

                                _ ->
                                    Nothing
                            )
                            (assetOut |> Uint.fromString)
                            (collateral |> Uint.fromString)
                            (maxDebt |> Uint.fromString)

                    _ ->
                        Nothing

            _ ->
                Nothing

    else
        Nothing


encode : Transaction -> Value
encode transaction =
    case transaction of
        GivenPercent { pool, to, assetOut, percent, maxDebt, maxCollateral, deadline } ->
            [ ( "asset", pool.pair |> Pair.toAsset |> Token.encode )
            , ( "collateral", pool.pair |> Pair.toAsset |> Token.encode )
            , ( "maturity", pool.maturity |> Maturity.encode )
            , ( "assetTo", to |> Address.encode )
            , ( "dueTo", to |> Address.encode )
            , ( "assetOut", assetOut |> Uint.encode )
            , ( "percent", percent |> Percent.encode )
            , ( "maxDebt", maxDebt |> Uint.encode )
            , ( "maxCollateral", maxCollateral |> Uint.encode )
            , ( "deadline", deadline |> Encode.int )
            ]
                |> Encode.object

        GivenDebt { pool, to, assetOut, debtIn, maxCollateral, deadline } ->
            [ ( "asset", pool.pair |> Pair.toAsset |> Token.encode )
            , ( "collateral", pool.pair |> Pair.toAsset |> Token.encode )
            , ( "maturity", pool.maturity |> Maturity.encode )
            , ( "assetTo", to |> Address.encode )
            , ( "dueTo", to |> Address.encode )
            , ( "assetOut", assetOut |> Uint.encode )
            , ( "debtIn", debtIn |> Uint.encode )
            , ( "maxCollateral", maxCollateral |> Uint.encode )
            , ( "deadline", deadline |> Encode.int )
            ]
                |> Encode.object

        GivenCollateral { pool, to, assetOut, collateralIn, maxDebt, deadline } ->
            [ ( "asset", pool.pair |> Pair.toAsset |> Token.encode )
            , ( "collateral", pool.pair |> Pair.toAsset |> Token.encode )
            , ( "maturity", pool.maturity |> Maturity.encode )
            , ( "assetTo", to |> Address.encode )
            , ( "dueTo", to |> Address.encode )
            , ( "assetOut", assetOut |> Uint.encode )
            , ( "collateralIn", collateralIn |> Uint.encode )
            , ( "maxDebt", maxDebt |> Uint.encode )
            , ( "deadline", deadline |> Encode.int )
            ]
                |> Encode.object


encodeApprove : Token -> Value
encodeApprove token =
    [ ( "token", token |> Token.encode ) ]
        |> Encode.object


hasAllowance :
    { model | user : Remote userError { user | balances : Remote () Balances, allowances : Remote () Allowances } }
    -> { modal | pool : { pool | pair : Pair }, duesOut : DuesOut }
    -> Bool
hasAllowance { user } { pool, duesOut } =
    case user of
        Success { balances, allowances } ->
            case ( balances, allowances ) of
                ( Success successBalances, Success successAllowances ) ->
                    (case duesOut of
                        DuesOut.Default (Success { maxCollateral }) ->
                            Just maxCollateral

                        DuesOut.Slider { dues } ->
                            case dues of
                                Success { maxCollateral } ->
                                    Just maxCollateral

                                _ ->
                                    Nothing

                        DuesOut.Debt { dues } ->
                            case dues of
                                Success { maxCollateral } ->
                                    Just maxCollateral

                                _ ->
                                    Nothing

                        DuesOut.Collateral { collateral } ->
                            Just collateral

                        _ ->
                            Nothing
                    )
                        |> (\maybeCollateral ->
                                maybeCollateral
                                    |> Maybe.map
                                        (\collateralOut ->
                                            (successBalances
                                                |> Balances.hasEnough
                                                    (pool.pair |> Pair.toCollateral)
                                                    collateralOut
                                            )
                                                && (successAllowances
                                                        |> Allowances.hasEnough
                                                            (pool.pair |> Pair.toCollateral)
                                                            collateralOut
                                                   )
                                        )
                                    |> Maybe.withDefault False
                           )

                _ ->
                    False

        _ ->
            False


view :
    { msgs
        | approveBorrow : Value -> msg
        , borrow : Value -> msg
        , onMouseEnter : Tooltip -> msg
        , onMouseLeave : msg
    }
    ->
        { model
            | device : Device
            , time : Posix
            , slippage : Slippage
            , deadline : Deadline
            , images : Images
            , user : Remote userError { user | address : Address, balances : Remote () Balances, allowances : Remote () Allowances }
        }
    ->
        { modal
            | pool : Pool
            , assetOut : String
            , duesOut : DuesOut
            , tooltip : Maybe Tooltip
        }
    -> Element msg
view msgs ({ user } as model) modal =
    column
        [ width fill
        , height shrink
        , spacing 12
        ]
        [ case user of
            Success _ ->
                row
                    [ width fill
                    , height shrink
                    , spacing 20
                    ]
                    [ el
                        [ width fill
                        , height shrink
                        ]
                        (approveSection msgs model modal)
                    , el
                        [ width fill
                        , height shrink
                        ]
                        (borrowSection msgs model modal)
                    ]

            _ ->
                connectButton model
        , transactionInfo msgs model modal
        ]


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


approveSection :
    { msgs | approveBorrow : Value -> msg }
    ->
        { model
            | device : Device
            , time : Posix
            , user : Remote userError { user | balances : Remote () Balances, allowances : Remote () Allowances }
        }
    -> { modal | pool : Pool, assetOut : String, duesOut : DuesOut }
    -> Element msg
approveSection msgs model modal =
    if
        DuesOut.hasTransaction model modal
            && (hasAllowance model modal |> not)
    then
        approveButton msgs model modal

    else
        disabledApprove model


approveButton :
    { msgs | approveBorrow : Value -> msg }
    -> { model | device : Device }
    -> { modal | pool : { pool | pair : Pair } }
    -> Element msg
approveButton msgs { device } { pool } =
    Input.button
        ([ width fill
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
        { onPress =
            pool.pair
                |> Pair.toCollateral
                |> encodeApprove
                |> msgs.approveBorrow
                |> Just
        , label =
            el
                [ width shrink
                , height shrink
                , centerX
                , centerY
                , Font.bold
                , Font.size 16
                , Font.color Color.light100
                ]
                (text "Approve")
        }


disabledApprove : { model | device : Device } -> Element msg
disabledApprove { device } =
    el
        ([ width fill
         , paddingEach
            { top = 0
            , right = 16
            , bottom = 0
            , left = 10
            }
         , centerX
         , centerY
         , Background.color Color.primary100
         , Border.rounded 4
         , Font.size 16
         , Font.color Color.light100
         ]
            ++ (if Device.isPhoneOrTablet device then
                    [ height <| px 35 ]

                else
                    [ height <| px 44 ]
               )
        )
        (el
            [ width shrink
            , height shrink
            , centerX
            , centerY
            , Font.bold
            , Font.size 16
            , Font.color Color.transparent100
            ]
            (text "Approve")
        )


borrowSection :
    { msgs | borrow : Value -> msg }
    ->
        { model
            | device : Device
            , time : Posix
            , deadline : Deadline
            , user : Remote userError { user | address : Address, balances : Remote () Balances, allowances : Remote () Allowances }
        }
    -> { modal | pool : Pool, assetOut : String, duesOut : DuesOut }
    -> Element msg
borrowSection msgs model modal =
    toTransaction model modal
        |> Maybe.map (borrowButton msgs model)
        |> Maybe.withDefault (disabledBorrow model)


borrowButton :
    { msgs | borrow : Value -> msg }
    -> { model | device : Device }
    -> Transaction
    -> Element msg
borrowButton msgs { device } transaction =
    Input.button
        ([ width fill
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
        { onPress =
            transaction
                |> encode
                |> msgs.borrow
                |> Just
        , label =
            el
                [ width shrink
                , height shrink
                , centerX
                , centerY
                , Font.bold
                , Font.size 16
                , Font.color Color.light100
                ]
                (text "Borrow")
        }


disabledBorrow : { model | device : Device } -> Element msg
disabledBorrow { device } =
    el
        ([ width fill
         , paddingEach
            { top = 0
            , right = 16
            , bottom = 0
            , left = 10
            }
         , centerX
         , centerY
         , Background.color Color.primary100
         , Border.rounded 4
         , Font.size 16
         , Font.color Color.light100
         ]
            ++ (if Device.isPhoneOrTablet device then
                    [ height <| px 35 ]

                else
                    [ height <| px 44 ]
               )
        )
        (el
            [ width shrink
            , height shrink
            , centerX
            , centerY
            , Font.bold
            , Font.size 16
            , Font.color Color.transparent100
            ]
            (text "Borrow")
        )


transactionInfo :
    { msgs | onMouseEnter : Tooltip -> msg, onMouseLeave : msg }
    -> { model | time : Posix, slippage : Slippage, images : Images }
    ->
        { modal
            | pool : Pool
            , assetOut : String
            , duesOut : DuesOut
            , tooltip : Maybe Tooltip
        }
    -> Element msg
transactionInfo msgs ({ slippage, images } as model) ({ pool, duesOut, tooltip } as modal) =
    row
        [ width shrink
        , height <| px 20
        , spacing 5
        , centerX
        , Font.regular
        , Font.size 14
        , Font.color Color.transparent300
        , Events.onMouseEnter (msgs.onMouseEnter Tooltip.TransactionInfo)
        , Events.onMouseLeave msgs.onMouseLeave
        , (if DuesOut.hasTransactionInfo model modal then
            case tooltip of
                Just Tooltip.TransactionInfo ->
                    (case duesOut of
                        DuesOut.Default (Success { maxDebt, maxCollateral }) ->
                            Just ( maxDebt, maxCollateral )

                        DuesOut.Slider { dues } ->
                            case dues of
                                Success { maxDebt, maxCollateral } ->
                                    Just ( maxDebt, maxCollateral )

                                _ ->
                                    Nothing

                        DuesOut.Debt { debt, dues } ->
                            case dues of
                                Success { maxCollateral } ->
                                    Just ( debt, maxCollateral )

                                _ ->
                                    Nothing

                        DuesOut.Collateral { collateral, dues } ->
                            case dues of
                                Success { maxDebt } ->
                                    Just ( maxDebt, collateral )

                                _ ->
                                    Nothing

                        _ ->
                            Nothing
                    )
                        |> Maybe.map
                            (\min ->
                                Tooltip.transactionInfo pool.pair min slippage
                            )
                        |> Maybe.withDefault none

                _ ->
                    none

           else
            none
          )
            |> below
        ]
        (if DuesOut.hasTransactionInfo model modal then
            [ el
                [ paddingXY 0 3
                , centerY
                ]
                (text "View more transaction info")
            , Image.info images
                [ width <| px 20
                , centerY
                ]
            ]

         else
            []
        )
