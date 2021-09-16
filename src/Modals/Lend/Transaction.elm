module Modals.Lend.Transaction exposing (view)

import Data.Address as Address exposing (Address)
import Data.Allowances as Allowances exposing (Allowances)
import Data.Balances as Balances exposing (Balances)
import Data.Deadline as Deadline exposing (Deadline)
import Data.Device as Device exposing (Device)
import Data.ERC20 as ERC20 exposing (ERC20)
import Data.Images exposing (Images)
import Data.Maturity as Maturity
import Data.Pair as Pair exposing (Pair)
import Data.Percent as Percent exposing (Percent)
import Data.Pool exposing (Pool)
import Data.Remote exposing (Remote(..))
import Data.Slippage exposing (Slippage)
import Data.Token as Token
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
import Modals.Lend.ClaimsOut as ClaimsOut exposing (ClaimsOut)
import Modals.Lend.Tooltip as Tooltip exposing (Tooltip)
import Time exposing (Posix)
import Utility.Color as Color
import Utility.Image as Image
import Utility.Input as Input
import Utility.Router as Router


type Transaction
    = GivenPercent TransactionPercent
    | GivenBond TransactionBond
    | GivenInsurance TransactionInsurance


type alias TransactionPercent =
    { pool : Pool
    , to : Address
    , assetIn : Uint
    , percent : Percent
    , minBond : Uint
    , minInsurance : Uint
    , deadline : Int
    }


type alias TransactionBond =
    { pool : Pool
    , to : Address
    , assetIn : Uint
    , bondOut : Uint
    , minInsurance : Uint
    , deadline : Int
    }


type alias TransactionInsurance =
    { pool : Pool
    , to : Address
    , assetIn : Uint
    , insuranceOut : Uint
    , minBond : Uint
    , deadline : Int
    }


toTransaction :
    { model
        | time : Posix
        , deadline : Deadline
        , user : Remote userError { user | address : Address, balances : Remote () Balances, allowances : Remote () Allowances }
    }
    -> { modal | pool : Pool, assetIn : String, claimsOut : ClaimsOut }
    -> Maybe Transaction
toTransaction ({ time, deadline, user } as model) ({ pool, assetIn, claimsOut } as modal) =
    if
        (pool.maturity |> Maturity.isActive time)
            && (assetIn |> Input.isZero |> not)
            && (claimsOut |> ClaimsOut.hasZeroInput |> not)
            && hasAllowance model modal
    then
        case claimsOut of
            ClaimsOut.Default (Success { minBond, minInsurance }) ->
                Maybe.map4
                    (\address uintAssetIn uintMinBond uintMinInsurance ->
                        { pool = pool
                        , to = address
                        , assetIn = uintAssetIn
                        , percent = Percent.init
                        , minBond = uintMinBond
                        , minInsurance = uintMinInsurance
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
                    (assetIn |> Uint.fromAmount (pool.pair |> Pair.toAsset))
                    (minBond |> Uint.fromAmount (pool.pair |> Pair.toAsset))
                    (minInsurance |> Uint.fromAmount (pool.pair |> Pair.toCollateral))

            ClaimsOut.Slider { percent, claims } ->
                case claims of
                    Success { minBond, minInsurance } ->
                        Maybe.map4
                            (\address uintAssetIn uintMinBond uintMinInsurance ->
                                { pool = pool
                                , to = address
                                , assetIn = uintAssetIn
                                , percent = percent
                                , minBond = uintMinBond
                                , minInsurance = uintMinInsurance
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
                            (assetIn |> Uint.fromAmount (pool.pair |> Pair.toAsset))
                            (minBond |> Uint.fromAmount (pool.pair |> Pair.toAsset))
                            (minInsurance |> Uint.fromAmount (pool.pair |> Pair.toCollateral))

                    _ ->
                        Nothing

            ClaimsOut.Bond { bond, claims } ->
                case claims of
                    Success { minInsurance } ->
                        Maybe.map4
                            (\address uintAssetIn uintBondOut uintMinInsurance ->
                                { pool = pool
                                , to = address
                                , assetIn = uintAssetIn
                                , bondOut = uintBondOut
                                , minInsurance = uintMinInsurance
                                , deadline = deadline |> Deadline.toInt time
                                }
                                    |> GivenBond
                            )
                            (case user of
                                Success { address } ->
                                    Just address

                                _ ->
                                    Nothing
                            )
                            (assetIn |> Uint.fromAmount (pool.pair |> Pair.toAsset))
                            (bond |> Uint.fromAmount (pool.pair |> Pair.toAsset))
                            (minInsurance |> Uint.fromAmount (pool.pair |> Pair.toCollateral))

                    _ ->
                        Nothing

            ClaimsOut.Insurance { insurance, claims } ->
                case claims of
                    Success { minBond } ->
                        Maybe.map4
                            (\address uintAssetIn uintInsuranceOut uintMinBond ->
                                { pool = pool
                                , to = address
                                , assetIn = uintAssetIn
                                , insuranceOut = uintInsuranceOut
                                , minBond = uintMinBond
                                , deadline = deadline |> Deadline.toInt time
                                }
                                    |> GivenInsurance
                            )
                            (case user of
                                Success { address } ->
                                    Just address

                                _ ->
                                    Nothing
                            )
                            (assetIn |> Uint.fromAmount (pool.pair |> Pair.toAsset))
                            (insurance |> Uint.fromAmount (pool.pair |> Pair.toCollateral))
                            (minBond |> Uint.fromAmount (pool.pair |> Pair.toAsset))

                    _ ->
                        Nothing

            _ ->
                Nothing

    else
        Nothing


encode : Transaction -> Value
encode transaction =
    case transaction of
        GivenPercent { pool, to, assetIn, percent, minBond, minInsurance, deadline } ->
            [ ( "asset", pool.pair |> Pair.toAsset |> Token.encode )
            , ( "collateral", pool.pair |> Pair.toAsset |> Token.encode )
            , ( "maturity", pool.maturity |> Maturity.encode )
            , ( "bondTo", to |> Address.encode )
            , ( "insuranceTo", to |> Address.encode )
            , ( "assetIn", assetIn |> Uint.encode )
            , ( "percent", percent |> Percent.encode )
            , ( "minBond", minBond |> Uint.encode )
            , ( "minInsurance", minInsurance |> Uint.encode )
            , ( "deadline", deadline |> Encode.int )
            ]
                |> Encode.object

        GivenBond { pool, to, assetIn, bondOut, minInsurance, deadline } ->
            [ ( "asset", pool.pair |> Pair.toAsset |> Token.encode )
            , ( "collateral", pool.pair |> Pair.toAsset |> Token.encode )
            , ( "maturity", pool.maturity |> Maturity.encode )
            , ( "bondTo", to |> Address.encode )
            , ( "insuranceTo", to |> Address.encode )
            , ( "assetIn", assetIn |> Uint.encode )
            , ( "bondOut", bondOut |> Uint.encode )
            , ( "minInsurance", minInsurance |> Uint.encode )
            , ( "deadline", deadline |> Encode.int )
            ]
                |> Encode.object

        GivenInsurance { pool, to, assetIn, insuranceOut, minBond, deadline } ->
            [ ( "asset", pool.pair |> Pair.toAsset |> Token.encode )
            , ( "collateral", pool.pair |> Pair.toAsset |> Token.encode )
            , ( "maturity", pool.maturity |> Maturity.encode )
            , ( "bondTo", to |> Address.encode )
            , ( "insuranceTo", to |> Address.encode )
            , ( "assetIn", assetIn |> Uint.encode )
            , ( "insuranceOut", insuranceOut |> Uint.encode )
            , ( "minBond", minBond |> Uint.encode )
            , ( "deadline", deadline |> Encode.int )
            ]
                |> Encode.object


encodeApprove : ERC20 -> Value
encodeApprove erc20 =
    [ ( "erc20", erc20 |> ERC20.encode ) ]
        |> Encode.object


hasAllowance :
    { model | user : Remote userError { user | balances : Remote () Balances, allowances : Remote () Allowances } }
    -> { modal | pool : { pool | pair : Pair }, assetIn : String }
    -> Bool
hasAllowance { user } { pool, assetIn } =
    case user of
        Success { balances, allowances } ->
            case ( balances, allowances ) of
                ( Success successBalances, Success successAllowances ) ->
                    (successBalances
                        |> Balances.hasEnough (pool.pair |> Pair.toAsset) assetIn
                    )
                        && (successAllowances
                                |> Allowances.hasEnough (pool.pair |> Pair.toAsset) assetIn
                           )

                _ ->
                    False

        _ ->
            False


view :
    { msgs
        | approveLend : Value -> msg
        , lend : Value -> msg
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
    -> { modal | pool : Pool, assetIn : String, claimsOut : ClaimsOut, tooltip : Maybe Tooltip }
    -> Element msg
view msgs ({ user } as model) ({ pool } as modal) =
    column
        [ width fill
        , height shrink
        , spacing 12
        ]
        [ case user of
            Success _ ->
                pool.pair
                    |> Pair.toAsset
                    |> (\token ->
                            case token of
                                Token.ETH ->
                                    lendSection msgs model modal

                                Token.ERC20 erc20 ->
                                    row
                                        [ width fill
                                        , height shrink
                                        , spacing 20
                                        ]
                                        [ el
                                            [ width fill
                                            , height shrink
                                            ]
                                            (approveSection msgs model modal erc20)
                                        , el
                                            [ width fill
                                            , height shrink
                                            ]
                                            (lendSection msgs model modal)
                                        ]
                       )

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
    { msgs | approveLend : Value -> msg }
    ->
        { model
            | device : Device
            , time : Posix
            , user : Remote userError { user | balances : Remote () Balances, allowances : Remote () Allowances }
        }
    -> { modal | pool : Pool, assetIn : String, claimsOut : ClaimsOut }
    -> ERC20
    -> Element msg
approveSection msgs model modal erc20 =
    if
        ClaimsOut.hasTransaction model modal
            && (hasAllowance model modal |> not)
    then
        approveButton msgs model erc20

    else
        disabledApprove model


approveButton :
    { msgs | approveLend : Value -> msg }
    -> { model | device : Device }
    -> ERC20
    -> Element msg
approveButton msgs { device } erc20 =
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
            erc20
                |> encodeApprove
                |> msgs.approveLend
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


lendSection :
    { msgs | lend : Value -> msg }
    ->
        { model
            | device : Device
            , time : Posix
            , deadline : Deadline
            , user : Remote userError { user | address : Address, balances : Remote () Balances, allowances : Remote () Allowances }
        }
    -> { modal | pool : Pool, assetIn : String, claimsOut : ClaimsOut }
    -> Element msg
lendSection msgs model modal =
    toTransaction model modal
        |> Maybe.map (lendButton msgs model)
        |> Maybe.withDefault (disabledLend model)


lendButton :
    { msgs | lend : Value -> msg }
    -> { model | device : Device }
    -> Transaction
    -> Element msg
lendButton msgs { device } transaction =
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
                |> msgs.lend
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
                (text "Lend")
        }


disabledLend : { model | device : Device } -> Element msg
disabledLend { device } =
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
            (text "Lend")
        )


transactionInfo :
    { msgs | onMouseEnter : Tooltip -> msg, onMouseLeave : msg }
    -> { model | time : Posix, slippage : Slippage, images : Images }
    ->
        { modal
            | pool : Pool
            , assetIn : String
            , claimsOut : ClaimsOut
            , tooltip : Maybe Tooltip
        }
    -> Element msg
transactionInfo msgs ({ slippage, images } as model) ({ pool, claimsOut, tooltip } as modal) =
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
        , (if ClaimsOut.hasTransactionInfo model modal then
            case tooltip of
                Just Tooltip.TransactionInfo ->
                    (case claimsOut of
                        ClaimsOut.Default (Success { minBond, minInsurance }) ->
                            Just ( minBond, minInsurance )

                        ClaimsOut.Slider { claims } ->
                            case claims of
                                Success { minBond, minInsurance } ->
                                    Just ( minBond, minInsurance )

                                _ ->
                                    Nothing

                        ClaimsOut.Bond { bond, claims } ->
                            case claims of
                                Success { minInsurance } ->
                                    Just ( bond, minInsurance )

                                _ ->
                                    Nothing

                        ClaimsOut.Insurance { insurance, claims } ->
                            case claims of
                                Success { minBond } ->
                                    Just ( minBond, insurance )

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
        (if ClaimsOut.hasTransactionInfo model modal then
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
