port module Page.Transaction.Liquidity.Add.Main exposing
    ( Effect(..)
    , Msg
    , Transaction
    , fromAddError
    , init
    , subscriptions
    , toAddError
    , update
    )

import Blockchain.Main as Blockchain exposing (Blockchain)
import Blockchain.User.Main as User exposing (User)
import Data.CDP as CDP exposing (CDP)
import Data.Chains exposing (Chains)
import Data.Deadline exposing (Deadline)
import Data.Images exposing (Images)
import Data.Or exposing (Or(..))
import Data.Pair as Pair
import Data.Pool exposing (Pool)
import Data.Remote as Remote exposing (Remote(..))
import Data.Slippage exposing (Slippage)
import Data.Token as Token exposing (Token)
import Data.Uint as Uint exposing (Uint)
import Element
    exposing
        ( Element
        , alignRight
        , alpha
        , centerY
        , column
        , el
        , fill
        , height
        , none
        , padding
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
import Json.Decode as Decode
import Json.Encode exposing (Value)
import Page.Approve as Approve
import Page.Transaction.Info as Info
import Page.Transaction.Liquidity.Add.Answer as Answer
import Page.Transaction.Liquidity.Add.Disabled as Disabled
import Page.Transaction.Liquidity.Add.Error exposing (Error)
import Page.Transaction.Liquidity.Add.Query as Query
import Page.Transaction.Liquidity.Add.Tooltip as Tooltip exposing (Tooltip)
import Page.Transaction.MaxButton as MaxButton
import Page.Transaction.PoolInfo exposing (PoolInfo)
import Page.Transaction.Textbox as Textbox
import Time exposing (Posix)
import Utility.Color as Color
import Utility.Input as Input
import Utility.Truncate as Truncate


type Transaction
    = Transaction
        { state : State
        , tooltip : Maybe Tooltip
        }


type State
    = Asset AssetInput
    | Debt DebtInput
    | Collateral CollateralInput


type alias AssetInput =
    { assetIn : String
    , out : Remote Error OutGivenAsset
    }


type alias DebtInput =
    { debtOut : String
    , out : Remote Error OutGivenDebt
    }


type alias CollateralInput =
    { collateralOut : String
    , out : Remote Error OutGivenCollateral
    }


type alias OutGivenAsset =
    { debtOut : Uint
    , collateralOut : Uint
    , liquidityOut : Uint
    , maxDebt : Uint
    , maxCollateral : Uint
    , minLiquidity : Uint
    , apr : Float
    , cdp : CDP
    }


type alias OutGivenDebt =
    { assetIn : Uint
    , collateralOut : Uint
    , liquidityOut : Uint
    , maxDebt : Uint
    , maxCollateral : Uint
    , minLiquidity : Uint
    , apr : Float
    , cdp : CDP
    }


type alias OutGivenCollateral =
    { assetIn : Uint
    , debtOut : Uint
    , liquidityOut : Uint
    , maxAsset : Uint
    , maxDebt : Uint
    , minLiquidity : Uint
    , apr : Float
    , cdp : CDP
    }


type Msg
    = ClickAssetIn
    | InputAssetIn String
    | InputMaxAsset
    | ClickDebtOut
    | InputDebtOut String
    | ClickCollateralOut
    | InputCollateralOut String
    | InputMaxCollateral
    | QueryAgain Posix
    | ClickConnect
    | ClickApproveAsset
    | ClickApproveCollateral
    | ReceiveAnswer Value
    | OnMouseEnter Tooltip
    | OnMouseLeave


type Effect
    = OpenConnect
    | OpenConfirm


init : Transaction
init =
    { state =
        { assetIn = ""
        , out =
            initGivenAsset
                |> Success
        }
            |> Asset
    , tooltip = Nothing
    }
        |> Transaction


initGivenAsset : OutGivenAsset
initGivenAsset =
    { debtOut = Uint.zero
    , collateralOut = Uint.zero
    , liquidityOut = Uint.zero
    , maxDebt = Uint.zero
    , maxCollateral = Uint.zero
    , minLiquidity = Uint.zero
    , apr = 0
    , cdp = CDP.init
    }


initGivenDebt : OutGivenDebt
initGivenDebt =
    { assetIn = Uint.zero
    , collateralOut = Uint.zero
    , liquidityOut = Uint.zero
    , maxDebt = Uint.zero
    , maxCollateral = Uint.zero
    , minLiquidity = Uint.zero
    , apr = 0
    , cdp = CDP.init
    }


initGivenCollateral : OutGivenCollateral
initGivenCollateral =
    { assetIn = Uint.zero
    , debtOut = Uint.zero
    , liquidityOut = Uint.zero
    , maxAsset = Uint.zero
    , maxDebt = Uint.zero
    , minLiquidity = Uint.zero
    , apr = 0
    , cdp = CDP.init
    }


fromAddError : Disabled.Transaction -> Transaction
fromAddError transaction =
    { state =
        case transaction of
            Disabled.Asset assetIn ->
                { assetIn = assetIn
                , out = Loading
                }
                    |> Asset

            Disabled.Debt debtOut ->
                { debtOut = debtOut
                , out = Loading
                }
                    |> Debt

            Disabled.Collateral collateralOut ->
                { collateralOut = collateralOut
                , out = Loading
                }
                    |> Collateral
    , tooltip = Nothing
    }
        |> Transaction


toAddError : Transaction -> Disabled.Transaction
toAddError (Transaction { state }) =
    case state of
        Asset { assetIn } ->
            Disabled.Asset assetIn

        Debt { debtOut } ->
            Disabled.Debt debtOut

        Collateral { collateralOut } ->
            Disabled.Collateral collateralOut


update :
    { model
        | time : Posix
        , slippage : Slippage
        , deadline : Deadline
    }
    -> Blockchain
    -> Pool
    -> PoolInfo
    -> Msg
    -> Transaction
    -> ( Transaction, Cmd Msg, Maybe Effect )
update model blockchain pool poolInfo msg (Transaction transaction) =
    case msg of
        ClickAssetIn ->
            (case transaction.state of
                Debt { out } ->
                    case out of
                        Success { assetIn } ->
                            assetIn
                                |> Uint.toAmount (pool.pair |> Pair.toAsset)
                                |> Just

                        _ ->
                            "" |> Just

                Collateral { out } ->
                    case out of
                        Success { assetIn } ->
                            assetIn
                                |> Uint.toAmount (pool.pair |> Pair.toAsset)
                                |> Just

                        _ ->
                            "" |> Just

                _ ->
                    Nothing
            )
                |> Maybe.map
                    (\assetIn ->
                        { transaction | state = assetIn |> updateGivenAssetIn }
                            |> query model blockchain pool poolInfo
                    )
                |> Maybe.withDefault (transaction |> noCmdAndEffect)

        InputAssetIn assetIn ->
            if assetIn |> Uint.isAmount (pool.pair |> Pair.toAsset) then
                { transaction | state = assetIn |> updateGivenAssetIn }
                    |> query model blockchain pool poolInfo

            else
                transaction |> noCmdAndEffect

        InputMaxAsset ->
            blockchain
                |> Blockchain.toUser
                |> Maybe.andThen
                    (\user ->
                        user
                            |> User.getBalance
                                (pool.pair |> Pair.toAsset)
                            |> Maybe.map
                                (Uint.toAmount
                                    (pool.pair |> Pair.toAsset)
                                )
                    )
                |> Maybe.map
                    (\assetIn ->
                        { transaction | state = assetIn |> updateGivenAssetIn }
                            |> query model blockchain pool poolInfo
                    )
                |> Maybe.withDefault (transaction |> noCmdAndEffect)

        ClickDebtOut ->
            (case transaction.state of
                Asset { out } ->
                    case out of
                        Success { debtOut } ->
                            debtOut
                                |> Uint.toAmount (pool.pair |> Pair.toAsset)
                                |> Just

                        _ ->
                            "" |> Just

                Collateral { out } ->
                    case out of
                        Success { debtOut } ->
                            debtOut
                                |> Uint.toAmount (pool.pair |> Pair.toAsset)
                                |> Just

                        _ ->
                            "" |> Just

                _ ->
                    Nothing
            )
                |> Maybe.map
                    (\debtOut ->
                        { transaction | state = debtOut |> updateGivenDebtOut }
                            |> query model blockchain pool poolInfo
                    )
                |> Maybe.withDefault (transaction |> noCmdAndEffect)

        InputDebtOut debtOut ->
            if debtOut |> Uint.isAmount (pool.pair |> Pair.toAsset) then
                { transaction | state = debtOut |> updateGivenDebtOut }
                    |> query model blockchain pool poolInfo

            else
                transaction |> noCmdAndEffect

        ClickCollateralOut ->
            (case transaction.state of
                Asset { out } ->
                    case out of
                        Success { collateralOut } ->
                            collateralOut
                                |> Uint.toAmount (pool.pair |> Pair.toCollateral)
                                |> Just

                        _ ->
                            "" |> Just

                Debt { out } ->
                    case out of
                        Success { collateralOut } ->
                            collateralOut
                                |> Uint.toAmount (pool.pair |> Pair.toCollateral)
                                |> Just

                        _ ->
                            "" |> Just

                _ ->
                    Nothing
            )
                |> Maybe.map
                    (\collateralOut ->
                        { transaction | state = collateralOut |> updateGivenCollateralOut }
                            |> query model blockchain pool poolInfo
                    )
                |> Maybe.withDefault (transaction |> noCmdAndEffect)

        InputCollateralOut collateralOut ->
            if collateralOut |> Uint.isAmount (pool.pair |> Pair.toCollateral) then
                { transaction | state = collateralOut |> updateGivenCollateralOut }
                    |> query model blockchain pool poolInfo

            else
                transaction |> noCmdAndEffect

        InputMaxCollateral ->
            blockchain
                |> Blockchain.toUser
                |> Maybe.andThen
                    (\user ->
                        user
                            |> User.getBalance
                                (pool.pair |> Pair.toCollateral)
                            |> Maybe.map
                                (Uint.toAmount
                                    (pool.pair |> Pair.toCollateral)
                                )
                    )
                |> Maybe.map
                    (\collateralOut ->
                        { transaction | state = collateralOut |> updateGivenCollateralOut }
                            |> query model blockchain pool poolInfo
                    )
                |> Maybe.withDefault (transaction |> noCmdAndEffect)

        QueryAgain _ ->
            transaction
                |> queryPerSecond model blockchain pool poolInfo

        ClickConnect ->
            blockchain
                |> Blockchain.toUser
                |> Maybe.map (\_ -> transaction |> noCmdAndEffect)
                |> Maybe.withDefault
                    ( transaction |> Transaction
                    , Cmd.none
                    , OpenConnect |> Just
                    )

        ClickApproveAsset ->
            (case
                ( blockchain |> Blockchain.toUser
                , case transaction.state of
                    Asset { assetIn } ->
                        assetIn
                            |> Uint.fromAmount
                                (pool.pair |> Pair.toAsset)

                    Debt { out } ->
                        case out of
                            Success { assetIn } ->
                                assetIn |> Just

                            _ ->
                                Nothing

                    Collateral { out } ->
                        case out of
                            Success { assetIn } ->
                                assetIn |> Just

                            _ ->
                                Nothing
                , pool.pair
                    |> Pair.toAsset
                    |> Token.toERC20
                )
             of
                ( Just user, Just assetIn, Just erc20 ) ->
                    if
                        (user
                            |> User.hasEnoughBalance
                                (pool.pair |> Pair.toAsset)
                                assetIn
                        )
                            && (user
                                    |> User.hasEnoughAllowance
                                        erc20
                                        assetIn
                                    |> not
                               )
                    then
                        ( transaction |> Transaction
                        , erc20
                            |> Approve.encode blockchain user
                            |> approveLiquidity
                        , OpenConfirm |> Just
                        )
                            |> Just

                    else
                        Nothing

                _ ->
                    Nothing
            )
                |> Maybe.withDefault (transaction |> noCmdAndEffect)

        ClickApproveCollateral ->
            (case
                ( blockchain |> Blockchain.toUser
                , case transaction.state of
                    Asset { out } ->
                        case out of
                            Success { collateralOut } ->
                                collateralOut |> Just

                            _ ->
                                Nothing

                    Debt { out } ->
                        case out of
                            Success { collateralOut } ->
                                collateralOut |> Just

                            _ ->
                                Nothing

                    Collateral { collateralOut } ->
                        collateralOut
                            |> Uint.fromAmount
                                (pool.pair |> Pair.toCollateral)
                , pool.pair
                    |> Pair.toCollateral
                    |> Token.toERC20
                )
             of
                ( Just user, Just collateralOut, Just erc20 ) ->
                    if
                        (user
                            |> User.hasEnoughBalance
                                (pool.pair |> Pair.toCollateral)
                                collateralOut
                        )
                            && (user
                                    |> User.hasEnoughAllowance
                                        erc20
                                        collateralOut
                                    |> not
                               )
                    then
                        ( transaction |> Transaction
                        , erc20
                            |> Approve.encode blockchain user
                            |> approveLiquidity
                        , OpenConfirm |> Just
                        )
                            |> Just

                    else
                        Nothing

                _ ->
                    Nothing
            )
                |> Maybe.withDefault (transaction |> noCmdAndEffect)

        ReceiveAnswer value ->
            (case
                ( value
                    |> Decode.decodeValue Answer.decoder
                , transaction.state
                )
             of
                ( Ok (Answer.GivenAsset answer), Asset asset ) ->
                    if
                        (answer.chainId == (blockchain |> Blockchain.toChain))
                            && (answer.pool == pool)
                            && (answer.poolInfo == poolInfo)
                            && (Just answer.assetIn
                                    == (asset.assetIn
                                            |> Uint.fromAmount
                                                (pool.pair |> Pair.toAsset)
                                       )
                               )
                            && (answer.slippage == model.slippage)
                    then
                        { transaction
                            | state =
                                { asset | out = answer.result |> toRemote }
                                    |> Asset
                        }
                            |> Just

                    else
                        Nothing

                ( Ok (Answer.GivenDebt answer), Debt debt ) ->
                    if
                        (answer.chainId == (blockchain |> Blockchain.toChain))
                            && (answer.pool == pool)
                            && (answer.poolInfo == poolInfo)
                            && (Just answer.debtOut
                                    == (debt.debtOut
                                            |> Uint.fromAmount
                                                (pool.pair |> Pair.toAsset)
                                       )
                               )
                            && (answer.slippage == model.slippage)
                    then
                        { transaction
                            | state =
                                { debt | out = answer.result |> toRemote }
                                    |> Debt
                        }
                            |> Just

                    else
                        Nothing

                ( Ok (Answer.GivenCollateral answer), Collateral collateral ) ->
                    if
                        (answer.chainId == (blockchain |> Blockchain.toChain))
                            && (answer.pool == pool)
                            && (answer.poolInfo == poolInfo)
                            && (Just answer.collateralOut
                                    == (collateral.collateralOut
                                            |> Uint.fromAmount
                                                (pool.pair |> Pair.toCollateral)
                                       )
                               )
                            && (answer.slippage == model.slippage)
                    then
                        { transaction
                            | state =
                                { collateral | out = answer.result |> toRemote }
                                    |> Collateral
                        }
                            |> Just

                    else
                        Nothing

                _ ->
                    Nothing
            )
                |> Maybe.map noCmdAndEffect
                |> Maybe.withDefault (transaction |> noCmdAndEffect)

        OnMouseEnter tooltip ->
            { transaction | tooltip = Just tooltip }
                |> noCmdAndEffect

        OnMouseLeave ->
            { transaction | tooltip = Nothing }
                |> noCmdAndEffect


updateGivenAssetIn : String -> State
updateGivenAssetIn assetIn =
    { assetIn = assetIn
    , out =
        if assetIn |> Input.isZero then
            initGivenAsset
                |> Success

        else
            Loading
    }
        |> Asset


updateGivenDebtOut : String -> State
updateGivenDebtOut debtOut =
    { debtOut = debtOut
    , out =
        if debtOut |> Input.isZero then
            initGivenDebt
                |> Success

        else
            Loading
    }
        |> Debt


updateGivenCollateralOut : String -> State
updateGivenCollateralOut collateralOut =
    { collateralOut = collateralOut
    , out =
        if collateralOut |> Input.isZero then
            initGivenCollateral
                |> Success

        else
            Loading
    }
        |> Collateral


toRemote :
    Result error answer
    -> Remote error answer
toRemote result =
    case result of
        Ok out ->
            Success out

        Err error ->
            Failure error


noCmdAndEffect :
    { state : State
    , tooltip : Maybe Tooltip
    }
    -> ( Transaction, Cmd Msg, Maybe Effect )
noCmdAndEffect transaction =
    ( transaction |> Transaction
    , Cmd.none
    , Nothing
    )


query :
    { model | slippage : Slippage }
    -> Blockchain
    -> Pool
    -> PoolInfo
    ->
        { state : State
        , tooltip : Maybe Tooltip
        }
    -> ( Transaction, Cmd Msg, Maybe Effect )
query =
    constructQuery queryLiquidity


queryPerSecond :
    { model | slippage : Slippage }
    -> Blockchain
    -> Pool
    -> PoolInfo
    ->
        { state : State
        , tooltip : Maybe Tooltip
        }
    -> ( Transaction, Cmd Msg, Maybe Effect )
queryPerSecond =
    constructQuery queryLiquidityPerSecond


constructQuery :
    (Value -> Cmd Msg)
    -> { model | slippage : Slippage }
    -> Blockchain
    -> Pool
    -> PoolInfo
    ->
        { state : State
        , tooltip : Maybe Tooltip
        }
    -> ( Transaction, Cmd Msg, Maybe Effect )
constructQuery givenCmd { slippage } blockchain pool poolInfo transaction =
    (case transaction.state of
        Asset asset ->
            case
                ( asset.assetIn |> Input.isZero
                , asset.assetIn
                    |> Uint.fromAmount
                        (pool.pair |> Pair.toAsset)
                )
            of
                ( True, _ ) ->
                    Nothing

                ( False, Just assetIn ) ->
                    { chainId = blockchain |> Blockchain.toChain
                    , pool = pool
                    , poolInfo = poolInfo
                    , assetIn = assetIn
                    , slippage = slippage
                    }
                        |> Query.givenAsset
                        |> Just

                _ ->
                    Nothing

        Debt debt ->
            case
                ( debt.debtOut |> Input.isZero
                , debt.debtOut
                    |> Uint.fromAmount
                        (pool.pair |> Pair.toAsset)
                )
            of
                ( True, _ ) ->
                    Nothing

                ( False, Just debtOut ) ->
                    { chainId = blockchain |> Blockchain.toChain
                    , pool = pool
                    , poolInfo = poolInfo
                    , debtOut = debtOut
                    , slippage = slippage
                    }
                        |> Query.givenDebt
                        |> Just

                _ ->
                    Nothing

        Collateral collateral ->
            case
                ( collateral.collateralOut |> Input.isZero
                , collateral.collateralOut
                    |> Uint.fromAmount
                        (pool.pair |> Pair.toCollateral)
                )
            of
                ( True, _ ) ->
                    Nothing

                ( False, Just collateralOut ) ->
                    { chainId = blockchain |> Blockchain.toChain
                    , pool = pool
                    , poolInfo = poolInfo
                    , collateralOut = collateralOut
                    , slippage = slippage
                    }
                        |> Query.givenCollateral
                        |> Just

                _ ->
                    Nothing
    )
        |> Maybe.map givenCmd
        |> Maybe.withDefault Cmd.none
        |> (\cmd ->
                ( transaction |> Transaction
                , cmd
                , Nothing
                )
           )


port queryLiquidity : Value -> Cmd msg


port queryLiquidityPerSecond : Value -> Cmd msg


port approveLiquidity : Value -> Cmd msg


port receiveAddAnswer : (Value -> msg) -> Sub msg


subscriptions : Transaction -> Sub Msg
subscriptions (Transaction { state }) =
    if state |> hasInputZero then
        Sub.none

    else
        [ Time.every 1000 QueryAgain
        , receiveAddAnswer ReceiveAnswer
        ]
            |> Sub.batch


hasInputZero : State -> Bool
hasInputZero state =
    case state of
        Asset { assetIn } ->
            assetIn |> Input.isZero

        Debt { debtOut } ->
            debtOut |> Input.isZero

        Collateral { collateralOut } ->
            collateralOut |> Input.isZero


view :
    { model | images : Images }
    -> Blockchain
    -> Pool
    -> Transaction
    ->
        { first : Element Msg
        , second : Element Msg
        , buttons : Element Msg
        }
view model blockchain pool (Transaction transaction) =
    { first =
        transaction
            |> viewAssetIn model
                blockchain
                (pool.pair |> Pair.toAsset)
    , second =
        el
            [ width <| px 335
            , height <| px 392
            , Background.color Color.light500
            , Border.rounded 8
            ]
            none
    , buttons = none
    }


viewAssetIn :
    { model | images : Images }
    -> Blockchain
    -> Token
    -> { transaction | state : State, tooltip : Maybe Tooltip }
    -> Element Msg
viewAssetIn model blockchain asset { state, tooltip } =
    column
        [ Region.description "lend asset"
        , width <| px 343
        , height shrink
        , padding 16
        , spacing 10
        , Background.color Color.light500
        , Border.rounded 8
        ]
        [ row
            [ width fill
            , height shrink
            , spacing 6
            , centerY
            ]
            [ el
                [ width shrink
                , height shrink
                , Font.size 14
                ]
                (text "Amount to Lend")
            , blockchain
                |> Blockchain.toUser
                |> Maybe.andThen (User.getBalance asset)
                |> Maybe.map
                    (\balance ->
                        MaxButton.view
                            { onPress = InputMaxAsset
                            , onMouseEnter = OnMouseEnter
                            , onMouseLeave = OnMouseLeave
                            , tooltip = Tooltip.AssetBalance
                            , opened = tooltip
                            , token = asset
                            , balance = balance
                            }
                    )
                |> Maybe.withDefault none
            ]
        , Textbox.view model
            { onMouseEnter = OnMouseEnter
            , onMouseLeave = OnMouseLeave
            , tooltip = Tooltip.AssetInSymbol
            , opened = tooltip
            , token = asset
            , onClick = Just ClickAssetIn
            , onChange = InputAssetIn
            , text =
                case state of
                    Asset { assetIn } ->
                        Left assetIn

                    Debt { out } ->
                        case out |> Remote.map .assetIn of
                            Success uint ->
                                Right uint

                            _ ->
                                Left ""

                    Collateral { out } ->
                        case out |> Remote.map .assetIn of
                            Success uint ->
                                Right uint

                            _ ->
                                Left ""
            , description = "asset in textbox"
            }
        ]


userBalance :
    User
    -> Token
    -> { transaction | tooltip : Maybe Tooltip }
    -> Element Msg
userBalance user asset { tooltip } =
    user
        |> User.getBalance asset
        |> Maybe.map
            (\balance ->
                row
                    [ width shrink
                    , height shrink
                    , alignRight
                    , centerY
                    ]
                    [ el
                        [ width shrink
                        , height shrink
                        , Font.size 12
                        , paddingXY 0 2
                        , Font.color Color.transparent300
                        ]
                        (text "Bal: ")
                    , Truncate.viewBalance
                        { onMouseEnter = OnMouseEnter
                        , onMouseLeave = OnMouseLeave
                        , tooltip = Tooltip.AssetBalance
                        , opened = tooltip
                        , token = asset
                        , balance = balance
                        }
                    ]
            )
        |> Maybe.withDefault none


maxButton : Element Msg
maxButton =
    Input.button
        [ Region.description "max asset lend"
        , width shrink
        , height shrink
        , alignRight
        , centerY
        , Font.size 12
        , paddingXY 0 2
        , Font.color Color.warning400
        , Font.bold
        ]
        { onPress = Just InputMaxAsset
        , label = text "MAX"
        }


disabled :
    { model | images : Images }
    -> Blockchain
    -> Pool
    -> Transaction
    ->
        { first : Element Never
        , second : Element Never
        }
disabled model blockchain pool (Transaction transaction) =
    { first =
        transaction
            |> disabledAssetIn
                model
                blockchain
                (pool.pair |> Pair.toAsset)
    , second =
        transaction
            |> disabledDues model pool
    }


disabledAssetIn :
    { model | images : Images }
    -> Blockchain
    -> Token
    -> { transaction | state : State }
    -> Element Never
disabledAssetIn model blockchain asset transaction =
    column
        [ Region.description "lend asset"
        , width <| px 343
        , height shrink
        , padding 16
        , spacing 10
        , alpha 0.2
        , Background.color Color.primary100
        , Border.rounded 8
        ]
        [ row
            [ width fill
            , height shrink
            , spacing 6
            , centerY
            ]
            (el
                [ width shrink
                , height shrink
                , Font.size 14
                , paddingXY 0 3
                , Font.color Color.primary400
                ]
                (text "Amount to Lend")
                :: (blockchain
                        |> Blockchain.toUser
                        |> Maybe.map
                            (\user ->
                                [ disabledUserBalance user asset
                                , disabledMaxButton
                                ]
                            )
                        |> Maybe.withDefault
                            []
                   )
            )
        , Textbox.disabled model
            { token = asset
            , text =
                case transaction.state of
                    Asset { assetIn } ->
                        assetIn

                    _ ->
                        ""
            , description = "lend asset textbox"
            }
        ]


disabledUserBalance :
    User
    -> Token
    -> Element Never
disabledUserBalance user asset =
    user
        |> User.getBalance asset
        |> Maybe.map
            (\balance ->
                row
                    [ width shrink
                    , height shrink
                    , alignRight
                    , centerY
                    ]
                    [ el
                        [ width shrink
                        , height shrink
                        , Font.size 12
                        , paddingXY 0 2
                        , Font.color Color.transparent300
                        ]
                        (text "Bal: ")
                    , Truncate.disabledBalance
                        { token = asset
                        , balance = balance
                        }
                    ]
            )
        |> Maybe.withDefault none


disabledMaxButton : Element Never
disabledMaxButton =
    el
        [ Region.description "max asset lend"
        , width shrink
        , height shrink
        , alignRight
        , centerY
        , Font.size 12
        , paddingXY 0 2
        , Font.color Color.warning400
        , Font.bold
        ]
        (text "MAX")


disabledDues :
    { model | images : Images }
    -> Pool
    -> { transaction | state : State }
    -> Element Never
disabledDues model pool transaction =
    column
        [ Region.description "dues"
        , width <| px 343
        , height shrink
        , padding 16
        , spacing 12
        , alpha 0.2
        , Background.color Color.primary100
        , Border.rounded 8
        ]
        []


empty :
    { model | images : Images }
    ->
        { asset : Maybe Token
        , collateral : Maybe Token
        }
    ->
        { first : Element Never
        , second : Element Never
        }
empty model { asset, collateral } =
    { first = emptyAssetIn model asset
    , second = emptyDues model asset collateral
    }


emptyAssetIn :
    { model | images : Images }
    -> Maybe Token
    -> Element Never
emptyAssetIn model token =
    column
        [ Region.description "lend asset"
        , width <| px 343
        , height shrink
        , padding 16
        , spacing 10
        , alpha 0.2
        , Background.color Color.primary100
        , Border.rounded 8
        ]
        [ el
            [ width shrink
            , height shrink
            , Font.size 14
            , paddingXY 0 3
            , Font.color Color.primary400
            ]
            (text "Amount to Lend")
        , token
            |> Maybe.map
                (\asset ->
                    Textbox.disabled model
                        { token = asset
                        , text = ""
                        , description = "asset in textbox"
                        }
                )
            |> Maybe.withDefault
                (Textbox.empty "asset in textbox")
        ]


emptyDues :
    { model | images : Images }
    -> Maybe Token
    -> Maybe Token
    -> Element Never
emptyDues model asset collateral =
    column
        [ Region.description "dues"
        , width <| px 343
        , height shrink
        , padding 16
        , spacing 12
        , alpha 0.2
        , Background.color Color.primary100
        , Border.rounded 8
        ]
        [ row
            [ width fill
            , height shrink
            , spacing 16
            ]
            [ Info.emptyAPR
            , Info.emptyCDP
            ]
        , emptyDuesIn model asset collateral
        ]


emptyDuesIn :
    { model | images : Images }
    -> Maybe Token
    -> Maybe Token
    -> Element Never
emptyDuesIn model asset collateral =
    column
        [ width fill
        , height shrink
        , padding 12
        , spacing 12
        , Background.color Color.primary100
        , Border.rounded 8
        ]
        [ emptyDebtOut model asset
        , emptyCollateralOut model collateral
        ]


emptyDebtOut :
    { model | images : Images }
    -> Maybe Token
    -> Element Never
emptyDebtOut model asset =
    column
        [ width fill
        , height shrink
        , spacing 10
        ]
        [ el
            [ width shrink
            , height shrink
            , Font.size 14
            , Font.color Color.primary400
            ]
            (text "Debt to Repay")
        , asset
            |> Maybe.map
                (\token ->
                    Textbox.disabled model
                        { token = token
                        , text = ""
                        , description = "debt output"
                        }
                )
            |> Maybe.withDefault
                (Textbox.empty "debt output")
        ]


emptyCollateralOut :
    { model | images : Images }
    -> Maybe Token
    -> Element Never
emptyCollateralOut model collateral =
    column
        [ width fill
        , height shrink
        , spacing 10
        ]
        [ el
            [ width shrink
            , height shrink
            , Font.size 14
            , Font.color Color.primary400
            ]
            (text "Collateral to Lock")
        , collateral
            |> Maybe.map
                (\token ->
                    Textbox.disabled model
                        { token = token
                        , text = ""
                        , description = "collateral output"
                        }
                )
            |> Maybe.withDefault
                (Textbox.empty "collateral output")
        ]
