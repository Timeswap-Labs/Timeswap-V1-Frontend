port module Page.Transaction.Liquidity.Add.Main exposing
    ( Effect(..)
    , Msg
    , Transaction
    , fromDisabled
    , init
    , subscriptions
    , toDisabled
    , update
    , view
    )

import Blockchain.Main as Blockchain exposing (Blockchain)
import Blockchain.User.Main as User
import Data.CDP as CDP exposing (CDP)
import Data.Deadline exposing (Deadline)
import Data.Images exposing (Images)
import Data.Or exposing (Or(..))
import Data.Pair as Pair
import Data.Pool exposing (Pool)
import Data.PriceFeed exposing (PriceFeed)
import Data.Remote as Remote exposing (Remote(..))
import Data.Slippage exposing (Slippage)
import Data.Token as Token exposing (Token)
import Data.Uint as Uint exposing (Uint)
import Element
    exposing
        ( Element
        , centerY
        , column
        , el
        , fill
        , height
        , map
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
import Page.Transaction.Button as Button
import Page.Transaction.Info as Info
import Page.Transaction.Liquidity.Add.Disabled as Disabled
import Page.Transaction.Liquidity.Add.Error exposing (Error)
import Page.Transaction.Liquidity.Add.Query as Query
import Page.Transaction.Liquidity.Add.Tooltip as Tooltip exposing (Tooltip)
import Page.Transaction.Liquidity.Add.Write as Write
import Page.Transaction.MaxButton as MaxButton
import Page.Transaction.Output as Output
import Page.Transaction.PoolInfo exposing (PoolInfo)
import Page.Transaction.Textbox as Textbox
import Time exposing (Posix)
import Utility.Color as Color
import Utility.Input as Input


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
    { debtIn : String
    , out : Remote Error OutGivenDebt
    }


type alias CollateralInput =
    { collateralIn : String
    , out : Remote Error OutGivenCollateral
    }


type alias OutGivenAsset =
    { debtIn : Uint
    , collateralIn : Uint
    , liquidityOut : Uint
    , maxDebt : Uint
    , maxCollateral : Uint
    , minLiquidity : Uint
    , apr : Float
    , cdp : CDP
    }


type alias OutGivenDebt =
    { assetIn : Uint
    , collateralIn : Uint
    , liquidityOut : Uint
    , maxAsset : Uint
    , maxCollateral : Uint
    , minLiquidity : Uint
    , apr : Float
    , cdp : CDP
    }


type alias OutGivenCollateral =
    { assetIn : Uint
    , debtIn : Uint
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
    | ClickDebtIn
    | InputDebtIn String
    | ClickCollateralIn
    | InputCollateralIn String
    | InputMaxCollateral
    | QueryAgain Posix
    | ClickConnect
    | ClickApproveAsset
    | ClickApproveCollateral
    | ClickLiquidity
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
    { debtIn = Uint.zero
    , collateralIn = Uint.zero
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
    , collateralIn = Uint.zero
    , liquidityOut = Uint.zero
    , maxAsset = Uint.zero
    , maxCollateral = Uint.zero
    , minLiquidity = Uint.zero
    , apr = 0
    , cdp = CDP.init
    }


initGivenCollateral : OutGivenCollateral
initGivenCollateral =
    { assetIn = Uint.zero
    , debtIn = Uint.zero
    , liquidityOut = Uint.zero
    , maxAsset = Uint.zero
    , maxDebt = Uint.zero
    , minLiquidity = Uint.zero
    , apr = 0
    , cdp = CDP.init
    }


fromDisabled :
    { model | slippage : Slippage }
    -> Blockchain
    -> Pool
    -> PoolInfo
    -> Disabled.Transaction
    -> ( Transaction, Cmd Msg )
fromDisabled model blockchain pool poolInfo transaction =
    (case transaction of
        Disabled.Asset assetIn ->
            if assetIn |> Input.isZero then
                { assetIn = assetIn
                , out = initGivenAsset |> Success
                }
                    |> Asset
                    |> Left

            else
                { assetIn = assetIn
                , out = Loading
                }
                    |> Asset
                    |> Right

        Disabled.Debt debtIn ->
            if debtIn |> Input.isZero then
                { debtIn = debtIn
                , out = initGivenDebt |> Success
                }
                    |> Debt
                    |> Left

            else
                { debtIn = debtIn
                , out = Loading
                }
                    |> Debt
                    |> Right

        Disabled.Collateral collateralIn ->
            if collateralIn |> Input.isZero then
                { collateralIn = collateralIn
                , out = initGivenCollateral |> Success
                }
                    |> Collateral
                    |> Left

            else
                { collateralIn = collateralIn
                , out = Loading
                }
                    |> Collateral
                    |> Right
    )
        |> (\or ->
                case or of
                    Left state ->
                        { state = state
                        , tooltip = Nothing
                        }
                            |> noCmd

                    Right state ->
                        { state = state
                        , tooltip = Nothing
                        }
                            |> initQuery model blockchain pool poolInfo
           )


toDisabled : Transaction -> Disabled.Transaction
toDisabled (Transaction { state }) =
    case state of
        Asset { assetIn } ->
            Disabled.Asset assetIn

        Debt { debtIn } ->
            Disabled.Debt debtIn

        Collateral { collateralIn } ->
            Disabled.Collateral collateralIn


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
    case ( msg, transaction.state ) of
        ( ClickAssetIn, Debt { debtIn, out } ) ->
            (case out of
                Success { assetIn } ->
                    if debtIn |> Input.isZero then
                        ""

                    else
                        assetIn
                            |> Uint.toAmount (pool.pair |> Pair.toAsset)

                _ ->
                    ""
            )
                |> (\assetIn ->
                        { transaction | state = assetIn |> updateGivenAssetIn }
                            |> query model blockchain pool poolInfo
                   )

        ( ClickAssetIn, Collateral { collateralIn, out } ) ->
            (case out of
                Success { assetIn } ->
                    if collateralIn |> Input.isZero then
                        ""

                    else
                        assetIn
                            |> Uint.toAmount (pool.pair |> Pair.toAsset)

                _ ->
                    ""
            )
                |> (\assetIn ->
                        { transaction | state = assetIn |> updateGivenAssetIn }
                            |> query model blockchain pool poolInfo
                   )

        ( InputAssetIn assetIn, _ ) ->
            if assetIn |> Uint.isAmount (pool.pair |> Pair.toAsset) then
                { transaction | state = assetIn |> updateGivenAssetIn }
                    |> query model blockchain pool poolInfo

            else
                transaction |> noCmdAndEffect

        ( InputMaxAsset, _ ) ->
            blockchain
                |> Blockchain.toUser
                |> Maybe.andThen
                    (\user ->
                        user
                            |> User.getBalance
                                (pool.pair |> Pair.toAsset)
                            |> (Maybe.map << Remote.map)
                                (Uint.toAmount
                                    (pool.pair |> Pair.toAsset)
                                )
                            |> (Maybe.map << Remote.withDefault) ""
                    )
                |> Maybe.map
                    (\assetIn ->
                        { transaction | state = assetIn |> updateGivenAssetIn }
                            |> query model blockchain pool poolInfo
                    )
                |> Maybe.withDefault (transaction |> noCmdAndEffect)

        ( ClickDebtIn, Asset { assetIn, out } ) ->
            (case out of
                Success { debtIn } ->
                    if assetIn |> Input.isZero then
                        ""

                    else
                        debtIn
                            |> Uint.toAmount (pool.pair |> Pair.toAsset)

                _ ->
                    ""
            )
                |> (\debtIn ->
                        { transaction | state = debtIn |> updateGivenDebtOut }
                            |> query model blockchain pool poolInfo
                   )

        ( ClickDebtIn, Collateral { collateralIn, out } ) ->
            (case out of
                Success { debtIn } ->
                    if collateralIn |> Input.isZero then
                        ""

                    else
                        debtIn
                            |> Uint.toAmount (pool.pair |> Pair.toAsset)

                _ ->
                    ""
            )
                |> (\debtIn ->
                        { transaction | state = debtIn |> updateGivenDebtOut }
                            |> query model blockchain pool poolInfo
                   )

        ( InputDebtIn debtIn, _ ) ->
            if debtIn |> Uint.isAmount (pool.pair |> Pair.toAsset) then
                { transaction | state = debtIn |> updateGivenDebtOut }
                    |> query model blockchain pool poolInfo

            else
                transaction |> noCmdAndEffect

        ( ClickCollateralIn, Asset { assetIn, out } ) ->
            (case out of
                Success { collateralIn } ->
                    if assetIn |> Input.isZero then
                        ""

                    else
                        collateralIn
                            |> Uint.toAmount (pool.pair |> Pair.toCollateral)

                _ ->
                    ""
            )
                |> (\collateralIn ->
                        { transaction | state = collateralIn |> updateGivenCollateralOut }
                            |> query model blockchain pool poolInfo
                   )

        ( ClickCollateralIn, Debt { debtIn, out } ) ->
            (case out of
                Success { collateralIn } ->
                    if debtIn |> Input.isZero then
                        ""

                    else
                        collateralIn
                            |> Uint.toAmount (pool.pair |> Pair.toCollateral)

                _ ->
                    ""
            )
                |> (\collateralIn ->
                        { transaction | state = collateralIn |> updateGivenCollateralOut }
                            |> query model blockchain pool poolInfo
                   )

        ( InputCollateralIn collateralIn, _ ) ->
            if collateralIn |> Uint.isAmount (pool.pair |> Pair.toCollateral) then
                { transaction | state = collateralIn |> updateGivenCollateralOut }
                    |> query model blockchain pool poolInfo

            else
                transaction |> noCmdAndEffect

        ( InputMaxCollateral, _ ) ->
            blockchain
                |> Blockchain.toUser
                |> Maybe.andThen
                    (\user ->
                        user
                            |> User.getBalance
                                (pool.pair |> Pair.toCollateral)
                            |> (Maybe.map << Remote.map)
                                (Uint.toAmount
                                    (pool.pair |> Pair.toCollateral)
                                )
                            |> (Maybe.map << Remote.withDefault) ""
                    )
                |> Maybe.map
                    (\collateralIn ->
                        { transaction | state = collateralIn |> updateGivenCollateralOut }
                            |> query model blockchain pool poolInfo
                    )
                |> Maybe.withDefault (transaction |> noCmdAndEffect)

        ( QueryAgain _, _ ) ->
            transaction
                |> queryPerSecond model blockchain pool poolInfo

        ( ClickConnect, _ ) ->
            blockchain
                |> Blockchain.toUser
                |> Maybe.map (\_ -> transaction |> noCmdAndEffect)
                |> Maybe.withDefault
                    ( transaction |> Transaction
                    , Cmd.none
                    , OpenConnect |> Just
                    )

        ( ClickApproveAsset, _ ) ->
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

        ( ClickApproveCollateral, _ ) ->
            (case
                ( blockchain |> Blockchain.toUser
                , case transaction.state of
                    Asset { out } ->
                        case out of
                            Success { collateralIn } ->
                                collateralIn |> Just

                            _ ->
                                Nothing

                    Debt { out } ->
                        case out of
                            Success { collateralIn } ->
                                collateralIn |> Just

                            _ ->
                                Nothing

                    Collateral { collateralIn } ->
                        collateralIn
                            |> Uint.fromAmount
                                (pool.pair |> Pair.toCollateral)
                , pool.pair
                    |> Pair.toCollateral
                    |> Token.toERC20
                )
             of
                ( Just user, Just collateralIn, Just erc20 ) ->
                    if
                        (user
                            |> User.hasEnoughBalance
                                (pool.pair |> Pair.toCollateral)
                                collateralIn
                        )
                            && (user
                                    |> User.hasEnoughAllowance
                                        erc20
                                        collateralIn
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

        ( ClickLiquidity, Asset asset ) ->
            (case asset.out of
                Success answer ->
                    case
                        ( blockchain |> Blockchain.toUser
                        , asset.assetIn
                            |> Uint.fromAmount
                                (pool.pair |> Pair.toAsset)
                        )
                    of
                        ( Just user, Just assetIn ) ->
                            if
                                (user
                                    |> User.hasEnoughBalance
                                        (pool.pair |> Pair.toAsset)
                                        assetIn
                                )
                                    && (user
                                            |> User.hasEnoughBalance
                                                (pool.pair |> Pair.toCollateral)
                                                answer.collateralIn
                                       )
                                    && (pool.pair
                                            |> Pair.toAsset
                                            |> Token.toERC20
                                            |> Maybe.map
                                                (\erc20 ->
                                                    user
                                                        |> User.hasEnoughAllowance
                                                            erc20
                                                            assetIn
                                                )
                                            |> Maybe.withDefault True
                                       )
                                    && (pool.pair
                                            |> Pair.toCollateral
                                            |> Token.toERC20
                                            |> Maybe.map
                                                (\erc20 ->
                                                    user
                                                        |> User.hasEnoughAllowance
                                                            erc20
                                                            answer.collateralIn
                                                )
                                            |> Maybe.withDefault True
                                       )
                            then
                                ( transaction |> Transaction
                                , { pool = pool
                                  , assetIn = assetIn
                                  , minLiquidity = answer.minLiquidity
                                  , maxDebt = answer.maxDebt
                                  , maxCollateral = answer.maxCollateral
                                  }
                                    |> Write.GivenAsset
                                    |> Write.encode model blockchain user
                                    |> liquidity
                                , OpenConfirm |> Just
                                )
                                    |> Just

                            else
                                Nothing

                        _ ->
                            Nothing

                _ ->
                    Nothing
            )
                |> Maybe.withDefault (transaction |> noCmdAndEffect)

        ( ClickLiquidity, Debt debt ) ->
            (case debt.out of
                Success answer ->
                    case
                        ( blockchain |> Blockchain.toUser
                        , debt.debtIn
                            |> Uint.fromAmount
                                (pool.pair |> Pair.toAsset)
                        )
                    of
                        ( Just user, Just debtIn ) ->
                            if
                                (user
                                    |> User.hasEnoughBalance
                                        (pool.pair |> Pair.toAsset)
                                        answer.assetIn
                                )
                                    && (user
                                            |> User.hasEnoughBalance
                                                (pool.pair |> Pair.toCollateral)
                                                answer.collateralIn
                                       )
                                    && (pool.pair
                                            |> Pair.toAsset
                                            |> Token.toERC20
                                            |> Maybe.map
                                                (\erc20 ->
                                                    user
                                                        |> User.hasEnoughAllowance
                                                            erc20
                                                            answer.assetIn
                                                )
                                            |> Maybe.withDefault True
                                       )
                                    && (pool.pair
                                            |> Pair.toCollateral
                                            |> Token.toERC20
                                            |> Maybe.map
                                                (\erc20 ->
                                                    user
                                                        |> User.hasEnoughAllowance
                                                            erc20
                                                            answer.collateralIn
                                                )
                                            |> Maybe.withDefault True
                                       )
                            then
                                ( transaction |> Transaction
                                , { pool = pool
                                  , debtIn = debtIn
                                  , minLiquidity = answer.minLiquidity
                                  , maxAsset = answer.maxAsset
                                  , maxCollateral = answer.maxCollateral
                                  }
                                    |> Write.GivenDebt
                                    |> Write.encode model blockchain user
                                    |> liquidity
                                , OpenConfirm |> Just
                                )
                                    |> Just

                            else
                                Nothing

                        _ ->
                            Nothing

                _ ->
                    Nothing
            )
                |> Maybe.withDefault (transaction |> noCmdAndEffect)

        ( ClickLiquidity, Collateral collateral ) ->
            (case collateral.out of
                Success answer ->
                    case
                        ( blockchain |> Blockchain.toUser
                        , collateral.collateralIn
                            |> Uint.fromAmount
                                (pool.pair |> Pair.toCollateral)
                        )
                    of
                        ( Just user, Just collateralIn ) ->
                            if
                                (user
                                    |> User.hasEnoughBalance
                                        (pool.pair |> Pair.toAsset)
                                        answer.assetIn
                                )
                                    && (user
                                            |> User.hasEnoughBalance
                                                (pool.pair |> Pair.toCollateral)
                                                collateralIn
                                       )
                                    && (pool.pair
                                            |> Pair.toAsset
                                            |> Token.toERC20
                                            |> Maybe.map
                                                (\erc20 ->
                                                    user
                                                        |> User.hasEnoughAllowance
                                                            erc20
                                                            answer.assetIn
                                                )
                                            |> Maybe.withDefault True
                                       )
                                    && (pool.pair
                                            |> Pair.toCollateral
                                            |> Token.toERC20
                                            |> Maybe.map
                                                (\erc20 ->
                                                    user
                                                        |> User.hasEnoughAllowance
                                                            erc20
                                                            collateralIn
                                                )
                                            |> Maybe.withDefault True
                                       )
                            then
                                ( transaction |> Transaction
                                , { pool = pool
                                  , collateralIn = collateralIn
                                  , minLiquidity = answer.minLiquidity
                                  , maxAsset = answer.maxAsset
                                  , maxDebt = answer.maxDebt
                                  }
                                    |> Write.GivenCollateral
                                    |> Write.encode model blockchain user
                                    |> liquidity
                                , OpenConfirm |> Just
                                )
                                    |> Just

                            else
                                Nothing

                        _ ->
                            Nothing

                _ ->
                    Nothing
            )
                |> Maybe.withDefault (transaction |> noCmdAndEffect)

        ( ReceiveAnswer value, Asset asset ) ->
            (case value |> Decode.decodeValue Query.decoder of
                Ok (Query.GivenAsset answer) ->
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

                _ ->
                    Nothing
            )
                |> Maybe.map noCmdAndEffect
                |> Maybe.withDefault (transaction |> noCmdAndEffect)

        ( ReceiveAnswer value, Debt debt ) ->
            (case value |> Decode.decodeValue Query.decoder of
                Ok (Query.GivenDebt answer) ->
                    if
                        (answer.chainId == (blockchain |> Blockchain.toChain))
                            && (answer.pool == pool)
                            && (answer.poolInfo == poolInfo)
                            && (Just answer.debtIn
                                    == (debt.debtIn
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

                _ ->
                    Nothing
            )
                |> Maybe.map noCmdAndEffect
                |> Maybe.withDefault (transaction |> noCmdAndEffect)

        ( ReceiveAnswer value, Collateral collateral ) ->
            (case value |> Decode.decodeValue Query.decoder of
                Ok (Query.GivenCollateral answer) ->
                    if
                        (answer.chainId == (blockchain |> Blockchain.toChain))
                            && (answer.pool == pool)
                            && (answer.poolInfo == poolInfo)
                            && (Just answer.collateralIn
                                    == (collateral.collateralIn
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

        ( OnMouseEnter tooltip, _ ) ->
            { transaction | tooltip = Just tooltip }
                |> noCmdAndEffect

        ( OnMouseLeave, _ ) ->
            { transaction | tooltip = Nothing }
                |> noCmdAndEffect

        _ ->
            transaction |> noCmdAndEffect


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
updateGivenDebtOut debtIn =
    { debtIn = debtIn
    , out =
        if debtIn |> Input.isZero then
            initGivenDebt
                |> Success

        else
            Loading
    }
        |> Debt


updateGivenCollateralOut : String -> State
updateGivenCollateralOut collateralIn =
    { collateralIn = collateralIn
    , out =
        if collateralIn |> Input.isZero then
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


noCmd :
    { state : State
    , tooltip : Maybe Tooltip
    }
    -> ( Transaction, Cmd Msg )
noCmd transaction =
    ( transaction |> Transaction
    , Cmd.none
    )


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


initQuery :
    { model | slippage : Slippage }
    -> Blockchain
    -> Pool
    -> PoolInfo
    ->
        { state : State
        , tooltip : Maybe Tooltip
        }
    -> ( Transaction, Cmd Msg )
initQuery =
    constructQuery queryLiquidity


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
query model blockchain pool poolInfo transaction =
    transaction
        |> constructQuery queryLiquidity
            model
            blockchain
            pool
            poolInfo
        |> (\( updated, cmd ) ->
                ( updated
                , cmd
                , Nothing
                )
           )


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
queryPerSecond model blockchain pool poolInfo transaction =
    transaction
        |> constructQuery queryLiquidityPerSecond
            model
            blockchain
            pool
            poolInfo
        |> (\( updated, cmd ) ->
                ( updated
                , cmd
                , Nothing
                )
           )


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
    -> ( Transaction, Cmd Msg )
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
                ( debt.debtIn |> Input.isZero
                , debt.debtIn
                    |> Uint.fromAmount
                        (pool.pair |> Pair.toAsset)
                )
            of
                ( False, Just debtIn ) ->
                    { chainId = blockchain |> Blockchain.toChain
                    , pool = pool
                    , poolInfo = poolInfo
                    , debtIn = debtIn
                    , slippage = slippage
                    }
                        |> Query.givenDebt
                        |> Just

                _ ->
                    Nothing

        Collateral collateral ->
            case
                ( collateral.collateralIn |> Input.isZero
                , collateral.collateralIn
                    |> Uint.fromAmount
                        (pool.pair |> Pair.toCollateral)
                )
            of
                ( False, Just collateralIn ) ->
                    { chainId = blockchain |> Blockchain.toChain
                    , pool = pool
                    , poolInfo = poolInfo
                    , collateralIn = collateralIn
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
                )
           )


port queryLiquidity : Value -> Cmd msg


port queryLiquidityPerSecond : Value -> Cmd msg


port approveLiquidity : Value -> Cmd msg


port liquidity : Value -> Cmd msg


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

        Debt { debtIn } ->
            debtIn |> Input.isZero

        Collateral { collateralIn } ->
            collateralIn |> Input.isZero


view :
    { model | spot : PriceFeed, images : Images }
    -> Blockchain
    -> Pool
    -> Transaction
    ->
        { first : Element Msg
        , second : Element Msg
        , third : Element Msg
        , buttons : Element Msg
        }
view model blockchain pool (Transaction transaction) =
    { first =
        transaction
            |> assetInSection model
                blockchain
                (pool.pair |> Pair.toAsset)
    , second =
        transaction
            |> duesInSection model
                blockchain
                pool
    , third =
        transaction
            |> liquidityOutSection model
                pool
    , buttons = buttons blockchain
    }


assetInSection :
    { model | images : Images }
    -> Blockchain
    -> Token
    -> { transaction | state : State, tooltip : Maybe Tooltip }
    -> Element Msg
assetInSection model blockchain asset { state, tooltip } =
    column
        [ Region.description "lend asset"
        , width <| px 343
        , height shrink
        , padding 16
        , spacing 10
        , Background.color Color.primary100
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
                , paddingXY 0 3
                , Font.color Color.primary400
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


duesInSection :
    { model | spot : PriceFeed, images : Images }
    -> Blockchain
    -> Pool
    -> { transaction | state : State, tooltip : Maybe Tooltip }
    -> Element Msg
duesInSection model blockchain pool ({ state, tooltip } as transaction) =
    column
        [ Region.description "dues"
        , width <| px 343
        , height shrink
        , padding 16
        , spacing 12
        , Background.color Color.primary100
        , Border.rounded 8
        ]
        [ row
            [ width fill
            , height shrink
            , spacing 16
            ]
            ((case state of
                Asset { out } ->
                    case out of
                        Success { apr, cdp } ->
                            ( apr, cdp ) |> Just

                        _ ->
                            Nothing

                Debt { out } ->
                    case out of
                        Success { apr, cdp } ->
                            ( apr, cdp ) |> Just

                        _ ->
                            Nothing

                Collateral { out } ->
                    case out of
                        Success { apr, cdp } ->
                            ( apr, cdp ) |> Just

                        _ ->
                            Nothing
             )
                |> Maybe.map
                    (\( apr, cdp ) ->
                        [ Info.lendAPR apr
                        , Info.lendCDP model
                            { onMouseEnter = OnMouseEnter
                            , onMouseLeave = OnMouseLeave
                            , cdpTooltip = Tooltip.CDP
                            , symbolTooltip = Tooltip.CDPSymbol
                            , opened = tooltip
                            , pair = pool.pair
                            , cdp = cdp
                            }
                        ]
                    )
                |> Maybe.withDefault
                    [ Info.emptyAPR |> map never
                    , Info.emptyCDP |> map never
                    ]
            )
        , column
            [ width fill
            , height shrink
            , spacing 12
            ]
            [ (case state of
                Asset { out } ->
                    case out |> Remote.map .debtIn of
                        Success uint ->
                            Right uint

                        _ ->
                            Left ""

                Debt { debtIn } ->
                    Left debtIn

                Collateral { out } ->
                    case out |> Remote.map .debtIn of
                        Success uint ->
                            Right uint

                        _ ->
                            Left ""
              )
                |> debtInSection model
                    (pool.pair |> Pair.toAsset)
                    transaction
            , (case state of
                Asset { out } ->
                    case out |> Remote.map .collateralIn of
                        Success uint ->
                            Right uint

                        _ ->
                            Left ""

                Debt { out } ->
                    case out |> Remote.map .collateralIn of
                        Success uint ->
                            Right uint

                        _ ->
                            Left ""

                Collateral { collateralIn } ->
                    Left collateralIn
              )
                |> collateralInSection model
                    blockchain
                    (pool.pair |> Pair.toCollateral)
                    transaction
            ]
        ]


debtInSection :
    { model | images : Images }
    -> Token
    -> { transaction | tooltip : Maybe Tooltip }
    -> Or String Uint
    -> Element Msg
debtInSection model asset { tooltip } out =
    column
        [ width fill
        , height shrink
        , spacing 10
        ]
        [ el
            [ width shrink
            , height shrink
            , Font.size 14
            , paddingXY 0 3
            , Font.color Color.primary400
            ]
            (text "Debt to Repay")
        , Textbox.view model
            { onMouseEnter = OnMouseEnter
            , onMouseLeave = OnMouseLeave
            , tooltip = Tooltip.DebtOutSymbol
            , opened = tooltip
            , token = asset
            , onClick = Just ClickDebtIn
            , onChange = InputDebtIn
            , text = out
            , description = "debt out textbox"
            }
        ]


collateralInSection :
    { model | images : Images }
    -> Blockchain
    -> Token
    -> { transaction | tooltip : Maybe Tooltip }
    -> Or String Uint
    -> Element Msg
collateralInSection model blockchain collateral { tooltip } or =
    column
        [ width fill
        , height shrink
        , spacing 10
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
                , paddingXY 0 3
                , Font.color Color.primary400
                ]
                (text "Collateral to Lock")
            , blockchain
                |> Blockchain.toUser
                |> Maybe.andThen (User.getBalance collateral)
                |> Maybe.map
                    (\balance ->
                        MaxButton.view
                            { onPress = InputMaxCollateral
                            , onMouseEnter = OnMouseEnter
                            , onMouseLeave = OnMouseLeave
                            , tooltip = Tooltip.CollateralBalance
                            , opened = tooltip
                            , token = collateral
                            , balance = balance
                            }
                    )
                |> Maybe.withDefault none
            ]
        , Textbox.view model
            { onMouseEnter = OnMouseEnter
            , onMouseLeave = OnMouseLeave
            , tooltip = Tooltip.CollateralOutSymbol
            , opened = tooltip
            , token = collateral
            , onClick = Just ClickCollateralIn
            , onChange = InputCollateralIn
            , text = or
            , description = "collateral out textbox"
            }
        ]


liquidityOutSection :
    { model | images : Images }
    -> Pool
    -> { transaction | state : State }
    -> Element Msg
liquidityOutSection model pool { state } =
    column
        [ Region.description "liquidity output"
        , width <| px 343
        , height shrink
        , padding 16
        , spacing 10
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
            (text "LP Tokens to Receive")
        , Output.liquidity model
            { asset = pool.pair |> Pair.toAsset
            , collateral = pool.pair |> Pair.toCollateral
            , output =
                case state of
                    Asset { out } ->
                        out
                            |> Remote.map .liquidityOut

                    Debt { out } ->
                        out
                            |> Remote.map .liquidityOut

                    Collateral { out } ->
                        out
                            |> Remote.map .liquidityOut
            , description = "liquidity out"
            }
        ]


buttons : Blockchain -> Element Msg
buttons blockchain =
    column
        [ width <| px 343
        , height shrink
        , spacing 12
        ]
        (blockchain
            |> Blockchain.toUser
            |> Maybe.map
                (\_ -> [])
            |> Maybe.withDefault
                [ Button.connect ClickConnect ]
        )
