port module Page.Transaction.Liquidity.Main exposing
    ( Create
    , CreateMsg
    , Effect(..)
    , Transaction
    , TransactionMsg
    , createPool
    , disabled
    , disabledCreate
    , empty
    , init
    , initCreate
    , refresh
    , refreshCreate
    , update
    , updateCreate
    , view
    )

import Blockchain.Main as Blockchain exposing (Blockchain)
import Blockchain.User.Main as User exposing (User)
import Data.Chains exposing (Chains)
import Data.Deadline exposing (Deadline)
import Data.Images exposing (Images)
import Data.Pair as Pair
import Data.Pool exposing (Pool)
import Data.Remote exposing (Remote(..))
import Data.Slippage exposing (Slippage)
import Data.Token as Token exposing (Token)
import Data.Uint as Uint
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
import Page.Transaction.Liquidity.Answer as Answer
    exposing
        ( LiquidityGivenNew
        , OutGivenAsset
        , OutGivenCollateral
        , OutGivenDebt
        )
import Page.Transaction.Liquidity.Error
    exposing
        ( CreateError
        , TransactionError
        )
import Page.Transaction.Liquidity.Query as Query
import Page.Transaction.Liquidity.Tooltip as Tooltip exposing (Tooltip)
import Page.Transaction.Output as Output
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
    , out : Remote TransactionError OutGivenAsset
    }


type alias DebtInput =
    { debtOut : String
    , out : Remote TransactionError OutGivenDebt
    }


type alias CollateralInput =
    { collateralOut : String
    , out : Remote TransactionError OutGivenCollateral
    }


type Create
    = Create
        { assetIn : String
        , debtOut : String
        , collateralOut : String
        , liquidity : Remote CreateError LiquidityGivenNew
        , tooltip : Maybe Tooltip
        }


type Msg otherMsg
    = InputAssetIn String
    | InputMaxAsset
    | InputDebtOut String
    | InputCollateralOut String
    | InputMaxCollateral
    | OtherMsg otherMsg
    | ClickConnect
    | ClickApproveAsset
    | ClickApproveCollateral
    | OnMouseEnter Tooltip
    | OnMouseLeave


type OnlyTransaction
    = ReceiveTransactionAnswer Value
    | ClickAssetIn
    | ClickDebtOut
    | ClickCollateralOut
    | QueryAgain Posix


type OnlyCreate
    = ReceiveCreateAnswer Value


type alias TransactionMsg =
    Msg OnlyTransaction


type alias CreateMsg =
    Msg OnlyCreate


type Effect
    = OpenConnect
    | OpenConfirm


init : Transaction
init =
    { state =
        { assetIn = ""
        , out =
            Answer.initGivenAsset
                |> Success
        }
            |> Asset
    , tooltip = Nothing
    }
        |> Transaction


initCreate : Create
initCreate =
    { assetIn = ""
    , debtOut = ""
    , collateralOut = ""
    , liquidity =
        Answer.initGivenNew
            |> Success
    , tooltip = Nothing
    }
        |> Create


refresh : Transaction -> Transaction
refresh (Transaction transaction) =
    { transaction
        | state =
            case transaction.state of
                Asset asset ->
                    { asset | out = Loading }
                        |> Asset

                Debt debt ->
                    { debt | out = Loading }
                        |> Debt

                Collateral collateral ->
                    { collateral | out = Loading }
                        |> Collateral
        , tooltip = Nothing
    }
        |> Transaction


refreshCreate : Create -> Create
refreshCreate (Create create) =
    { create
        | liquidity = Loading
        , tooltip = Nothing
    }
        |> Create


update :
    { model
        | time : Posix
        , chains : Chains
        , slippage : Slippage
        , deadline : Deadline
    }
    -> Blockchain
    -> Pool
    -> PoolInfo
    -> TransactionMsg
    -> Transaction
    -> ( Transaction, Cmd TransactionMsg, Maybe Effect )
update model blockchain pool poolInfo msg (Transaction transaction) =
    case msg of
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

        InputDebtOut debtOut ->
            if debtOut |> Uint.isAmount (pool.pair |> Pair.toAsset) then
                { transaction | state = debtOut |> updateGivenDebtOut }
                    |> query model blockchain pool poolInfo

            else
                transaction |> noCmdAndEffect

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

        OtherMsg (ReceiveTransactionAnswer value) ->
            (case
                ( value
                    |> Decode.decodeValue (Answer.decoder model)
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
                                { asset
                                    | out =
                                        answer.result
                                            |> Answer.toOutGivenAsset
                                }
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
                                { debt
                                    | out =
                                        answer.result
                                            |> Answer.toOutGivenDebt
                                }
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
                                { collateral
                                    | out =
                                        answer.result
                                            |> Answer.toOutGivenCollateral
                                }
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

        OtherMsg ClickAssetIn ->
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

        OtherMsg ClickDebtOut ->
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

        OtherMsg ClickCollateralOut ->
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

        OtherMsg (QueryAgain _) ->
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
            Answer.initGivenAsset
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
            Answer.initGivenDebt
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
            Answer.initGivenCollateral
                |> Success

        else
            Loading
    }
        |> Collateral


noCmdAndEffect :
    { state : State, tooltip : Maybe Tooltip }
    -> ( Transaction, Cmd TransactionMsg, Maybe Effect )
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
    -> ( Transaction, Cmd TransactionMsg, Maybe Effect )
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
    -> ( Transaction, Cmd TransactionMsg, Maybe Effect )
queryPerSecond =
    constructQuery queryLiquidityPerSecond


constructQuery :
    (Value -> Cmd TransactionMsg)
    -> { model | slippage : Slippage }
    -> Blockchain
    -> Pool
    -> PoolInfo
    ->
        { state : State
        , tooltip : Maybe Tooltip
        }
    -> ( Transaction, Cmd TransactionMsg, Maybe Effect )
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


updateCreate :
    { model | chains : Chains }
    -> Blockchain
    -> Pool
    -> CreateMsg
    -> Create
    -> ( Create, Cmd CreateMsg, Maybe Effect )
updateCreate model blockchain pool msg (Create create) =
    case msg of
        InputAssetIn assetIn ->
            if assetIn |> Uint.isAmount (pool.pair |> Pair.toAsset) then
                { create
                    | assetIn = assetIn
                    , liquidity =
                        if
                            (assetIn |> Input.isZero)
                                || (create.debtOut |> Input.isZero)
                                || (create.collateralOut |> Input.isZero)
                        then
                            Answer.initGivenNew
                                |> Success

                        else
                            Loading
                }
                    |> queryNew blockchain pool

            else
                create |> noCreateCmdAndEffect

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
                        { create
                            | assetIn = assetIn
                            , liquidity =
                                if
                                    (assetIn |> Input.isZero)
                                        || (create.debtOut |> Input.isZero)
                                        || (create.collateralOut |> Input.isZero)
                                then
                                    Answer.initGivenNew
                                        |> Success

                                else
                                    Loading
                        }
                            |> queryNew blockchain pool
                    )
                |> Maybe.withDefault (create |> noCreateCmdAndEffect)

        InputDebtOut debtOut ->
            if debtOut |> Uint.isAmount (pool.pair |> Pair.toAsset) then
                { create
                    | debtOut = debtOut
                    , liquidity =
                        if
                            (create.assetIn |> Input.isZero)
                                || (debtOut |> Input.isZero)
                                || (create.collateralOut |> Input.isZero)
                        then
                            Answer.initGivenNew
                                |> Success

                        else
                            Loading
                }
                    |> queryNew blockchain pool

            else
                create |> noCreateCmdAndEffect

        InputCollateralOut collateralOut ->
            if collateralOut |> Uint.isAmount (pool.pair |> Pair.toCollateral) then
                { create
                    | collateralOut = collateralOut
                    , liquidity =
                        if
                            (create.assetIn |> Input.isZero)
                                || (create.debtOut |> Input.isZero)
                                || (collateralOut |> Input.isZero)
                        then
                            Answer.initGivenNew
                                |> Success

                        else
                            Loading
                }
                    |> queryNew blockchain pool

            else
                create |> noCreateCmdAndEffect

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
                        { create
                            | collateralOut = collateralOut
                            , liquidity =
                                if
                                    (create.assetIn |> Input.isZero)
                                        || (create.debtOut |> Input.isZero)
                                        || (collateralOut |> Input.isZero)
                                then
                                    Answer.initGivenNew
                                        |> Success

                                else
                                    Loading
                        }
                            |> queryNew blockchain pool
                    )
                |> Maybe.withDefault (create |> noCreateCmdAndEffect)

        OtherMsg (ReceiveCreateAnswer value) ->
            (case value |> Decode.decodeValue (Answer.decoderCreate model) of
                Ok answer ->
                    if
                        (answer.chainId == (blockchain |> Blockchain.toChain))
                            && (answer.pool == pool)
                            && (Just answer.assetIn
                                    == (create.assetIn
                                            |> Uint.fromAmount
                                                (pool.pair |> Pair.toAsset)
                                       )
                               )
                            && (Just answer.debtOut
                                    == (create.debtOut
                                            |> Uint.fromAmount
                                                (pool.pair |> Pair.toAsset)
                                       )
                               )
                            && (Just answer.collateralOut
                                    == (create.collateralOut
                                            |> Uint.fromAmount
                                                (pool.pair |> Pair.toCollateral)
                                       )
                               )
                    then
                        { create
                            | liquidity =
                                answer.result
                                    |> Answer.toLiquidityGivenNew
                        }
                            |> Just

                    else
                        Nothing

                Err _ ->
                    Nothing
            )
                |> Maybe.map noCreateCmdAndEffect
                |> Maybe.withDefault (create |> noCreateCmdAndEffect)

        ClickConnect ->
            blockchain
                |> Blockchain.toUser
                |> Maybe.map (\_ -> create |> noCreateCmdAndEffect)
                |> Maybe.withDefault
                    ( create |> Create
                    , Cmd.none
                    , OpenConnect |> Just
                    )

        ClickApproveAsset ->
            (case
                ( blockchain |> Blockchain.toUser
                , create.assetIn
                    |> Uint.fromAmount
                        (pool.pair |> Pair.toAsset)
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
                        ( create |> Create
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
                |> Maybe.withDefault (create |> noCreateCmdAndEffect)

        ClickApproveCollateral ->
            (case
                ( blockchain |> Blockchain.toUser
                , create.collateralOut
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
                        ( create |> Create
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
                |> Maybe.withDefault (create |> noCreateCmdAndEffect)

        OnMouseEnter tooltip ->
            { create | tooltip = Just tooltip }
                |> noCreateCmdAndEffect

        OnMouseLeave ->
            { create | tooltip = Nothing }
                |> noCreateCmdAndEffect


noCreateCmdAndEffect :
    { assetIn : String
    , debtOut : String
    , collateralOut : String
    , liquidity : Remote CreateError LiquidityGivenNew
    , tooltip : Maybe Tooltip
    }
    -> ( Create, Cmd CreateMsg, Maybe Effect )
noCreateCmdAndEffect create =
    ( create |> Create
    , Cmd.none
    , Nothing
    )


queryNew :
    Blockchain
    -> Pool
    ->
        { assetIn : String
        , debtOut : String
        , collateralOut : String
        , liquidity : Remote CreateError LiquidityGivenNew
        , tooltip : Maybe Tooltip
        }
    -> ( Create, Cmd CreateMsg, Maybe Effect )
queryNew =
    constructQueryNew queryLiquidity


constructQueryNew :
    (Value -> Cmd CreateMsg)
    -> Blockchain
    -> Pool
    ->
        { assetIn : String
        , debtOut : String
        , collateralOut : String
        , liquidity : Remote CreateError LiquidityGivenNew
        , tooltip : Maybe Tooltip
        }
    -> ( Create, Cmd CreateMsg, Maybe Effect )
constructQueryNew givenCmd blockchain pool create =
    (if
        (create.assetIn |> Input.isZero)
            || (create.debtOut |> Input.isZero)
            || (create.collateralOut |> Input.isZero)
     then
        Nothing

     else
        case
            ( create.assetIn
                |> Uint.fromAmount
                    (pool.pair |> Pair.toAsset)
            , create.debtOut
                |> Uint.fromAmount
                    (pool.pair |> Pair.toAsset)
            , create.collateralOut
                |> Uint.fromAmount
                    (pool.pair |> Pair.toCollateral)
            )
        of
            ( Just assetIn, Just debtIn, Just collateralIn ) ->
                { chainId = blockchain |> Blockchain.toChain
                , pool = pool
                , assetIn = assetIn
                , debtOut = debtIn
                , collateralOut = collateralIn
                }
                    |> Query.givenNew
                    |> Just

            _ ->
                Nothing
    )
        |> Maybe.map givenCmd
        |> Maybe.withDefault Cmd.none
        |> (\cmd ->
                ( create |> Create
                , cmd
                , Nothing
                )
           )


port queryLiquidity : Value -> Cmd msg


port queryLiquidityPerSecond : Value -> Cmd msg


port approveLiquidity : Value -> Cmd msg


view :
    { model | images : Images }
    -> Blockchain
    -> Pool
    -> Transaction
    ->
        { first : Element TransactionMsg
        , second : Element TransactionMsg
        , buttons : Element TransactionMsg
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
    -> Element TransactionMsg
viewAssetIn model blockchain asset ({ state, tooltip } as transaction) =
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
            (el
                [ width shrink
                , height shrink
                , Font.size 14
                ]
                (text "Amount to Lend")
                :: (blockchain
                        |> Blockchain.toUser
                        |> Maybe.map
                            (\user ->
                                [ userBalance user asset transaction
                                , maxButton
                                ]
                            )
                        |> Maybe.withDefault
                            []
                   )
            )
        , Textbox.view model
            { onMouseEnter = OnMouseEnter
            , onMouseLeave = OnMouseLeave
            , tooltip = Tooltip.AssetInSymbol
            , opened = tooltip
            , token = asset
            , onChange = InputAssetIn
            , text = "" |> Debug.log "later"
            , description = "asset in textbox"
            }
        ]


userBalance :
    User
    -> Token
    -> { transaction | tooltip : Maybe Tooltip }
    -> Element (Msg otherMsg)
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


maxButton : Element (Msg otherMsg)
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


createPool :
    { model | images : Images }
    -> Blockchain
    -> Pool
    -> Create
    ->
        { first : Element CreateMsg
        , second : Element CreateMsg
        , buttons : Element CreateMsg
        }
createPool model blockchain pool (Create create) =
    { first =
        create
            |> createAssetIn model blockchain (pool.pair |> Pair.toAsset)
    , second = none |> Debug.log "later"
    , buttons = none |> Debug.log "later"
    }


createAssetIn :
    { model | images : Images }
    -> Blockchain
    -> Token
    -> { create | tooltip : Maybe Tooltip }
    -> Element CreateMsg
createAssetIn model blockchain asset ({ tooltip } as transaction) =
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
            (el
                [ width shrink
                , height shrink
                , Font.size 14
                ]
                (text "Amount to Lend")
                :: (blockchain
                        |> Blockchain.toUser
                        |> Maybe.map
                            (\user ->
                                [ userBalance user asset transaction
                                , maxButton
                                ]
                            )
                        |> Maybe.withDefault
                            []
                   )
            )
        , Textbox.view model
            { onMouseEnter = OnMouseEnter
            , onMouseLeave = OnMouseLeave
            , tooltip = Tooltip.AssetInSymbol
            , opened = tooltip
            , token = asset
            , onChange = InputAssetIn
            , text = "" |> Debug.log "later"
            , description = "asset in textbox"
            }
        ]


disabledCreate :
    { model | images : Images }
    -> Blockchain
    -> Pool
    -> Create
    ->
        { first : Element Never
        , second : Element Never
        }
disabledCreate model blockchain pool (Create create) =
    { first =
        create
            |> disabledCreateAssetIn
                model
                blockchain
                (pool.pair |> Pair.toAsset)
    , second = none |> Debug.log "later"
    }


disabledCreateAssetIn :
    { model | images : Images }
    -> Blockchain
    -> Token
    -> { create | assetIn : String }
    -> Element Never
disabledCreateAssetIn model blockchain asset create =
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
            , text = create.assetIn
            , description = "lend asset textbox"
            }
        ]


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
