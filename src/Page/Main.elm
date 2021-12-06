module Page.Main exposing
    ( Effect(..)
    , Msg
    , Page
    , change
    , init
    , subscriptions
    , toParameter
    , toPoolInfo
    , toTab
    , update
    , view
    )

import Blockchain.Main exposing (Blockchain)
import Data.Backdrop exposing (Backdrop)
import Data.Chains exposing (Chains)
import Data.ChosenZone exposing (ChosenZone)
import Data.Deadline exposing (Deadline)
import Data.Images exposing (Images)
import Data.Pair exposing (Pair)
import Data.Parameter as Parameter exposing (Parameter)
import Data.ShowCreate as ShowCreate exposing (ShowCreate)
import Data.Slippage exposing (Slippage)
import Data.Support exposing (Support(..))
import Data.Tab as Tab exposing (Tab)
import Data.TokenParam exposing (TokenParam)
import Element
    exposing
        ( Element
        , alignTop
        , centerX
        , column
        , height
        , map
        , shrink
        , spacing
        , width
        )
import Page.Route as Route
import Page.Transaction.Borrow.Main as TransactionBorrow
import Page.Transaction.Lend.Main as Lend
import Page.Transaction.Liquidity.Main as Liquidity
import Page.Transaction.Main as Transaction
import Page.Transaction.PoolInfo exposing (PoolInfo)
import Time exposing (Posix, Zone)
import Url exposing (Url)


type Page
    = Lend { transaction : Transaction.Section Lend.Transaction () }
    | Borrow { transaction : TransactionBorrow.Section }
    | Liquidity { transaction : Transaction.Section Liquidity.Transaction Liquidity.Create }


type Msg
    = TransactionLendMsg (Transaction.Msg Lend.Msg Never)
    | TransactionLiquidityMsg (Transaction.Msg Liquidity.TransactionMsg Liquidity.CreateMsg)


type Effect
    = OpenTokenList TokenParam
    | OpenMaturityList Pair ShowCreate
    | OpenChooseMaturity Pair
    | OpenSettings
    | OpenConnect
    | OpenConfirm


init :
    { model
        | time : Posix
        , chains : Chains
        , blockchain : Support userNotSupported Blockchain
    }
    -> Url
    -> ( Page, Cmd Msg )
init model url =
    Nothing
        |> construct model url


change :
    { model
        | time : Posix
        , chains : Chains
        , blockchain : Support userNotSupported Blockchain
    }
    -> Url
    -> Page
    -> ( Page, Cmd Msg )
change model url page =
    Just page
        |> construct model url


construct :
    { model
        | time : Posix
        , chains : Chains
        , blockchain : Support userNotSupported Blockchain
    }
    -> Url
    -> Maybe Page
    -> ( Page, Cmd Msg )
construct ({ chains } as model) url maybePage =
    case
        ( url |> Route.fromUrl model.blockchain chains
        , model.blockchain
        , maybePage |> Maybe.andThen toPoolInfo
        )
    of
        ( Just (Route.Lend (Just (Parameter.Pool pool))), Supported blockchain, Just poolInfo ) ->
            poolInfo
                |> Transaction.initGivenPool Lend.init model blockchain pool
                |> Tuple.mapBoth
                    (\transaction ->
                        { transaction = transaction }
                            |> Lend
                    )
                    (Cmd.map TransactionLendMsg)

        ( Just (Route.Lend parameter), Supported blockchain, _ ) ->
            parameter
                |> Transaction.init model blockchain
                |> Tuple.mapBoth
                    (\transaction ->
                        { transaction = transaction }
                            |> Lend
                    )
                    (Cmd.map TransactionLendMsg)

        ( Just (Route.Borrow parameter), _, _ ) ->
            ( { transaction = TransactionBorrow.init parameter }
                |> Borrow
            , Cmd.none
            )

        ( Just (Route.Liquidity (Just (Parameter.Pool pool))), Supported blockchain, Just poolInfo ) ->
            poolInfo
                |> Transaction.initGivenPool Liquidity.init model blockchain pool
                |> Tuple.mapBoth
                    (\transaction ->
                        { transaction = transaction }
                            |> Liquidity
                    )
                    (Cmd.map TransactionLiquidityMsg)

        ( Just (Route.Liquidity parameter), Supported blockchain, _ ) ->
            parameter
                |> Transaction.init model blockchain
                |> Tuple.mapBoth
                    (\transaction ->
                        { transaction = transaction }
                            |> Liquidity
                    )
                    (Cmd.map TransactionLiquidityMsg)

        _ ->
            ( { transaction = Transaction.notSupported }
                |> Lend
            , Cmd.none
            )


update :
    { model
        | time : Posix
        , chains : Chains
        , slippage : Slippage
        , deadline : Deadline
    }
    -> Blockchain
    -> Msg
    -> Page
    -> ( Page, Cmd Msg, Maybe Effect )
update model blockchain msg page =
    case ( msg, page ) of
        ( TransactionLendMsg transactionLendMsg, Lend lend ) ->
            lend.transaction
                |> Transaction.update
                    { initTransaction = Lend.init
                    , refreshTransaction = Lend.refresh
                    , transaction = Lend.update model blockchain
                    , initCreate = ()
                    , refreshCreate = identity
                    , create = \_ _ () -> ( (), Cmd.none, Nothing )
                    }
                    model
                    blockchain
                    transactionLendMsg
                |> (\( updated, cmd, maybeEffect ) ->
                        ( { lend | transaction = updated }
                            |> Lend
                        , cmd |> Cmd.map TransactionLendMsg
                        , maybeEffect
                            |> Maybe.andThen transactionLendEffect
                        )
                   )

        ( TransactionLiquidityMsg transactionLiquidityMsg, Liquidity liquidity ) ->
            liquidity.transaction
                |> Transaction.update
                    { initTransaction = Liquidity.init
                    , refreshTransaction = Liquidity.refresh
                    , transaction = Liquidity.update model blockchain
                    , initCreate = Liquidity.initCreate
                    , refreshCreate = Liquidity.refreshCreate
                    , create = Liquidity.updateCreate model blockchain
                    }
                    model
                    blockchain
                    transactionLiquidityMsg
                |> (\( updated, cmd, maybeEffect ) ->
                        ( { liquidity | transaction = updated }
                            |> Liquidity
                        , cmd |> Cmd.map TransactionLiquidityMsg
                        , maybeEffect
                            |> Maybe.map transactionLiquidityEffect
                        )
                   )

        _ ->
            ( page, Cmd.none, Nothing )


transactionLendEffect :
    Transaction.Effect Lend.Effect Never
    -> Maybe Effect
transactionLendEffect effect =
    case effect of
        Transaction.OpenTokenList tokenParam ->
            OpenTokenList tokenParam |> Just

        Transaction.OpenMaturityList pair ->
            OpenMaturityList pair ShowCreate.DoNot |> Just

        Transaction.OpenSettings ->
            OpenSettings |> Just

        Transaction.OpenConnect ->
            OpenConnect |> Just

        Transaction.TransactionEffect Lend.OpenConnect ->
            OpenConnect |> Just

        Transaction.TransactionEffect Lend.OpenConfirm ->
            OpenConfirm |> Just

        _ ->
            Nothing


transactionLiquidityEffect :
    Transaction.Effect Liquidity.Effect Liquidity.Effect
    -> Effect
transactionLiquidityEffect effect =
    case effect of
        Transaction.OpenTokenList tokenParam ->
            OpenTokenList tokenParam

        Transaction.OpenMaturityList pair ->
            OpenMaturityList pair ShowCreate.Do

        Transaction.OpenChooseMaturity pair ->
            OpenChooseMaturity pair

        Transaction.OpenSettings ->
            OpenSettings

        Transaction.OpenConnect ->
            OpenConnect

        Transaction.TransactionEffect Liquidity.OpenConnect ->
            OpenConnect

        Transaction.TransactionEffect Liquidity.OpenConfirm ->
            OpenConfirm

        Transaction.CreateEffect Liquidity.OpenConnect ->
            OpenConnect

        Transaction.CreateEffect Liquidity.OpenConfirm ->
            OpenConfirm


subscriptions : Page -> Sub Msg
subscriptions page =
    case page of
        Lend { transaction } ->
            [ transaction
                |> Transaction.subscriptions Lend.subscriptions
                |> Sub.map TransactionLendMsg
            ]
                |> Sub.batch

        _ ->
            Sub.none


toTab : Page -> Tab
toTab page =
    case page of
        Lend _ ->
            Tab.Lend

        Borrow _ ->
            Tab.Borrow

        Liquidity _ ->
            Tab.Liquidity


toParameter : Page -> Maybe Parameter
toParameter page =
    case page of
        Lend { transaction } ->
            transaction
                |> Transaction.toParameter

        Borrow { transaction } ->
            transaction
                |> TransactionBorrow.toParameter

        Liquidity { transaction } ->
            transaction
                |> Transaction.toParameter


toPoolInfo : Page -> Maybe PoolInfo
toPoolInfo page =
    case page of
        Lend { transaction } ->
            transaction
                |> Transaction.toPoolInfo

        Liquidity { transaction } ->
            transaction
                |> Transaction.toPoolInfo

        _ ->
            Nothing


view :
    { model
        | time : Posix
        , zone : Zone
        , chosenZone : ChosenZone
        , backdrop : Backdrop
        , images : Images
    }
    -> Blockchain
    -> Page
    -> Element Msg
view model blockchain page =
    column
        [ width shrink
        , height shrink
        , spacing 20
        , centerX
        , alignTop
        ]
        (case page of
            Lend { transaction } ->
                [ transaction
                    |> Transaction.view
                        { title = "Lend"
                        , createTitle = "Lend"
                        , showCreate = ShowCreate.DoNot
                        , transaction = Lend.view model blockchain
                        , disabledTransaction = Lend.disabled model blockchain
                        , create = Lend.doesNotExist model
                        , disabledCreate = Lend.disabledDoesNotExist model
                        , empty = Lend.empty model
                        }
                        model
                    |> map TransactionLendMsg
                ]

            Liquidity { transaction } ->
                [ transaction
                    |> Transaction.view
                        { title = "Add Liquidity"
                        , createTitle = "Create Pool"
                        , showCreate = ShowCreate.Do
                        , transaction = Liquidity.view model blockchain
                        , disabledTransaction = Liquidity.disabled model blockchain
                        , create = Liquidity.createPool model blockchain
                        , disabledCreate = Liquidity.disabledCreate model blockchain
                        , empty = Liquidity.empty model
                        }
                        model
                    |> map TransactionLiquidityMsg
                ]

            _ ->
                []
        )
