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
import Data.Chains exposing (Chains)
import Data.Deadline exposing (Deadline)
import Data.Pair exposing (Pair)
import Data.Parameter as Parameter exposing (Parameter)
import Data.Slippage exposing (Slippage)
import Data.Support exposing (Support)
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
import Page.Transaction.Liquidity.Main as TransactionLiquidity
import Page.Transaction.Main as Transaction
import Page.Transaction.PoolInfo exposing (PoolInfo)
import Time exposing (Posix)
import Url exposing (Url)


type Page
    = Lend { transaction : Transaction.Section Lend.Transaction () }
    | Borrow { transaction : TransactionBorrow.Section }
    | Liquidity { transaction : TransactionLiquidity.Section }


type Msg
    = TransactionLendMsg (Transaction.Msg Lend.Msg Never)


type Effect
    = OpenTokenList TokenParam
    | OpenMaturityList Pair
    | OpenConnect
    | OpenPending


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
construct ({ chains, blockchain } as model) url maybePage =
    url
        |> Route.fromUrl blockchain chains
        |> Maybe.map
            (\route ->
                case ( route, maybePage |> Maybe.andThen toPoolInfo ) of
                    ( Route.Lend (Just (Parameter.Pool pool)), Just poolInfo ) ->
                        poolInfo
                            |> Transaction.initGivenPool Lend.init model pool
                            |> (\transaction ->
                                    ( { transaction = transaction }
                                        |> Lend
                                    , Cmd.none
                                    )
                               )

                    ( Route.Lend parameter, _ ) ->
                        parameter
                            |> Transaction.init model
                            |> Tuple.mapBoth
                                (\transaction ->
                                    { transaction = transaction }
                                        |> Lend
                                )
                                (Cmd.map TransactionLendMsg)

                    ( Route.Borrow parameter, _ ) ->
                        ( { transaction = TransactionBorrow.init parameter }
                            |> Borrow
                        , Cmd.none
                        )

                    ( Route.Liquidity parameter, _ ) ->
                        ( { transaction = TransactionLiquidity.init parameter }
                            |> Liquidity
                        , Cmd.none
                        )
            )
        |> Maybe.withDefault
            (Nothing
                |> Transaction.init model
                |> Tuple.mapBoth
                    (\transaction ->
                        { transaction = transaction }
                            |> Lend
                    )
                    (Cmd.map TransactionLendMsg)
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
                    { transaction = Lend.update
                    , create = Lend.updateDoesNotExist
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
            OpenMaturityList pair |> Just

        Transaction.OpenConnect ->
            OpenConnect |> Just

        Transaction.TransactionEffect Lend.OpenConnect ->
            OpenConnect |> Just

        Transaction.TransactionEffect Lend.OpenPending ->
            OpenPending |> Just

        _ ->
            Nothing


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
                |> TransactionLiquidity.toParameter


toPoolInfo : Page -> Maybe PoolInfo
toPoolInfo page =
    case page of
        Lend { transaction } ->
            transaction
                |> Transaction.toPoolInfo

        _ ->
            Nothing


view : Blockchain -> Page -> Element Msg
view blockchain page =
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
                        { transaction = Lend.view
                        , create = Lend.viewDoesNotExist
                        , disabled = Lend.viewDisabled
                        , empty = Lend.viewEmpty
                        }
                        blockchain
                    |> map TransactionLendMsg
                ]

            _ ->
                []
        )
