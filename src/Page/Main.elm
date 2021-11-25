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
        , el
        , height
        , map
        , padding
        , shrink
        , spacing
        , text
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Page.Route as Route
import Page.Transaction.Borrow.Main as TransactionBorrow
import Page.Transaction.Lend.Main as Lend
import Page.Transaction.Liquidity.Main as TransactionLiquidity
import Page.Transaction.Main as Transaction
import Page.Transaction.PoolInfo exposing (PoolInfo)
import Time exposing (Posix)
import Url exposing (Url)
import Utility.Color as Color


type Page
    = Lend { transaction : Transaction.Section Lend.Transaction () }
    | Borrow { transaction : TransactionBorrow.Section }
    | Liquidity { transaction : TransactionLiquidity.Section }


type Msg
    = TransactionLendMsg (Transaction.Msg Lend.Msg)


type Effect
    = OpenTokenList TokenParam
    | OpenMaturityList Pair
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
                    Lend.update
                    model
                    blockchain
                    transactionLendMsg
                |> (\( updated, cmd, maybeEffect ) ->
                        ( { lend | transaction = updated }
                            |> Lend
                        , cmd |> Cmd.map TransactionLendMsg
                        , maybeEffect
                            |> Maybe.map transactionLendEffect
                        )
                   )

        _ ->
            ( page, Cmd.none, Nothing )


transactionLendEffect : Transaction.Effect Lend.Effect -> Effect
transactionLendEffect effect =
    case effect of
        Transaction.OpenTokenList tokenParam ->
            OpenTokenList tokenParam

        Transaction.OpenMaturityList pair ->
            OpenMaturityList pair

        Transaction.TransactionEffect Lend.OpenPending ->
            OpenPending


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


view : Element Msg
view =
    column
        [ width shrink
        , height shrink
        , padding 20
        , spacing 20
        , centerX
        , alignTop
        , Background.color Color.light100
        , Border.rounded 8
        ]
        [ el [] (text "Lend")
        , Transaction.view Lend.viewEmpty |> map TransactionLendMsg
        ]
