module Page.Main exposing
    ( Effect(..)
    , Msg
    , Page
    , change
    , init
    , toParameter
    , toPoolInfo
    , toTab
    , update
    )

import Blockchain.Main exposing (Blockchain)
import Data.Chains exposing (Chains)
import Data.Pair exposing (Pair)
import Data.Parameter as Parameter exposing (Parameter)
import Data.Pool exposing (Pool)
import Data.Support exposing (Support)
import Data.Tab as Tab exposing (Tab)
import Data.TokenParam exposing (TokenParam)
import Page.Route as Route
import Page.Transaction.Borrow.Main as TransactionBorrow
import Page.Transaction.Lend.Main as TransactionLend
import Page.Transaction.Liquidity.Main as TransactionLiquidity
import Page.Transaction.PoolInfo exposing (PoolInfo)
import Time exposing (Posix)
import Url exposing (Url)


type Page
    = Lend { transaction : TransactionLend.Section }
    | Borrow { transaction : TransactionBorrow.Section }
    | Liquidity { transaction : TransactionLiquidity.Section }


type Msg
    = TransactionLendMsg TransactionLend.Msg


type Effect
    = OpenTokenList TokenParam
    | OpenMaturityList Pair


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
                            |> TransactionLend.initGivenPool model pool
                            |> (\transaction ->
                                    ( { transaction = transaction }
                                        |> Lend
                                    , Cmd.none
                                    )
                               )

                    ( Route.Lend parameter, _ ) ->
                        parameter
                            |> TransactionLend.init model
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
                |> TransactionLend.init model
                |> Tuple.mapBoth
                    (\transaction ->
                        { transaction = transaction }
                            |> Lend
                    )
                    (Cmd.map TransactionLendMsg)
            )


update : Msg -> Page -> ( Page, Cmd Msg, Maybe Effect )
update msg page =
    case ( msg, page ) of
        ( TransactionLendMsg transactionLendMsg, Lend lend ) ->
            lend.transaction
                |> TransactionLend.update transactionLendMsg
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


transactionLendEffect : TransactionLend.Effect -> Effect
transactionLendEffect effect =
    case effect of
        TransactionLend.OpenTokenList tokenParam ->
            OpenTokenList tokenParam

        TransactionLend.OpenMaturityList pair ->
            OpenMaturityList pair


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
                |> TransactionLend.toParameter

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
                |> TransactionLend.toPoolInfo

        _ ->
            Nothing
