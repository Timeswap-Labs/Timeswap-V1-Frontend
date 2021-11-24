module Page.Main exposing
    ( Page
    , init
    , toParameter
    , toTab
    )

import Blockchain.Main exposing (Blockchain)
import Data.Chains exposing (Chains)
import Data.Parameter exposing (Parameter)
import Data.Support exposing (Support)
import Data.Tab as Tab exposing (Tab)
import Page.Route as Route
import Page.Transaction.Borrow.Main as TransactionBorrow
import Page.Transaction.Lend.Main as TransactionLend
import Page.Transaction.Liquidity.Main as TransactionLiquidity
import Url exposing (Url)


type Page
    = Lend { transaction : TransactionLend.Section }
    | Borrow { transaction : TransactionBorrow.Section }
    | Liquidity { transaction : TransactionLiquidity.Section }


init :
    { model
        | chains : Chains
        , blockchain : Support userNotSupported Blockchain
    }
    -> Url
    -> Page
init { chains, blockchain } url =
    url
        |> Route.fromUrl blockchain chains
        |> Maybe.map
            (\route ->
                case route of
                    Route.Lend parameter ->
                        { transaction = TransactionLend.init parameter }
                            |> Lend

                    Route.Borrow parameter ->
                        { transaction = TransactionBorrow.init parameter }
                            |> Borrow

                    Route.Liquidity parameter ->
                        { transaction = TransactionLiquidity.init parameter }
                            |> Liquidity
            )
        |> Maybe.withDefault
            ({ transaction = TransactionLend.init Nothing }
                |> Lend
            )


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
