module Data.Wallets exposing
    ( Flags
    , Wallets
    , init
    )

import Data.Wallet as Wallet exposing (Wallet)
import Sort.Set as Set exposing (Set)


type alias Wallets =
    Set Wallet


type alias Flags =
    List Wallet.Flag


init : Flags -> Wallets
init list =
    list
        |> List.foldl
            (\flag accumulator ->
                flag
                    |> Wallet.init
                    |> Maybe.map
                        (\wallet ->
                            accumulator
                                |> Set.insert wallet
                        )
                    |> Maybe.withDefault accumulator
            )
            (Set.empty Wallet.sorter)
