module Data.ERC20 exposing (ERC20, compare, fromString, sorter, toString, toSymbol)

import Data.Address as Address exposing (Address)
import Data.Chain exposing (Chain)
import Sort exposing (Sorter)
import Sort.Set as Set exposing (Set)


type ERC20
    = ERC20
        { address : Address
        , name : String
        , symbol : String
        , decimals : Int
        }


fromString : Chain -> String -> Maybe ERC20
fromString chain string =
    string
        |> Address.fromString
        |> Maybe.andThen
            (\address ->
                whitelist chain
                    |> Set.foldl
                        (\erc20 accumulator ->
                            if (erc20 |> toAddress) == address then
                                Just erc20

                            else
                                accumulator
                        )
                        Nothing
            )


toString : ERC20 -> String
toString erc20 =
    erc20
        |> toAddress
        |> Address.toString


toAddress : ERC20 -> Address
toAddress (ERC20 { address }) =
    address


toSymbol : ERC20 -> String
toSymbol (ERC20 { symbol }) =
    symbol


whitelist : Chain -> Set ERC20
whitelist chain =
    Set.empty (sorter chain)


sorter : Chain -> Sorter ERC20
sorter chain =
    Sort.increasing
        |> Sort.by (\erc20 -> 0)
        |> Sort.tiebreaker (Address.sorter |> Sort.by (\(ERC20 { address }) -> address))


compare : Chain -> ERC20 -> ERC20 -> Order
compare chain ((ERC20 erc20a) as erc20A) ((ERC20 erc20b) as erc20B) =
    Address.compare erc20a.address erc20b.address
