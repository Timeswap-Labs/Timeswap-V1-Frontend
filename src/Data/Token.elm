module Data.Token exposing
    ( Token(..)
    , compare
    , fromAssetFragment
    , fromCollateralFragment
    , sorter
    , toAssetFragment
    , toCollateralFragment
    , toName
    , toSymbol
    )

import Data.Chain exposing (Chain)
import Data.ERC20 as ERC20 exposing (ERC20)
import Sort exposing (Sorter)


type Token
    = ETH
    | ERC20 ERC20


fromAssetFragment : Chain -> String -> Maybe Token
fromAssetFragment chain string =
    string
        |> String.split "="
        |> (\list ->
                case list of
                    "asset" :: "ETH" :: _ ->
                        Just ETH

                    "asset" :: address :: _ ->
                        address
                            |> ERC20.fromString chain
                            |> Maybe.map ERC20

                    _ ->
                        Nothing
           )


fromCollateralFragment : Chain -> String -> Maybe Token
fromCollateralFragment chain string =
    string
        |> String.split "="
        |> (\list ->
                case list of
                    "collateral" :: "ETH" :: _ ->
                        Just ETH

                    "collateral" :: address :: _ ->
                        address
                            |> ERC20.fromString chain
                            |> Maybe.map ERC20

                    _ ->
                        Nothing
           )


toName : Token -> String
toName token =
    case token of
        ETH ->
            "Ethereum"

        ERC20 erc20 ->
            erc20 |> ERC20.toName


toSymbol : Token -> String
toSymbol token =
    case token of
        ETH ->
            "ETH"

        ERC20 erc20 ->
            erc20 |> ERC20.toSymbol


toAssetFragment : Token -> String
toAssetFragment token =
    case token of
        ETH ->
            "asset=ETH"

        ERC20 erc20 ->
            erc20
                |> ERC20.toString
                |> (++) "asset="


toCollateralFragment : Token -> String
toCollateralFragment token =
    case token of
        ETH ->
            "collateral=ETH"

        ERC20 erc20 ->
            erc20
                |> ERC20.toString
                |> (++) "collateral="


compare : Chain -> Token -> Token -> Order
compare chain token1 token2 =
    case ( token1, token2 ) of
        ( ETH, ETH ) ->
            EQ

        ( ETH, ERC20 _ ) ->
            GT

        ( ERC20 _, ETH ) ->
            LT

        ( ERC20 erc20a, ERC20 erc20b ) ->
            ERC20.compare chain erc20a erc20b


sorter : Chain -> Sorter Token
sorter chain =
    compare chain
        |> Sort.custom
