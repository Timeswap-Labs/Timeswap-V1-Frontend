module Data.Tokens exposing (Tokens, example, fromAssetFragment, fromCollateralFragment)

import Data.ERC20 as ERC20
import Data.Token as Token exposing (Token)
import Sort.Set as Set exposing (Set)


type alias Tokens =
    Set Token


fromAssetFragment : Tokens -> String -> Maybe Token
fromAssetFragment tokens string =
    string
        |> String.split "="
        |> (\list ->
                case list of
                    "asset" :: "ETH" :: _ ->
                        Just Token.ETH

                    "asset" :: address :: _ ->
                        tokens
                            |> Set.foldl
                                (\token accumulator ->
                                    case token of
                                        Token.ETH ->
                                            accumulator

                                        Token.ERC20 erc20 ->
                                            accumulator
                                                |> Set.insert erc20
                                )
                                (Set.empty ERC20.sorter)
                            |> (\set ->
                                    address
                                        |> ERC20.fromString set
                                        |> Maybe.map Token.ERC20
                               )

                    _ ->
                        Nothing
           )


fromCollateralFragment : Tokens -> String -> Maybe Token
fromCollateralFragment tokens string =
    string
        |> String.split "="
        |> (\list ->
                case list of
                    "collateral" :: "ETH" :: _ ->
                        Just Token.ETH

                    "collateral" :: address :: _ ->
                        tokens
                            |> Set.foldl
                                (\token accumulator ->
                                    case token of
                                        Token.ETH ->
                                            accumulator

                                        Token.ERC20 erc20 ->
                                            accumulator
                                                |> Set.insert erc20
                                )
                                (Set.empty ERC20.sorter)
                            |> (\set ->
                                    address
                                        |> ERC20.fromString set
                                        |> Maybe.map Token.ERC20
                               )

                    _ ->
                        Nothing
           )


example : Tokens
example =
    ERC20.example
        |> Set.map Token.sorter (\erc20 -> Token.ERC20 erc20)
        |> Set.insert Token.ETH
