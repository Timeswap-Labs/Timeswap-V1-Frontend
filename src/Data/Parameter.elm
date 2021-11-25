module Data.Parameter exposing (Parameter(..), fromFragment, inputToken, toFragment)

import Data.Chain exposing (Chain)
import Data.Chains as Chains exposing (Chains)
import Data.Maturity as Maturity
import Data.Pair as Pair exposing (Pair)
import Data.Pool as Pool exposing (Pool)
import Data.Token as Token exposing (Token)
import Data.TokenParam as TokenParam exposing (TokenParam)


type Parameter
    = Asset Token
    | Collateral Token
    | Pair Pair
    | Pool Pool


toFragment : Parameter -> String
toFragment parameter =
    case parameter of
        Asset token ->
            token
                |> Token.toFragmentAsset

        Collateral token ->
            token
                |> Token.toFragmentCollateral

        Pair pair ->
            pair
                |> Pair.toFragment

        Pool pool ->
            pool
                |> Pool.toFragment


fromFragment : Chain -> Chains -> String -> Maybe Parameter
fromFragment chain chains fragment =
    case fragment |> String.split "&" of
        assetString :: collateralString :: maturityString :: _ ->
            case
                ( assetString
                    |> Chains.fromFragment TokenParam.Asset chain chains
                , collateralString
                    |> Chains.fromFragment TokenParam.Collateral chain chains
                , maturityString
                    |> Maturity.fromFragment
                )
            of
                ( Just asset, Nothing, Nothing ) ->
                    Asset asset |> Just

                ( Nothing, Just collateral, Nothing ) ->
                    Collateral collateral |> Just

                ( Just asset, Just collateral, Nothing ) ->
                    Pair.init asset collateral
                        |> Maybe.map Pair

                ( Just asset, Just collateral, Just maturity ) ->
                    Pair.init asset collateral
                        |> Maybe.map
                            (\pair ->
                                { pair = pair
                                , maturity = maturity
                                }
                                    |> Pool
                            )

                _ ->
                    Nothing

        assetString :: collateralString :: _ ->
            case
                ( assetString
                    |> Chains.fromFragment TokenParam.Asset chain chains
                , collateralString
                    |> Chains.fromFragment TokenParam.Collateral chain chains
                )
            of
                ( Just asset, Nothing ) ->
                    Asset asset |> Just

                ( Nothing, Just collateral ) ->
                    Collateral collateral |> Just

                ( Just asset, Just collateral ) ->
                    Pair.init asset collateral
                        |> Maybe.map Pair

                _ ->
                    Nothing

        tokenString :: _ ->
            tokenString
                |> Chains.fromFragment TokenParam.Asset chain chains
                |> Maybe.map (Asset >> Just)
                |> Maybe.withDefault
                    (tokenString
                        |> Chains.fromFragment TokenParam.Collateral chain chains
                        |> Maybe.map Collateral
                    )

        _ ->
            Nothing


inputToken : TokenParam -> Token -> Maybe Parameter -> Parameter
inputToken tokenParam token parameter =
    case ( parameter, tokenParam ) of
        ( Nothing, TokenParam.Asset ) ->
            Asset token

        ( Nothing, TokenParam.Collateral ) ->
            Collateral token

        ( Just (Asset _), TokenParam.Asset ) ->
            Asset token

        ( Just (Asset asset), TokenParam.Collateral ) ->
            Pair.init asset token
                |> Maybe.map Pair
                |> Maybe.withDefault
                    (Collateral token)

        ( Just (Collateral collateral), TokenParam.Asset ) ->
            Pair.init token collateral
                |> Maybe.map Pair
                |> Maybe.withDefault
                    (Asset token)

        ( Just (Collateral _), TokenParam.Collateral ) ->
            Collateral token

        ( Just (Pair pair), TokenParam.Asset ) ->
            pair
                |> Pair.toCollateral
                |> Pair.init token
                |> Maybe.map Pair
                |> Maybe.withDefault
                    (pair
                        |> Pair.opposite
                        |> Pair
                    )

        ( Just (Pair pair), TokenParam.Collateral ) ->
            pair
                |> Pair.toAsset
                |> (\asset -> Pair.init asset token)
                |> Maybe.map Pair
                |> Maybe.withDefault
                    (pair
                        |> Pair.opposite
                        |> Pair
                    )

        ( Just (Pool pool), TokenParam.Asset ) ->
            if (pool.pair |> Pair.toAsset) == token then
                Pool pool

            else
                pool.pair
                    |> Pair.toCollateral
                    |> Pair.init token
                    |> Maybe.map Pair
                    |> Maybe.withDefault
                        (pool.pair
                            |> Pair.opposite
                            |> Pair
                        )

        ( Just (Pool pool), TokenParam.Collateral ) ->
            if (pool.pair |> Pair.toCollateral) == token then
                Pool pool

            else
                pool.pair
                    |> Pair.toAsset
                    |> (\asset -> Pair.init asset token)
                    |> Maybe.map Pair
                    |> Maybe.withDefault
                        (pool.pair
                            |> Pair.opposite
                            |> Pair
                        )
