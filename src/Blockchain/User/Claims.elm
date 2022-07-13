module Blockchain.User.Claims exposing
    ( Claims
    , decoder
    , filterEmptyClaims
    , toERC20s
    , toList
    )

import Blockchain.User.Claim as Claim exposing (Claim)
import Data.Address as Address exposing (Address)
import Data.Chain exposing (Chain)
import Data.Chains exposing (Chains)
import Data.ERC20 as ERC20
import Data.ERC20s exposing (ERC20s)
import Data.Maturity as Maturity
import Data.Pair as Pair
import Data.Pool as Pool exposing (Pool)
import Data.Token as Token
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Modal.MaturityList.Sorting exposing (Sorting(..))
import Sort.Dict as Dict exposing (Dict)
import Sort.Set as Set
import Time exposing (Posix)


type alias Claims =
    Dict Address (Dict Pool Claim)


toList :
    Posix
    -> Claims
    -> List ( Address, ( Pool, Claim ) )
toList posix claims =
    claims
        |> Dict.map
            (\_ dict ->
                dict
                    |> Dict.dropIf
                        (\_ claim -> claim |> Claim.isZero)
                    |> Dict.partition
                        (\{ maturity } _ ->
                            maturity |> Maturity.isActive posix
                        )
                    |> Tuple.mapBoth Dict.toList Dict.toList
                    |> (\( active, matured ) ->
                            [ matured
                            , active
                            ]
                                |> List.concat
                       )
            )
        |> Dict.toList
        |> List.foldl
            (\( convAddress, poolClaimList ) acc ->
                acc
                    |> List.append
                        (poolClaimList |> List.map (\poolClaimTuple -> ( convAddress, poolClaimTuple )))
            )
            []


decoder : Chain -> Chains -> Decoder Claims
decoder chain chains =
    Decode.succeed Tuple.pair
        |> Pipeline.required "convAddress" Address.decoder
        |> Pipeline.required "pools" (poolClaimsDecoder chain chains)
        |> Decode.list
        |> Decode.map (Dict.fromList Address.sorter)


poolClaimsDecoder : Chain -> Chains -> Decoder (Dict Pool Claim)
poolClaimsDecoder chain chains =
    Decode.succeed Tuple.pair
        |> Pipeline.required "pool" Pool.decoder
        |> Pipeline.required "claim" Claim.decoder
        |> Decode.list
        |> (Decode.map << List.map << Tuple.mapFirst) (Pool.toNative chain chains)
        |> Decode.map (Dict.fromList Pool.sorter)


toERC20s : Claims -> ERC20s
toERC20s claims =
    claims
        |> Dict.values
        |> List.map
            (\dict ->
                dict
                    |> Dict.dropIf
                        (\_ claim -> claim |> Claim.isZero)
                    |> Dict.keys
                    |> List.concatMap
                        (\pool ->
                            []
                                |> (++)
                                    (pool.pair
                                        |> Pair.toAsset
                                        |> Token.toERC20
                                        |> Maybe.map List.singleton
                                        |> Maybe.withDefault []
                                    )
                                |> (++)
                                    (pool.pair
                                        |> Pair.toCollateral
                                        |> Token.toERC20
                                        |> Maybe.map List.singleton
                                        |> Maybe.withDefault []
                                    )
                        )
                    |> Set.fromList ERC20.sorter
            )
        |> List.foldl
            (\erc20Set acc ->
                Set.union ERC20.sorter erc20Set acc
            )
            (Set.empty ERC20.sorter)


filterEmptyClaims : Claims -> Claims
filterEmptyClaims claims =
    claims
        |> Dict.map
            (\_ dict ->
                dict
                    |> Dict.dropIf
                        (\_ claim ->
                            claim
                                |> Claim.isZero
                        )
            )
