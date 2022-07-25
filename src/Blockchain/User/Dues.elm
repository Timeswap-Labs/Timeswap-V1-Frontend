module Blockchain.User.Dues exposing
    ( Dues
    , decoder
    , filterEmptyDues
    , getMultiple
    , getSingle
    , toERC20s
    , toList
    )

import Blockchain.User.Due as Due exposing (Due)
import Blockchain.User.TokenId exposing (TokenId)
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
import Sort.Dict as Dict exposing (Dict)
import Sort.Set as Set exposing (Set)
import Time exposing (Posix)


type alias Dues =
    Dict Pool ( Dict TokenId Due, Address )


toList :
    Posix
    -> Dues
    -> List ( Pool, Dict TokenId Due )
toList posix dues =
    dues
        |> Dict.map (\_ tuple -> tuple |> Tuple.first |> Due.dropZero)
        |> Dict.dropIf (\_ dict -> dict |> Dict.isEmpty)
        |> Dict.partition
            (\{ maturity } _ ->
                maturity |> Maturity.isActive posix
            )
        |> Tuple.mapBoth Dict.toList Dict.toList
        |> (\( active, matured ) ->
                [ active
                , matured
                ]
                    |> List.concat
           )


getMultiple : Pool -> Set TokenId -> Dues -> Maybe (Dict TokenId Due)
getMultiple pool tokenIds dues =
    dues
        |> Dict.get pool
        |> Maybe.map (\tuple -> tuple |> Tuple.first)
        |> (Maybe.map << Dict.keepIf)
            (\tokenId _ ->
                tokenId
                    |> Set.memberOf tokenIds
            )


getSingle : Pool -> TokenId -> Dues -> Maybe Due
getSingle pool tokenId dues =
    dues
        |> Dict.get pool
        |> Maybe.map (\tuple -> tuple |> Tuple.first)
        |> Maybe.map (Dict.get tokenId)
        |> Maybe.withDefault Nothing


decoder : Chain -> Chains -> Decoder Dues
decoder chain chains =
    Decode.succeed Tuple.pair
        |> Pipeline.required "pool" Pool.decoder
        |> Pipeline.custom decoderTuple
        |> Decode.list
        |> (Decode.map << List.map << Tuple.mapFirst) (Pool.toNative chain chains)
        |> Decode.map (Dict.fromList Pool.sorter)


decoderTuple : Decoder ( Dict TokenId Due, Address )
decoderTuple =
    Decode.succeed Tuple.pair
        |> Pipeline.required "dues" Due.decoderMultiple
        |> Pipeline.required "collateralizedDebt" Address.decoder


toERC20s : Dues -> ERC20s
toERC20s dues =
    dues
        |> Dict.map (\_ tuple -> tuple |> Tuple.first |> Due.dropZero)
        |> Dict.dropIf (\_ dict -> dict |> Dict.isEmpty)
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


filterEmptyDues : Dues -> Dues
filterEmptyDues dues =
    dues
        |> Dict.map (\_ tuple -> tuple |> Tuple.mapFirst Due.dropZero)
        |> Dict.dropIf (\_ tuple -> tuple |> Tuple.first |> Dict.isEmpty)
