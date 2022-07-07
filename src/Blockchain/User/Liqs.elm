module Blockchain.User.Liqs exposing (Liqs, decoder, filterEmptyLiqs, toERC20s, toList)

import Blockchain.User.Liq as Liq exposing (Liq)
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
import Sort.Set as Set
import Time exposing (Posix)


type alias Liqs =
    Dict Address (Dict Pool Liq)


toList :
    Posix
    -> Liqs
    -> List ( Address, ( Pool, Liq ) )
toList posix liqs =
    liqs
        |> Dict.map
            (\_ dict ->
                dict
                    |> Dict.dropIf (\_ liq -> liq |> Liq.isZero)
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
            (\( convAddress, poolLiqList ) acc ->
                acc
                    |> List.append
                        (poolLiqList |> List.map (\poolLiqTuple -> ( convAddress, poolLiqTuple )))
            )
            []


decoder : Chain -> Chains -> Decoder Liqs
decoder chain chains =
    Decode.succeed Tuple.pair
        |> Pipeline.required "convAddress" Address.decoder
        |> Pipeline.required "pools" (poolLiqsDecoder chain chains)
        |> Decode.list
        |> Decode.map (Dict.fromList Address.sorter)


poolLiqsDecoder : Chain -> Chains -> Decoder (Dict Pool Liq)
poolLiqsDecoder chain chains =
    Decode.succeed Tuple.pair
        |> Pipeline.required "pool" Pool.decoder
        |> Pipeline.required "liq" Liq.decoder
        |> Decode.list
        |> (Decode.map << List.map << Tuple.mapFirst) (Pool.toNative chain chains)
        |> Decode.map (Dict.fromList Pool.sorter)


toERC20s : Liqs -> ERC20s
toERC20s liqs =
    liqs
        |> Dict.values
        |> List.map
            (\dict ->
                dict
                    |> Dict.dropIf (\_ liq -> liq |> Liq.isZero)
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


filterEmptyLiqs : Liqs -> Liqs
filterEmptyLiqs liqs =
    liqs
        |> Dict.map
            (\_ dict ->
                dict |> Dict.dropIf (\_ liq -> liq |> Liq.isZero)
            )
