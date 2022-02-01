module Blockchain.User.Dues exposing
    ( Dues
    , decoder
    , getMultiple
    , toERC20s
    , toList
    )

import Blockchain.User.Due as Due exposing (Due)
import Blockchain.User.TokenId as TokenId exposing (TokenId)
import Data.ERC20 as ERC20
import Data.ERC20s exposing (ERC20s)
import Data.Maturity as Maturity
import Data.Pair as Pair
import Data.Pool as Pool exposing (Pool)
import Data.Token as Token
import Data.Uint as Uint
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Sort.Dict as Dict exposing (Dict)
import Sort.Set as Set exposing (Set)
import Time exposing (Posix)


type alias Dues =
    Dict Pool (Dict TokenId Due)


toList :
    Posix
    -> Dues
    -> List ( Pool, Dict TokenId Due )
toList posix dues =
    dues
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
        |> (Maybe.map << Dict.keepIf)
            (\tokenId _ ->
                tokenId
                    |> Set.memberOf tokenIds
            )


decoder : Decoder Dues
decoder =
    Decode.succeed Tuple.pair
        |> Pipeline.required "pool" Pool.decoder
        |> Pipeline.required "dues" Due.decoderMultiple
        |> Decode.list
        |> Decode.map (Dict.fromList Pool.sorter)


toERC20s : Dues -> ERC20s
toERC20s dues =
    dues
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
