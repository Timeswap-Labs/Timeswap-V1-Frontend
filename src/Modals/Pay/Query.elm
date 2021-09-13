module Modals.Pay.Query exposing (Query, decoder, givenTokenIds, updateQuery)

import Data.Maturity as Maturity
import Data.Pair as Pair exposing (Pair)
import Data.Pool exposing (Pool)
import Data.Pools as Pools exposing (Pools)
import Data.Positions as Positions exposing (Positions)
import Data.Remote exposing (Remote(..))
import Data.Token as Token
import Data.TokenId as TokenId exposing (TokenId)
import Data.Tokens exposing (Tokens)
import Data.Uint as Uint exposing (Uint)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode exposing (Value)
import Modals.Pay.DuesIn exposing (DuesIn)
import Sort.Dict as Dict exposing (Dict)
import Sort.Set as Set exposing (Set)


type alias Query =
    { pool : Pool
    , dues : Dict TokenId Positions.DueUint
    , totalDues : Maybe Dues
    }


type alias Dues =
    { assetIn : Uint
    , collateralOut : Uint
    }


decoder : Pools -> Tokens -> Decoder Query
decoder pools tokens =
    Decode.succeed Query
        |> Pipeline.custom (Pools.decoderPool pools tokens)
        |> Pipeline.custom decoderDues
        |> Pipeline.custom
            (Decode.oneOf
                [ decoderTotalDues |> Decode.map Just
                , Decode.succeed Nothing
                ]
            )


decoderDues : Decoder (Dict TokenId Positions.DueUint)
decoderDues =
    Decode.succeed Tuple.pair
        |> Pipeline.required "id" TokenId.decoder
        |> Pipeline.custom
            (Decode.succeed Positions.DueUint
                |> Pipeline.required "debt" Uint.decoder
                |> Pipeline.required "collateral" Uint.decoder
            )
        |> Decode.list
        |> Decode.map (Dict.fromList TokenId.sorter)


decoderTotalDues : Decoder Dues
decoderTotalDues =
    Decode.succeed Dues
        |> Pipeline.required "assetIn" Uint.decoder
        |> Pipeline.required "collateralOut" Uint.decoder


givenTokenIds : Positions -> Pool -> Set TokenId -> Maybe Value
givenTokenIds positions pool ids =
    if positions |> Positions.isOwner pool ids then
        positions
            |> Positions.toDueQuery pool ids
            |> Maybe.map
                (\dict ->
                    dict
                        |> Dict.toList
                        |> List.map
                            (\( tokenId, { debt, collateral } ) ->
                                [ ( "id", tokenId |> TokenId.encode )
                                , ( "debt", debt |> Uint.encode )
                                , ( "collateral", collateral |> Uint.encode )
                                ]
                            )
                        |> Encode.list Encode.object
                        |> (\dues ->
                                [ ( "asset", pool.pair |> Pair.toAsset |> Token.encode )
                                , ( "collateral", pool.pair |> Pair.toCollateral |> Token.encode )
                                , ( "maturity", pool.maturity |> Maturity.encode )
                                , ( "dues", dues )
                                ]
                                    |> Encode.object
                           )
                )

    else
        Nothing


updateQuery :
    { modal | pool : { pool | pair : Pair } }
    -> Maybe Dues
    -> DuesIn
updateQuery { pool } maybeDues =
    maybeDues
        |> Maybe.map
            (\{ assetIn, collateralOut } ->
                { assetIn =
                    assetIn
                        |> Uint.toAmount (pool.pair |> Pair.toAsset)
                , collateralOut =
                    collateralOut
                        |> Uint.toAmount (pool.pair |> Pair.toCollateral)
                }
                    |> Success
            )
        |> Maybe.withDefault (Failure ())
