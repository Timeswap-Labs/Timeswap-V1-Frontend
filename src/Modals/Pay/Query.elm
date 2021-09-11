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
import Sort.Set as Set exposing (Set)


type alias Query =
    { pool : Pool
    , ids : Set TokenId
    , dues : Maybe Dues
    }


type alias Dues =
    { assetIn : Uint
    , collateralOut : Uint
    }


decoder : Pools -> Tokens -> Decoder Query
decoder pools tokens =
    Decode.succeed Query
        |> Pipeline.custom (Pools.decoderPool pools tokens)
        |> Pipeline.required "ids"
            (TokenId.decoder
                |> Decode.list
                |> Decode.map (Set.fromList TokenId.sorter)
            )
        |> Pipeline.custom
            (Decode.succeed Dues
                |> Pipeline.required "assetIn" Uint.decoder
                |> Pipeline.required "collateralOut" Uint.decoder
                |> Decode.nullable
            )


givenTokenIds : Positions -> Pool -> Set TokenId -> Maybe Value
givenTokenIds positions pool ids =
    if positions |> Positions.isOwner pool ids then
        [ ( "asset", pool.pair |> Pair.toAsset |> Token.encode )
        , ( "collateral", pool.pair |> Pair.toAsset |> Token.encode )
        , ( "maturity", pool.maturity |> Maturity.encode )
        , ( "ids"
          , ids
                |> Set.toList
                |> Encode.list TokenId.encode
          )
        ]
            |> Encode.object
            |> Just

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
