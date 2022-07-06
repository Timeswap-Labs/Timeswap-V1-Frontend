module Blockchain.User.Due exposing
    ( Due
    , decoder
    , decoderMultiple
    , dropZero
    , encode
    , encodeMultiple
    )

import Blockchain.User.TokenId as TokenId exposing (TokenId)
import Data.Uint as Uint exposing (Uint)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode exposing (Value)
import Sort.Dict as Dict exposing (Dict)


type alias Due =
    { debt : Uint
    , collateral : Uint
    }


encode : Due -> Value
encode { debt, collateral } =
    [ ( "debt", debt |> Uint.encode )
    , ( "collateral", collateral |> Uint.encode )
    ]
        |> Encode.object


encodeMultiple : Dict TokenId Due -> Value
encodeMultiple dict =
    dict
        |> Dict.toList
        |> Encode.list encodeSingle


encodeSingle : ( TokenId, Due ) -> Value
encodeSingle ( tokenId, due ) =
    [ ( "tokenId", tokenId |> TokenId.encode )
    , ( "due", due |> encode )
    ]
        |> Encode.object


decoder : Decoder Due
decoder =
    Decode.succeed Due
        |> Pipeline.required "debt" Uint.decoder
        |> Pipeline.required "collateral" Uint.decoder


decoderMultiple : Decoder (Dict TokenId Due)
decoderMultiple =
    Decode.succeed Tuple.pair
        |> Pipeline.required "tokenId" TokenId.decoder
        |> Pipeline.required "due" decoder
        |> Decode.list
        |> Decode.map (Dict.fromList TokenId.sorter)


isZeroSingle : Due -> Bool
isZeroSingle { collateral } =
    collateral |> Uint.isZero


dropZero : Dict TokenId Due -> Dict TokenId Due
dropZero dict =
    dict
        |> Dict.dropIf (\_ due -> due |> isZeroSingle)
