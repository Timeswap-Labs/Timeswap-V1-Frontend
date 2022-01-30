module Modal.PayTransaction.Query exposing
    ( Custom
    , Full
    , decoderCustom
    , decoderFull
    , givenCustom
    , givenFull
    )

import Blockchain.User.Due as Due exposing (Due)
import Blockchain.User.TokenId as TokenId exposing (TokenId)
import Data.Chain as Chain exposing (Chain)
import Data.Pool as Pool exposing (Pool)
import Data.Uint as Uint exposing (Uint)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode exposing (Value)
import Modal.PayTransaction.Error as Error exposing (Error)
import Modal.PayTransaction.Total as Total exposing (Total)
import Sort.Dict as Dict exposing (Dict)


type alias Full =
    { chain : Chain
    , pool : Pool
    , dues : Dict TokenId Due
    }


type alias Custom =
    { chain : Chain
    , pool : Pool
    , dues : Dict TokenId Due
    , assetsIn : Dict TokenId Uint
    }


type alias FullAnswer =
    { chain : Chain
    , pool : Pool
    , dues : Dict TokenId Due
    , result : Result Error Total
    }


type alias CustomAnswer =
    { chain : Chain
    , pool : Pool
    , dues : Dict TokenId Due
    , assetsIn : Dict TokenId Uint
    , result : CustomResult
    }


type alias CustomResult =
    { collateralsOut : Dict TokenId (Result Error Uint)
    , total : Result Error Total
    }


givenFull : Full -> Value
givenFull { chain, pool, dues } =
    [ ( "chain", chain |> Chain.encode )
    , ( "pool", pool |> Pool.encode )
    , ( "dues", dues |> Due.encodeMultiple )
    ]
        |> Encode.object


givenCustom : Custom -> Value
givenCustom { chain, pool, dues, assetsIn } =
    [ ( "chain", chain |> Chain.encode )
    , ( "pool", pool |> Pool.encode )
    , ( "dues", dues |> Due.encodeMultiple )
    , ( "assetsIn", assetsIn |> encodeAssetsIn )
    ]
        |> Encode.object


encodeAssetsIn : Dict TokenId Uint -> Value
encodeAssetsIn assetsIn =
    assetsIn
        |> Dict.toList
        |> Encode.list encodeAssetIn


encodeAssetIn : ( TokenId, Uint ) -> Value
encodeAssetIn ( tokenId, assetIn ) =
    [ ( "tokenId", tokenId |> TokenId.encode )
    , ( "assetIn", assetIn |> Uint.encode )
    ]
        |> Encode.object


decoderFull : Decoder FullAnswer
decoderFull =
    Decode.succeed FullAnswer
        |> Pipeline.required "chain" Chain.decoder
        |> Pipeline.required "pool" Pool.decoder
        |> Pipeline.required "dues" Due.decoderMultiple
        |> Pipeline.required "result"
            ([ Total.decoder |> Decode.map Ok
             , Error.decoder |> Decode.map Err
             ]
                |> Decode.oneOf
            )


decoderCustom : Decoder CustomAnswer
decoderCustom =
    Decode.succeed CustomAnswer
        |> Pipeline.required "chain" Chain.decoder
        |> Pipeline.required "pool" Pool.decoder
        |> Pipeline.required "dues" Due.decoderMultiple
        |> Pipeline.required "assetsIn" decoderAssetsIn
        |> Pipeline.required "result" decoderCustomResult


decoderAssetsIn : Decoder (Dict TokenId Uint)
decoderAssetsIn =
    Decode.succeed Tuple.pair
        |> Pipeline.required "tokenId" TokenId.decoder
        |> Pipeline.required "assetIn" Uint.decoder
        |> Decode.list
        |> Decode.map (Dict.fromList TokenId.sorter)


decoderCustomResult : Decoder CustomResult
decoderCustomResult =
    Decode.succeed CustomResult
        |> Pipeline.required "collateralsOut" decoderCollateralsOut
        |> Pipeline.required "total"
            ([ Total.decoder |> Decode.map Ok
             , Error.decoder |> Decode.map Err
             ]
                |> Decode.oneOf
            )


decoderCollateralsOut : Decoder (Dict TokenId (Result Error Uint))
decoderCollateralsOut =
    Decode.succeed Tuple.pair
        |> Pipeline.required "tokenId" TokenId.decoder
        |> Pipeline.required "collateralOut"
            ([ Uint.decoder |> Decode.map Ok
             , Error.decoder |> Decode.map Err
             ]
                |> Decode.oneOf
            )
        |> Decode.list
        |> Decode.map (Dict.fromList TokenId.sorter)
