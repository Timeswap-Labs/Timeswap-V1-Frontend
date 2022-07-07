module Blockchain.User.Natives exposing (AllNatives, Natives, decoder, decoderNatives, encode, toPoolNativesList, toUrlString)

import Data.Address as Address exposing (Address)
import Data.Chain as Chain exposing (Chain)
import Data.Chains exposing (Chains)
import Data.Pool as Pool exposing (Pool)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode exposing (Value)
import Sort.Dict as Dict exposing (Dict)
import Url.Builder as Builder


type alias Natives =
    { bondPrincipal : Address
    , bondInterest : Address
    , insurancePrincipal : Address
    , insuranceInterest : Address
    , collateralizedDebt : Address
    , liquidity : Address
    }


type alias AllNatives =
    Dict Address (Dict Pool Natives)


toUrlString : Chain -> String -> String
toUrlString chain endPoint =
    Builder.crossOrigin endPoint
        [ "allnatives" ]
        [ chain |> Chain.toQueryParameter ]


encode : Chain -> Address -> AllNatives -> Value
encode chain owner answer =
    [ ( "chain", chain |> Chain.encode )
    , ( "owner", owner |> Address.encode )
    , ( "allNatives"
      , answer
            |> Dict.toList
            |> Encode.list encodeConvData
      )
    ]
        |> Encode.object


encodeConvData : ( Address, Dict Pool Natives ) -> Value
encodeConvData ( convAddress, dictPoolNatives ) =
    [ ( "convAddress", convAddress |> Address.encode )
    , ( "nativeResponse"
      , dictPoolNatives
            |> Dict.toList
            |> Encode.list
                (\( pool, natives ) ->
                    [ ( "pool", pool |> Pool.encode )
                    , ( "natives", natives |> encodeNatives )
                    ]
                        |> Encode.object
                )
      )
    ]
        |> Encode.object


encodeNatives : Natives -> Value
encodeNatives natives =
    [ ( "bondPrincipal", natives.bondPrincipal |> Address.encode )
    , ( "bondInterest", natives.bondInterest |> Address.encode )
    , ( "insurancePrincipal", natives.insurancePrincipal |> Address.encode )
    , ( "insuranceInterest", natives.insuranceInterest |> Address.encode )
    , ( "collateralizedDebt", natives.collateralizedDebt |> Address.encode )
    , ( "liquidity", natives.liquidity |> Address.encode )
    ]
        |> Encode.object


decoder : Chain -> Chains -> Decoder AllNatives
decoder chain chains =
    Decode.succeed Tuple.pair
        |> Pipeline.required "convAddress" Address.decoder
        |> Pipeline.required "nativeResponse" (poolNativesDecoder chain chains)
        |> Decode.list
        |> Decode.map (Dict.fromList Address.sorter)


poolNativesDecoder : Chain -> Chains -> Decoder (Dict Pool Natives)
poolNativesDecoder chain chains =
    Decode.succeed Tuple.pair
        |> Pipeline.required "pool" Pool.decoder
        |> Pipeline.required "natives" decoderNatives
        |> Decode.list
        |> (Decode.map << List.map << Tuple.mapFirst) (Pool.toNative chain chains)
        |> Decode.map (Dict.fromList Pool.sorter)


decoderNatives : Decoder Natives
decoderNatives =
    Decode.succeed Natives
        |> Pipeline.required "bondPrincipal" Address.decoder
        |> Pipeline.required "bondInterest" Address.decoder
        |> Pipeline.required "insurancePrincipal" Address.decoder
        |> Pipeline.required "insuranceInterest" Address.decoder
        |> Pipeline.required "collateralizedDebt" Address.decoder
        |> Pipeline.required "liquidity" Address.decoder


toPoolNativesList : AllNatives -> List ( Pool, Natives )
toPoolNativesList allNatives =
    allNatives
        |> Dict.values
        |> List.foldl (\dict acc -> dict |> Dict.toList |> List.append acc) []
