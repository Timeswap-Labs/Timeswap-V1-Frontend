module Blockchain.User.Natives exposing (Answer, Natives, decoder, decoderNatives, encode, toUrlString)

import Data.Address as Address exposing (Address)
import Data.Chain as Chain exposing (Chain)
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


type alias Answer =
    Dict Pool Natives


toUrlString : Chain -> String
toUrlString chain =
    Builder.crossOrigin "https://ts-gamification-api.herokuapp.com/v1"
        [ "natives" ]
        [ chain |> Chain.toQueryParameter ]


encode : Chain -> Address -> Answer -> Value
encode chain owner answer =
    [ ( "chain", chain |> Chain.encode )
    , ( "owner", owner |> Address.encode )
    , ( "natives"
      , answer
            |> Dict.toList
            |> Encode.list encodeMapping
      )
    ]
        |> Encode.object


encodeMapping : ( Pool, Natives ) -> Value
encodeMapping ( pool, natives ) =
    [ ( "pool", pool |> Pool.encode )
    , ( "natives", natives |> encodeNatives )
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


decoder : Decoder Answer
decoder =
    Decode.succeed Tuple.pair
        |> Pipeline.required "pool" Pool.decoder
        |> Pipeline.required "natives" decoderNatives
        |> Decode.list
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
