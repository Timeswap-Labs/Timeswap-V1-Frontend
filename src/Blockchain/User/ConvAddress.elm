module Blockchain.User.ConvAddress exposing (ConvAddress, decoder, toUrlString)

import Data.Address as Address exposing (Address)
import Data.Chain as Chain exposing (Chain)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Url.Builder as Builder


type alias ConvAddress =
    { convAddress : Address
    }


toUrlString : Chain -> String
toUrlString chain =
    Builder.crossOrigin "https://ts-gamification-api.herokuapp.com/v1"
        [ "convaddress" ]
        [ chain |> Chain.toQueryParameter ]


decoder : Decoder ConvAddress
decoder =
    Decode.succeed ConvAddress
        |> Pipeline.required "convAddress" Address.decoder
