module Blockchain.User.Due exposing (Due, decoder, encode)

import Data.Uint as Uint exposing (Uint)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode exposing (Value)


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


decoder : Decoder Due
decoder =
    Decode.succeed Due
        |> Pipeline.required "debt" Uint.decoder
        |> Pipeline.required "collateral" Uint.decoder
