module Blockchain.User.Claim exposing (Claim, decoder)

import Data.Uint as Uint exposing (Uint)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline


type alias Claim =
    { bond : Uint
    , insurance : Uint
    }


decoder : Decoder Claim
decoder =
    Decode.succeed Claim
        |> Pipeline.required "bond" Uint.decoder
        |> Pipeline.required "insurance" Uint.decoder
