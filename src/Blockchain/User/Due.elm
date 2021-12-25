module Blockchain.User.Due exposing (Due, decoder)

import Data.Uint as Uint exposing (Uint)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline


type alias Due =
    { debt : Uint
    , collateral : Uint
    }


decoder : Decoder Due
decoder =
    Decode.succeed Due
        |> Pipeline.required "debt" Uint.decoder
        |> Pipeline.required "collateral" Uint.decoder
