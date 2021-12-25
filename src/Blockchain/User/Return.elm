module Blockchain.User.Return exposing (Return, decoder)

import Data.Uint as Uint exposing (Uint)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline


type alias Return =
    { asset : Uint
    , collateral : Uint
    }


decoder : Decoder Return
decoder =
    Decode.succeed Return
        |> Pipeline.required "asset" Uint.decoder
        |> Pipeline.required "collateral" Uint.decoder
