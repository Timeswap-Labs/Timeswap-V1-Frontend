module Blockchain.User.TokenId exposing (TokenId, decoder)

import Data.Uint as Uint exposing (Uint)
import Json.Decode exposing (Decoder)


type alias TokenId =
    Uint


decoder : Decoder TokenId
decoder =
    Uint.decoder
