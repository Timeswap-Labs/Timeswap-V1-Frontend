module Blockchain.User.TokenId exposing (TokenId, decoder, encode, sorter)

import Data.Uint as Uint exposing (Uint)
import Json.Decode exposing (Decoder)
import Json.Encode exposing (Value)
import Sort exposing (Sorter)


type alias TokenId =
    Uint


encode : TokenId -> Value
encode =
    Uint.encode


decoder : Decoder TokenId
decoder =
    Uint.decoder


sorter : Sorter TokenId
sorter =
    Uint.sorter
