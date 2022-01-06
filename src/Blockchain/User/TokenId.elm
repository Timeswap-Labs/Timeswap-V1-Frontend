module Blockchain.User.TokenId exposing (TokenId, decoder, sorter)

import Data.Uint as Uint exposing (Uint)
import Json.Decode exposing (Decoder)
import Sort exposing (Sorter)


type alias TokenId =
    Uint


decoder : Decoder TokenId
decoder =
    Uint.decoder


sorter : Sorter TokenId
sorter =
    Uint.sorter
