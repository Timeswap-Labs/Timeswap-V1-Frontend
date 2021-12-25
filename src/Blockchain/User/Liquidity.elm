module Blockchain.User.Liquidity exposing (Liquidity, decoder)

import Data.Uint as Uint exposing (Uint)
import Json.Decode exposing (Decoder)


type alias Liquidity =
    Uint


decoder : Decoder Liquidity
decoder =
    Uint.decoder
