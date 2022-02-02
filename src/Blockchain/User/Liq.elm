module Blockchain.User.Liq exposing (Liq, decoder, encode, isZero)

import Data.Uint as Uint exposing (Uint)
import Json.Decode exposing (Decoder)
import Json.Encode exposing (Value)


type alias Liq =
    Uint


decoder : Decoder Liq
decoder =
    Uint.decoder


encode : Liq -> Value
encode =
    Uint.encode


isZero : Liq -> Bool
isZero =
    Uint.isZero
