module Blockchain.User.TokenId exposing (TokenId, decoder, dummy, encode, sorter, toString)

import Data.Uint as Uint exposing (Uint)
import Json.Decode exposing (Decoder)
import Json.Encode exposing (Value)
import Sort exposing (Sorter)
import Sort.Set as Set exposing (Set)


type alias TokenId =
    Uint


dummy : Set TokenId
dummy =
    Set.fromList Uint.sorter [ Uint.dummy, Uint.dummy2 ]


encode : TokenId -> Value
encode =
    Uint.encode


decoder : Decoder TokenId
decoder =
    Uint.decoder


sorter : Sorter TokenId
sorter =
    Uint.sorter


toString : TokenId -> String
toString =
    Uint.toString
