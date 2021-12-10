module Page.Transaction.Answer exposing (Answer, decoder)

import Json.Decode as Decode exposing (Decoder)
import Page.Transaction.PoolInfo as PoolInfo exposing (PoolInfo)


type alias Answer =
    Maybe PoolInfo


decoder : Decoder Answer
decoder =
    PoolInfo.decoder |> Decode.nullable
