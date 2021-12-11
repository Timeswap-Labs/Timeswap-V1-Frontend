module Page.Transaction.Answer exposing (Answer, decoder)

import Data.Or exposing (Or(..))
import Data.Uint as Uint exposing (Uint)
import Json.Decode as Decode exposing (Decoder)
import Page.Transaction.PoolInfo as PoolInfo exposing (PoolInfo)


type alias Answer =
    Or (Maybe Uint) PoolInfo


decoder : Decoder Answer
decoder =
    [ PoolInfo.decoder |> Decode.map Right
    , Uint.decoder
        |> Decode.nullable
        |> Decode.map Left
    ]
        |> Decode.oneOf
