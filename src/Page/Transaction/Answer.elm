module Page.Transaction.Answer exposing (Answer, decoder)

import Data.Or exposing (Or(..))
import Json.Decode as Decode exposing (Decoder)
import Page.Transaction.PoolInfo as PoolInfo exposing (PoolInfo)
import Page.Transaction.SpotPrice as SpotPrice exposing (SpotPrice)


type alias Answer =
    Or SpotPrice PoolInfo


decoder : Decoder Answer
decoder =
    [ PoolInfo.decoder |> Decode.map Right
    , SpotPrice.decoder |> Decode.map Left
    ]
        |> Decode.oneOf
