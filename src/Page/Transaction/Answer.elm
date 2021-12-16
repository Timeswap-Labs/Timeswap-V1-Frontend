module Page.Transaction.Answer exposing (Answer, decoder)

import Data.Or exposing (Or(..))
import Json.Decode as Decode exposing (Decoder)
import Page.Transaction.PoolInfo as PoolInfo exposing (PoolInfo)
import Page.Transaction.Price as Price exposing (Price)


type alias Answer =
    Or Price PoolInfo


decoder : Decoder Answer
decoder =
    [ PoolInfo.decoder |> Decode.map Right
    , Price.decoder |> Decode.map Left
    ]
        |> Decode.oneOf
