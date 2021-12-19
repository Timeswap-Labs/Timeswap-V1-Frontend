module Data.Settings exposing (encode)

import Data.Deadline as Deadline exposing (Deadline)
import Data.PriceFeed as PriceFeed exposing (PriceFeed)
import Data.Slippage as Slippage exposing (Slippage)
import Json.Encode as Encode exposing (Value)


encode : Slippage -> Deadline -> PriceFeed -> Value
encode slippage deadline priceFeed =
    [ ( "slippage", slippage |> Slippage.encode )
    , ( "deadline", deadline |> Deadline.encode )
    , ( "priceFeed", priceFeed |> PriceFeed.encode )
    ]
        |> Encode.object
