module Page.Transaction.Query exposing (toUrlString)

import Data.Chain as Chain exposing (Chain)
import Data.Maturity as Maturity
import Data.Pair as Pair
import Data.Pool exposing (Pool)
import Data.Token as Token
import Data.TokenParam as TokenParam
import Url.Builder as Builder


toUrlString : Chain -> Pool -> String
toUrlString chain { pair, maturity } =
    Builder.crossOrigin "https://api.timeswap.io"
        []
        [ chain |> Chain.toQueryParameter
        , pair
            |> Pair.toAsset
            |> Token.toQueryParameter TokenParam.Asset
        , pair
            |> Pair.toCollateral
            |> Token.toQueryParameter TokenParam.Collateral
        , maturity
            |> Maturity.toQueryParameter
        ]
