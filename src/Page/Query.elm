module Page.Query exposing (toUrlString)

import Data.Chain as Chain exposing (Chain)
import Data.Maturity as Maturity
import Data.Pair as Pair
import Data.Pool exposing (Pool)
import Data.Token as Token
import Data.TokenParam as TokenParam
import Url.Builder as Builder


toUrlString : Chain -> String -> Pool -> String
toUrlString chain endPoint { pair, maturity } =
    Builder.crossOrigin endPoint
        [ "pool" ]
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
