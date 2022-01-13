module Modal.MaturityList.Query exposing (toUrlString)

import Data.Chain as Chain exposing (Chain)
import Data.Pair as Pair exposing (Pair)
import Data.Token as Token
import Data.TokenParam as TokenParam
import Url.Builder as Builder


toUrlString : Chain -> Pair -> String
toUrlString chain pair =
    Builder.crossOrigin "https://ts-gamification-api.herokuapp.com/v1/pool/summary"
        []
        [ chain
            |> Chain.toQueryParameter
        , pair
            |> Pair.toAsset
            |> Token.toQueryParameter TokenParam.Asset
        , pair
            |> Pair.toCollateral
            |> Token.toQueryParameter TokenParam.Collateral
        ]
