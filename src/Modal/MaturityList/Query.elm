module Modal.MaturityList.Query exposing (toUrlString)

import Blockchain.Main as Blockchain exposing (Blockchain)
import Data.Chain as Chain
import Data.Pair as Pair exposing (Pair)
import Data.Token as Token
import Data.TokenParam as TokenParam
import Url.Builder as Builder


toUrlString : Blockchain -> Pair -> String
toUrlString blockchain pair =
    Builder.crossOrigin "https://api.timeswap.io"
        []
        [ blockchain
            |> Blockchain.toChain
            |> Chain.toQueryParameter
        , pair
            |> Pair.toAsset
            |> Token.toQueryParameter TokenParam.Asset
        , pair
            |> Pair.toCollateral
            |> Token.toQueryParameter TokenParam.Collateral
        ]
