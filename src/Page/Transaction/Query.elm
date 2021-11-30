module Page.Transaction.Query exposing (toUrlString)

import Blockchain.Main as Blockchain exposing (Blockchain)
import Data.Chain as Chain
import Data.Maturity as Maturity
import Data.Pair as Pair
import Data.Pool exposing (Pool)
import Data.Token as Token
import Data.TokenParam as TokenParam
import Url.Builder as Builder


toUrlString : Blockchain -> Pool -> String
toUrlString blockchain { pair, maturity } =
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
        , maturity
            |> Maturity.toQueryParameter
        ]
