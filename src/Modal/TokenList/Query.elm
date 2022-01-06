module Modal.TokenList.Query exposing (toUrlString)

import Blockchain.Main as Blockchain exposing (Blockchain)
import Data.Address as Address exposing (Address)
import Data.Chain as Chain
import Url.Builder as Builder


toUrlString : Blockchain -> Address -> String
toUrlString blockchain address =
    Builder.crossOrigin "https://api.timeswap.io"
        []
        [ blockchain
            |> Blockchain.toChain
            |> Chain.toQueryParameter
        , address
            |> Address.toQueryParameter
        ]