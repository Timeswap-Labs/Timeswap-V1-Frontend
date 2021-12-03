module Modal.Connect.Etherscan exposing (toUrlString)

import Data.Address as Address exposing (Address)
import Data.Chain as Chain exposing (Chain)
import Url.Builder as Builder


toUrlString : Chain -> Maybe String -> Address -> String
toUrlString chain name address =
    Builder.crossOrigin
        (chain |> Chain.toEtherscan)
        [ "address"
        , name
            |> Maybe.withDefault
                (address |> Address.toString)
        ]
        []
