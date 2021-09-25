module Utility.OpenSea exposing (..)

import Data.Address as Address exposing (Address)
import Data.Chain exposing (Chain(..))
import Data.TokenId as TokenId exposing (TokenId)


url : Chain -> Address -> TokenId -> String
url chain address tokenId =
    [ "https:/"
    , case chain of
        Mainnet ->
            "opensea.io"

        Rinkeby ->
            "testnets.opensea.io"
    , "assets"
    , address |> Address.toString |> String.toLower
    , tokenId |> TokenId.toString
    ]
        |> String.join "/"
