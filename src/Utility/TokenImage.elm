module Utility.TokenImage exposing (icon)

import Data.ERC20 as ERC20
import Data.Token as Token exposing (Token)
import Data.TokenImages exposing (TokenImages)
import Element exposing (Attribute, Element, image)
import Sort.Dict as Dict


icon : TokenImages -> List (Attribute msg) -> Token -> Element msg
icon dict attributes token =
    (case token of
        Token.ETH ->
            "ETH"

        Token.ERC20 erc20 ->
            erc20 |> ERC20.toSymbol
    )
        |> (\symbol ->
                image
                    attributes
                    { src =
                        dict
                            |> Dict.get symbol
                            |> Maybe.withDefault ""
                    , description = symbol
                    }
           )
