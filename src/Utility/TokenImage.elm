module Utility.TokenImage exposing (getIcon)

import Data.Token as Token exposing (Token)
import Element exposing (Attribute, Element, image)


getIcon : List (Attribute msg) -> Token -> Element msg
getIcon attribute token =
    case token of
        Token.ETH ->
            eth attribute

        Token.ERC20 erc20 ->
            eth attribute


weth : List (Attribute msg) -> Element msg
weth attributes =
    image
        attributes
        { src = "./../../image/tokens/WETH.svg"
        , description = "WETH Token"
        }


matic : List (Attribute msg) -> Element msg
matic attributes =
    image
        attributes
        { src = "./../../image/tokens/MATIC.svg"
        , description = "MATIC Token"
        }


dai : List (Attribute msg) -> Element msg
dai attributes =
    image
        attributes
        { src = "./../../image/tokens/DAI.svg"
        , description = "DAI Token"
        }


btc : List (Attribute msg) -> Element msg
btc attributes =
    image
        attributes
        { src = "./../../image/tokens/BIC.svg"
        , description = "BTC Token"
        }


eth : List (Attribute msg) -> Element msg
eth attributes =
    image
        attributes
        { src = "./../../image/tokens/ETH.svg"
        , description = "ETH Token"
        }


usdt : List (Attribute msg) -> Element msg
usdt attributes =
    image
        attributes
        { src = "./../../image/tokens/USDT.svg"
        , description = "USDT Token"
        }


usdc : List (Attribute msg) -> Element msg
usdc attributes =
    image
        attributes
        { src = "./../../image/tokens/USDC.svg"
        , description = "USDC Token"
        }
