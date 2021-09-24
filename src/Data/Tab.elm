module Data.Tab exposing (Tab(..), toName, toUrl)

import Utility.Router as Router


type Tab
    = Market
    | Dashboard
    | Liquidity


toUrl : Tab -> String
toUrl tab =
    case tab of
        Market ->
            Router.toAllMarket

        Dashboard ->
            Router.toLendDashboard Nothing

        Liquidity ->
            Router.toLiquidityProvider


toName : Tab -> String
toName tab =
    case tab of
        Market ->
            "Market"

        Dashboard ->
            "Dashboard"

        Liquidity ->
            "Liquidity"
