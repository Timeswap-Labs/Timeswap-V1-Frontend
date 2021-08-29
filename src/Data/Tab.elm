module Data.Tab exposing (Tab(..), toName, toUrl)

import Pages.AllMarket.Main as AllMarket
import Pages.LendDashboard.Main as LendDashboard
import Pages.LiquidityProvider.Main as LiquidityProvider


type Tab
    = Market
    | Dashboard
    | Liquidity


toUrl : Tab -> String
toUrl tab =
    case tab of
        Market ->
            AllMarket.toUrl

        Dashboard ->
            LendDashboard.toUrl

        Liquidity ->
            LiquidityProvider.toUrl


toName : Tab -> String
toName tab =
    case tab of
        Market ->
            "Market"

        Dashboard ->
            "Dashboard"

        Liquidity ->
            "Liqudiity"
