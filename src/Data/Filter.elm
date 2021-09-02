module Data.Filter exposing (Filter(..))

import Data.Pair exposing (Pair)


type Filter
    = AllMarket
    | PairMarket Pair
    | LendDashboard (Maybe Pair)
    | BorrowDashboard (Maybe Pair)
    | LiquidityProvider
