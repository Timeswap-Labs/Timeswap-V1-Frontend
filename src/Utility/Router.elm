module Utility.Router exposing
    ( exit
    , toAllMarket
    , toBorrow
    , toBorrowDashboard
    , toConnect
    , toFaucet
    , toLend
    , toLendDashboard
    , toLiquidityProvider
    , toNoMetamask
    , toPairMarket
    , toPay
    , toSettings
    , toWallet
    , toWithdraw
    )

import Data.Maturity as Maturity exposing (Maturity)
import Data.Pair as Pair exposing (Pair)
import Data.TokenId as TokenId exposing (TokenId)
import Sort.Set exposing (Set)


toAllMarket : String
toAllMarket =
    "#market"


toPairMarket : Pair -> String
toPairMarket pair =
    [ "#market"
    , pair |> Pair.toFragment
    ]
        |> String.join "?"


toLendDashboard : String
toLendDashboard =
    "#dashboard?transaction=lend"


toBorrowDashboard : String
toBorrowDashboard =
    "#dashboard?transaction=borrow"


toLiquidityProvider : String
toLiquidityProvider =
    "#liquidity"


toLend : Pair -> Maturity -> String
toLend pair maturity =
    [ "#lend"
    , [ pair |> Pair.toFragment
      , maturity |> Maturity.toFragment
      ]
        |> String.join "&"
    ]
        |> String.join "?"


toBorrow : Pair -> Maturity -> String
toBorrow pair maturity =
    [ "#borrow"
    , [ pair |> Pair.toFragment
      , maturity |> Maturity.toFragment
      ]
        |> String.join "&"
    ]
        |> String.join "?"


toWithdraw : Pair -> Maturity -> String
toWithdraw pair maturity =
    [ "#withdraw"
    , [ pair |> Pair.toFragment
      , maturity |> Maturity.toFragment
      ]
        |> String.join "&"
    ]
        |> String.join "?"


toPay : Pair -> Maturity -> Set TokenId -> String
toPay pair maturity tokenIds =
    [ "#pay"
    , [ pair |> Pair.toFragment
      , maturity |> Maturity.toFragment
      , tokenIds |> TokenId.toFragment
      ]
        |> String.join "&"
    ]
        |> String.join "?"


toConnect : String
toConnect =
    "#connect"


toWallet : String
toWallet =
    "#wallet"


toFaucet : String
toFaucet =
    "#faucet"


toNoMetamask : String
toNoMetamask =
    "#nometamask"


toSettings : String
toSettings =
    "#settings"


exit : String
exit =
    "#"
