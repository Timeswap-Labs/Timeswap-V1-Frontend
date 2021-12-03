module Utility.Router exposing
    ( exit
    , toAllMarket
    , toBorrow
    , toBorrowDashboard
    , toConnect
    , toLend
    , toLendDashboard
    , toLiquidityProvider
    , toNoMetamask
    , toPairMarket
    , toPay
    , toSettings
    , toSwap
    , toWallet
    , toWithdraw
    )

import Data.Pair as Pair exposing (Pair)
import Data.Pool as Pool exposing (Pool)
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


toLendDashboard : Maybe Pair -> String
toLendDashboard filter =
    filter
        |> Maybe.map Pair.toFragment
        |> Maybe.map ((++) "&")
        |> Maybe.withDefault ""
        |> (++) "#dashboard?transaction=lend"


toBorrowDashboard : Maybe Pair -> String
toBorrowDashboard filter =
    filter
        |> Maybe.map Pair.toFragment
        |> Maybe.map ((++) "&")
        |> Maybe.withDefault ""
        |> (++) "#dashboard?transaction=borrow"


toLiquidityProvider : String
toLiquidityProvider =
    "#liquidity"


toLend : Pool -> String
toLend pool =
    [ "#lend"
    , pool |> Pool.toFragment
    ]
        |> String.join "?"


toBorrow : Pool -> String
toBorrow pool =
    [ "#borrow"
    , pool |> Pool.toFragment
    ]
        |> String.join "?"


toWithdraw : Pool -> String
toWithdraw pool =
    [ "#withdraw"
    , pool |> Pool.toFragment
    ]
        |> String.join "?"


toPay : Pool -> Set TokenId -> String
toPay pool tokenIds =
    [ "#pay"
    , [ pool |> Pool.toFragment
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


toNoMetamask : String
toNoMetamask =
    "#nometamask"


toSettings : String
toSettings =
    "#settings"


exit : String
exit =
    "#"


toSwap : String
toSwap =
    "#swap"
