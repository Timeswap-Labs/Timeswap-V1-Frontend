module Page exposing
    ( Msg
    , Page
    , fromFragment
    , getPair
    , init
    , same
    , toTab
    , toUrl
    , update
    )

import Data.Chain exposing (Chain(..))
import Data.Pair as Pair exposing (Pair)
import Data.Pools exposing (Pools)
import Data.Tab as Tab exposing (Tab)
import Pages.AllMarket.Main as AllMarket
import Pages.BorrowDashboard.Main as BorrowDashboard
import Pages.LendDashboard.Main as LendDashboard
import Pages.LiquidityProvider.Main as LiquidityProvider
import Pages.PairMarket.Main as PairMarket


type Page
    = AllMarket AllMarket.Page
    | PairMarket PairMarket.Page
    | LendDashboard LendDashboard.Page
    | BorrowDashboard BorrowDashboard.Page
    | LiquidityProvider


init : { model | pools : Pools, user : Maybe { user | chain : Chain } } -> Page
init model =
    AllMarket.init model
        |> AllMarket


fromFragment :
    { model | pools : Pools, user : Maybe { user | chain : Chain } }
    -> String
    -> Maybe Page
fromFragment ({ user } as model) string =
    string
        |> String.split "?"
        |> List.concatMap (String.split "&")
        |> (\list ->
                case list of
                    "market" :: asset :: collateral :: _ ->
                        user
                            |> Maybe.map .chain
                            |> Maybe.withDefault Rinkeby
                            |> (\chain -> Pair.fromFragment chain asset collateral)
                            |> Maybe.map PairMarket.init
                            |> Maybe.map PairMarket

                    "market" :: _ ->
                        AllMarket.init model
                            |> AllMarket
                            |> Just

                    "dashboard" :: "transaction=lend" :: _ ->
                        LendDashboard LendDashboard.init
                            |> Just

                    "dashboard" :: "transaction=borrow" :: _ ->
                        BorrowDashboard BorrowDashboard.init
                            |> Just

                    "dashboard" :: _ ->
                        LendDashboard LendDashboard.init
                            |> Just

                    "liquidity" :: _ ->
                        LiquidityProvider
                            |> Just

                    _ ->
                        Nothing
           )


toUrl : Page -> String
toUrl page =
    case page of
        AllMarket _ ->
            AllMarket.toUrl

        PairMarket pairMarket ->
            pairMarket |> PairMarket.getPair |> PairMarket.toUrl

        LendDashboard _ ->
            LendDashboard.toUrl

        BorrowDashboard _ ->
            BorrowDashboard.toUrl

        LiquidityProvider ->
            LiquidityProvider.toUrl


same : Page -> Page -> Bool
same page1 page2 =
    case ( page1, page2 ) of
        ( AllMarket _, AllMarket _ ) ->
            True

        ( PairMarket pair1, PairMarket pair2 ) ->
            pair1 == pair2

        ( LendDashboard _, LendDashboard _ ) ->
            True

        ( BorrowDashboard _, BorrowDashboard _ ) ->
            True

        ( LiquidityProvider, LiquidityProvider ) ->
            True

        _ ->
            False


toTab : Page -> Tab
toTab page =
    case page of
        AllMarket _ ->
            Tab.Market

        PairMarket _ ->
            Tab.Market

        LendDashboard _ ->
            Tab.Dashboard

        BorrowDashboard _ ->
            Tab.Dashboard

        LiquidityProvider ->
            Tab.Liquidity


getPair : Page -> Maybe Pair
getPair page =
    case page of
        PairMarket pairMarket ->
            pairMarket |> PairMarket.getPair |> Just

        _ ->
            Nothing


type Msg
    = AllMarketMsg AllMarket.Msg
    | PairMarketMsg PairMarket.Msg
    | LendDashboardMsg LendDashboard.Msg
    | BorrowDashboardMsg BorrowDashboard.Msg


update : Msg -> Page -> Page
update msg page =
    case ( msg, page ) of
        ( AllMarketMsg allMarketMsg, AllMarket allMarket ) ->
            allMarket
                |> AllMarket.update allMarketMsg
                |> AllMarket

        _ ->
            page
