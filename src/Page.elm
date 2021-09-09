module Page exposing
    ( Msg
    , Page
    , fromFragment
    , init
    , same
    , toFilter
    , toTab
    , toUrl
    , update
    , view
    )

import Data.Chain exposing (Chain(..))
import Data.Device exposing (Device)
import Data.Filter as Filter exposing (Filter)
import Data.Images exposing (Images)
import Data.Pools exposing (Pools)
import Data.Positions exposing (Positions)
import Data.Remote exposing (Remote)
import Data.Tab as Tab exposing (Tab)
import Data.TokenImages exposing (TokenImages)
import Data.Tokens exposing (Tokens)
import Data.ZoneInfo exposing (ZoneInfo)
import Element
    exposing
        ( Element
        , el
        , fill
        , height
        , paddingXY
        , scrollbarY
        , width
        )
import Element.Font as Font
import Pages.AllMarket.Main as AllMarket
import Pages.BorrowDashboard.Main as BorrowDashboard
import Pages.LendDashboard.Main as LendDashboard
import Pages.LiquidityProvider.Main as LiquidityProvider
import Pages.PairMarket.Main as PairMarket
import Time exposing (Posix)
import Utility.Router as Router
import Utility.Typography as Typography


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
    { model
        | time : Posix
        , tokens : Tokens
        , pools : Pools
        , user : Maybe { user | positions : Remote Positions }
    }
    -> String
    -> Maybe Page
fromFragment model string =
    string
        |> String.split "?"
        |> (\list ->
                case list of
                    "market" :: parameter :: _ ->
                        parameter
                            |> PairMarket.fromFragment model
                            |> Maybe.map PairMarket

                    "market" :: _ ->
                        AllMarket.init model
                            |> AllMarket
                            |> Just

                    "dashboard" :: transactionString :: _ ->
                        transactionString
                            |> String.split "&"
                            |> (\innerList ->
                                    case innerList of
                                        "transaction=lend" :: asset :: collateral :: _ ->
                                            [ asset
                                            , collateral
                                            ]
                                                |> String.join "&"
                                                |> LendDashboard.fromFragment model
                                                |> LendDashboard
                                                |> Just

                                        "transaction=lend" :: _ ->
                                            LendDashboard.init model Nothing
                                                |> LendDashboard
                                                |> Just

                                        "transaction=borrow" :: asset :: collateral :: _ ->
                                            [ asset
                                            , collateral
                                            ]
                                                |> String.join "&"
                                                |> BorrowDashboard.fromFragment model
                                                |> BorrowDashboard
                                                |> Just

                                        "transaction=borrow" :: _ ->
                                            BorrowDashboard.init model Nothing
                                                |> BorrowDashboard
                                                |> Just

                                        _ ->
                                            LendDashboard.init model Nothing
                                                |> LendDashboard
                                                |> Just
                               )

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
            Router.toAllMarket

        PairMarket pairMarket ->
            pairMarket
                |> PairMarket.getPair
                |> Router.toPairMarket

        LendDashboard lendDashboard ->
            lendDashboard
                |> LendDashboard.getFilter
                |> Router.toLendDashboard

        BorrowDashboard borrowDashboard ->
            borrowDashboard
                |> BorrowDashboard.getFilter
                |> Router.toBorrowDashboard

        LiquidityProvider ->
            Router.toLiquidityProvider


same : Page -> Page -> Bool
same page1 page2 =
    case ( page1, page2 ) of
        ( AllMarket _, AllMarket _ ) ->
            True

        ( PairMarket pairMarket1, PairMarket pairMarket2 ) ->
            (pairMarket1 |> PairMarket.getPair)
                == (pairMarket2 |> PairMarket.getPair)

        ( LendDashboard lendDashboard1, LendDashboard lendDashboard2 ) ->
            (lendDashboard1 |> LendDashboard.getFilter)
                == (lendDashboard2 |> LendDashboard.getFilter)

        ( BorrowDashboard borrowDashboard1, BorrowDashboard borrowDashboard2 ) ->
            (borrowDashboard1 |> BorrowDashboard.getFilter)
                == (borrowDashboard2 |> BorrowDashboard.getFilter)

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


toFilter : Page -> Filter
toFilter page =
    case page of
        AllMarket _ ->
            Filter.AllMarket

        PairMarket pairMarket ->
            pairMarket
                |> PairMarket.getPair
                |> Filter.PairMarket

        LendDashboard lendDashboard ->
            lendDashboard
                |> LendDashboard.getFilter
                |> Filter.LendDashboard

        BorrowDashboard borrowDashboard ->
            borrowDashboard
                |> BorrowDashboard.getFilter
                |> Filter.BorrowDashboard

        LiquidityProvider ->
            Filter.LiquidityProvider


type Msg
    = AllMarketMsg AllMarket.Msg
    | PairMarketMsg PairMarket.Msg
    | LendDashboardMsg LendDashboard.Msg
    | BorrowDashboardMsg BorrowDashboard.Msg


update :
    { model
        | pools : Pools
        , user : Maybe { user | positions : Remote Positions }
    }
    -> Msg
    -> Page
    -> Page
update model msg page =
    case ( msg, page ) of
        ( AllMarketMsg allMarketMsg, AllMarket allMarket ) ->
            allMarket
                |> AllMarket.update model allMarketMsg
                |> AllMarket

        ( PairMarketMsg pairMarketMsg, PairMarket pairMarket ) ->
            pairMarket
                |> PairMarket.update pairMarketMsg
                |> PairMarket

        ( LendDashboardMsg lendDashboardMsg, LendDashboard lendDashboard ) ->
            lendDashboard
                |> LendDashboard.update model lendDashboardMsg
                |> LendDashboard

        ( BorrowDashboardMsg borrowDashboardMsg, BorrowDashboard borrowDashboard ) ->
            borrowDashboard
                |> BorrowDashboard.update model borrowDashboardMsg
                |> BorrowDashboard

        _ ->
            page


view :
    { model
        | device : Device
        , time : Posix
        , zoneInfo : Maybe ZoneInfo
        , images : Images
        , tokenImages : TokenImages
        , pools : Pools
        , user : Maybe { user | positions : Remote Positions }
    }
    -> Page
    -> Element Msg
view model page =
    el
        [ width fill
        , height fill
        , paddingXY 0 38
        , scrollbarY
        , Font.family Typography.supreme
        ]
        (case page of
            AllMarket allMarket ->
                AllMarket.view model allMarket
                    |> Element.map AllMarketMsg

            PairMarket pairMarket ->
                PairMarket.view model pairMarket
                    |> Element.map PairMarketMsg

            LendDashboard lendDashboard ->
                LendDashboard.view model lendDashboard
                    |> Element.map LendDashboardMsg

            BorrowDashboard borrowDashboard ->
                BorrowDashboard.view model borrowDashboard
                    |> Element.map BorrowDashboardMsg

            LiquidityProvider ->
                LiquidityProvider.view
        )
