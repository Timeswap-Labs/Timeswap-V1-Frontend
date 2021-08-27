module Page exposing (Page(..), fromFragment, toFragment, toName, updateAllMarket, updateBorrowDashboard, updateLendDashboard)

import Data.Chain exposing (Chain(..))
import Data.Pair as Pair exposing (Pair)
import Pages.AllMarket.Main as AllMarket
import Pages.BorrowDashboard.Main as BorrowDashboard
import Pages.LendDashboard.Main as LendDashboard


type Page
    = AllMarket AllMarket.Page
    | PairMarket Pair
    | LendDashboard LendDashboard.Page
    | BorrowDashboard BorrowDashboard.Page
    | Liquidity


fromFragment : { model | user : Maybe { user | chain : Chain } } -> String -> Maybe Page
fromFragment { user } string =
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
                            |> Maybe.map PairMarket

                    "market" :: _ ->
                        AllMarket AllMarket.init
                            |> Just

                    "dashboard" :: "transaction=lend" :: _ ->
                        LendDashboard LendDashboard.init
                            |> Just

                    "dashboard" :: "transaction=borrow" :: _ ->
                        BorrowDashboard BorrowDashboard.init
                            |> Just

                    "liquidity" :: _ ->
                        Liquidity
                            |> Just

                    _ ->
                        Nothing
           )


toFragment : Page -> String
toFragment page =
    case page of
        AllMarket _ ->
            "market"

        PairMarket pair ->
            [ "market"
            , pair |> Pair.toFragment
            ]
                |> String.join "?"

        LendDashboard _ ->
            "dashboard?transaction=lend"

        BorrowDashboard _ ->
            "dashboard?transaction=borrow"

        Liquidity ->
            "liquidity"


toName : Page -> String
toName page =
    case page of
        AllMarket _ ->
            "Market"

        PairMarket _ ->
            "Market"

        LendDashboard _ ->
            "Dashboard"

        BorrowDashboard _ ->
            "Dashboard"

        Liquidity ->
            "liquidity"


updateAllMarket : AllMarket.Msg -> Page -> ( Page, Cmd AllMarket.Msg )
updateAllMarket msg page =
    case page of
        AllMarket allMarket ->
            allMarket
                |> AllMarket.update msg
                |> Tuple.mapFirst AllMarket

        _ ->
            ( page, Cmd.none )


updateLendDashboard : LendDashboard.Msg -> Page -> ( Page, Cmd LendDashboard.Msg )
updateLendDashboard msg page =
    case page of
        LendDashboard lendDashboard ->
            lendDashboard
                |> LendDashboard.update msg
                |> Tuple.mapFirst LendDashboard

        _ ->
            ( page, Cmd.none )


updateBorrowDashboard : BorrowDashboard.Msg -> Page -> ( Page, Cmd BorrowDashboard.Msg )
updateBorrowDashboard msg page =
    case page of
        BorrowDashboard borrowDashboard ->
            borrowDashboard
                |> BorrowDashboard.update msg
                |> Tuple.mapFirst BorrowDashboard

        _ ->
            ( page, Cmd.none )
