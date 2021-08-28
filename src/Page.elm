module Page exposing (Page(..), fromFragment, same, toName, toUrl, updateAllMarket, updateBorrowDashboard, updateLendDashboard)

import Data.Chain exposing (Chain(..))
import Data.Pair as Pair exposing (Pair)
import Data.Pools exposing (Pools)
import Pages.AllMarket.Main as AllMarket
import Pages.BorrowDashboard.Main as BorrowDashboard
import Pages.LendDashboard.Main as LendDashboard
import Pages.Liquidity.Main as Liquidity
import Pages.PairMarket.Main as PairMarket


type Page
    = AllMarket AllMarket.Page
    | PairMarket Pair
    | LendDashboard LendDashboard.Page
    | BorrowDashboard BorrowDashboard.Page
    | Liquidity


fromFragment : { model | pools : Pools, user : Maybe { user | chain : Chain } } -> String -> Maybe Page
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
                        Liquidity
                            |> Just

                    _ ->
                        Nothing
           )


toUrl : Page -> String
toUrl page =
    case page of
        AllMarket _ ->
            AllMarket.toUrl

        PairMarket pair ->
            PairMarket.toUrl pair

        LendDashboard _ ->
            LendDashboard.toUrl

        BorrowDashboard _ ->
            BorrowDashboard.toUrl

        Liquidity ->
            Liquidity.toUrl


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

        ( Liquidity, Liquidity ) ->
            True

        _ ->
            False


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
