module Modal exposing (Modal(..), Msg, fromFragment, same, toUrl, update)

import Data.Chain exposing (Chain(..))
import Modals.Borrow.Main as Borrow
import Modals.Lend.Main as Lend
import Modals.Pay.Main as Pay
import Modals.Withdraw.Main as Withdraw


type Modal
    = Lend Lend.Modal
    | Borrow Borrow.Modal
    | Withdraw Withdraw.Modal
    | Pay Pay.Modal


fromFragment : { model | user : Maybe { user | chain : Chain } } -> String -> Maybe Modal
fromFragment { user } string =
    string
        |> String.split "?"
        |> (\list ->
                case list of
                    "lend" :: parameter :: _ ->
                        user
                            |> Maybe.map .chain
                            |> Maybe.withDefault Rinkeby
                            |> (\chain ->
                                    parameter
                                        |> Lend.fromFragment chain
                                        |> Maybe.map Lend
                               )

                    "borrow" :: parameter :: _ ->
                        user
                            |> Maybe.map .chain
                            |> Maybe.withDefault Rinkeby
                            |> (\chain ->
                                    parameter
                                        |> Borrow.fromFragment chain
                                        |> Maybe.map Borrow
                               )

                    "withdraw" :: parameter :: _ ->
                        user
                            |> Maybe.andThen
                                (\{ chain } ->
                                    parameter
                                        |> Withdraw.fromFragment chain
                                        |> Maybe.map Withdraw
                                )

                    "pay" :: parameter :: _ ->
                        user
                            |> Maybe.andThen
                                (\{ chain } ->
                                    parameter
                                        |> Pay.fromFragment chain
                                        |> Maybe.map Pay
                                )

                    _ ->
                        Nothing
           )


toUrl : Modal -> String
toUrl modal =
    case modal of
        Lend lend ->
            lend |> Lend.toUrl

        Borrow borrow ->
            borrow |> Borrow.toUrl

        Withdraw withdraw ->
            withdraw |> Withdraw.toUrl

        Pay pay ->
            pay |> Pay.toUrl


same : Modal -> Modal -> Bool
same modal1 modal2 =
    case ( modal1, modal2 ) of
        ( Lend lend1, Lend lend2 ) ->
            Lend.same lend1 lend2

        ( Borrow borrow1, Borrow borrow2 ) ->
            Borrow.same borrow1 borrow2

        ( Withdraw withdraw1, Withdraw withdraw2 ) ->
            Withdraw.same withdraw1 withdraw2

        ( Pay pay1, Pay pay2 ) ->
            Pay.same pay1 pay2

        _ ->
            False


type Msg
    = LendMsg Lend.Msg
    | BorrowMsg Borrow.Msg
    | WithdrawMsg Withdraw.Msg
    | PayMsg Pay.Msg


update : Msg -> Modal -> ( Modal, Cmd Msg )
update msg modal =
    case ( msg, modal ) of
        ( LendMsg lendMsg, Lend lend ) ->
            lend
                |> Lend.update lendMsg
                |> (\( updatedLend, cmd ) -> ( Lend updatedLend, cmd |> Cmd.map LendMsg ))

        ( BorrowMsg borrowMsg, Borrow borrow ) ->
            borrow
                |> Borrow.update borrowMsg
                |> (\( updatedBorrow, cmd ) -> ( Borrow updatedBorrow, cmd |> Cmd.map BorrowMsg ))

        ( WithdrawMsg withdrawMsg, Withdraw withdraw ) ->
            withdraw
                |> Withdraw.update withdrawMsg
                |> (\( updateWithdraw, cmd ) -> ( Withdraw updateWithdraw, cmd |> Cmd.map WithdrawMsg ))

        ( PayMsg lendMsg, Pay pay ) ->
            pay
                |> Pay.update lendMsg
                |> (\( updatedPay, cmd ) -> ( Pay updatedPay, cmd |> Cmd.map PayMsg ))

        _ ->
            ( modal, Cmd.none )
