module Modal exposing (Modal, Msg, fromFragment, toFragment, update)

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


toFragment : Modal -> String
toFragment modal =
    case modal of
        Lend lend ->
            [ "lend"
            , lend |> Lend.toFragment
            ]
                |> String.join "?"

        Borrow borrow ->
            [ "borrow"
            , borrow |> Borrow.toFragment
            ]
                |> String.join "?"

        Withdraw withdraw ->
            [ "withdraw"
            , withdraw |> Withdraw.toFragment
            ]
                |> String.join "?"

        Pay pay ->
            [ "pay"
            , pay |> Pay.toFragment
            ]
                |> String.join "?"


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
