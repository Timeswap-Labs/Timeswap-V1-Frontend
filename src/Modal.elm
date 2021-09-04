module Modal exposing (Modal, Msg, fromFragment, same, subscriptions, toUrl, update, view)

import Browser.Navigation exposing (Key)
import Data.Address exposing (Address)
import Data.Allowances exposing (Allowances)
import Data.Backdrop exposing (Backdrop)
import Data.Balances exposing (Balances)
import Data.Chain exposing (Chain(..))
import Data.Deadline exposing (Deadline)
import Data.Device as Device exposing (Device)
import Data.Images exposing (Images)
import Data.Maturity as Maturity
import Data.Pools exposing (Pools)
import Data.Remote exposing (Remote)
import Data.Slippage exposing (Slippage)
import Data.TokenImages exposing (TokenImages)
import Data.Tokens exposing (Tokens)
import Data.ZoneInfo exposing (ZoneInfo)
import Element
    exposing
        ( Element
        , el
        , fill
        , height
        , none
        , padding
        , scrollbarY
        , width
        )
import Element.Background as Background
import Element.Font as Font
import Modals.Borrow.Main as Borrow
import Modals.Lend.Main as Lend
import Modals.Pay.Main as Pay
import Modals.Withdraw.Main as Withdraw
import Page exposing (Page)
import Time exposing (Posix)
import Utility.Color as Color
import Utility.Router as Router
import Utility.Typography as Typography


type Modal
    = Lend Lend.Modal
    | Borrow Borrow.Modal
    | Withdraw Withdraw.Modal
    | Pay Pay.Modal


fromFragment :
    { model
        | time : Posix
        , tokens : Tokens
        , pools : Pools
        , user : Maybe user
    }
    -> String
    -> Maybe Modal
fromFragment ({ user } as model) string =
    string
        |> String.split "?"
        |> (\list ->
                case list of
                    "lend" :: parameter :: _ ->
                        parameter
                            |> Lend.fromFragment model
                            |> Maybe.map Lend

                    "borrow" :: parameter :: _ ->
                        parameter
                            |> Borrow.fromFragment model
                            |> Maybe.map Borrow

                    "withdraw" :: parameter :: _ ->
                        user
                            |> Maybe.andThen
                                (\_ ->
                                    parameter
                                        |> Withdraw.fromFragment model
                                        |> Maybe.map Withdraw
                                )

                    "pay" :: parameter :: _ ->
                        user
                            |> Maybe.andThen
                                (\_ ->
                                    parameter
                                        |> Pay.fromFragment model
                                        |> Maybe.map Pay
                                )

                    _ ->
                        Nothing
           )


toUrl : Modal -> String
toUrl modal =
    case modal of
        Lend lend ->
            lend
                |> Lend.getPool
                |> Router.toLend

        Borrow borrow ->
            borrow
                |> Borrow.getPool
                |> Router.toBorrow

        Withdraw withdraw ->
            withdraw
                |> Withdraw.getPool
                |> Router.toWithdraw

        Pay pay ->
            pay
                |> Pay.getFlags
                |> (\{ pool, tokenIds } ->
                        Router.toPay pool tokenIds
                   )


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


update :
    { model
        | key : Key
        , slippage : Slippage
        , tokens : Tokens
        , pools : Pools
        , user : Maybe { user | balances : Remote Balances }
        , page : Page
    }
    -> Msg
    -> Modal
    -> ( Modal, Cmd Msg )
update model msg modal =
    case ( msg, modal ) of
        ( LendMsg lendMsg, Lend lend ) ->
            lend
                |> Lend.update model lendMsg
                |> (\( updatedLend, cmd ) -> ( Lend updatedLend, cmd |> Cmd.map LendMsg ))

        ( BorrowMsg borrowMsg, Borrow borrow ) ->
            borrow
                |> Borrow.update model borrowMsg
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


subscriptions : { model | time : Posix } -> Modal -> Sub Msg
subscriptions model modal =
    case modal of
        Lend lend ->
            Lend.subscriptions model lend |> Sub.map LendMsg

        Borrow borrow ->
            Borrow.subscriptions model borrow |> Sub.map BorrowMsg

        _ ->
            Sub.none


view :
    { model
        | device : Device
        , time : Posix
        , zoneInfo : Maybe ZoneInfo
        , backdrop : Backdrop
        , deadline : Deadline
        , images : Images
        , tokenImages : TokenImages
        , user :
            Maybe
                { user
                    | address : Address
                    , balances : Remote Balances
                    , allowances : Remote Allowances
                }
    }
    -> Modal
    -> Element Msg
view ({ device, time } as model) modal =
    el
        [ width fill
        , height fill
        , if Device.isPhone device then
            padding 0

          else
            padding 80
        , scrollbarY
        , Background.color Color.modal
        , Font.family Typography.supreme
        ]
        |> (\element ->
                case modal of
                    Lend lend ->
                        lend
                            |> Lend.getPool
                            |> (\{ maturity } ->
                                    if maturity |> Maturity.isActive time then
                                        Lend.view model lend
                                            |> Element.map LendMsg
                                            |> element

                                    else
                                        none
                               )

                    Borrow borrow ->
                        borrow
                            |> Borrow.getPool
                            |> (\{ maturity } ->
                                    if maturity |> Maturity.isActive time then
                                        Borrow.view model borrow
                                            |> Element.map BorrowMsg
                                            |> element

                                    else
                                        none
                               )

                    _ ->
                        none
           )
