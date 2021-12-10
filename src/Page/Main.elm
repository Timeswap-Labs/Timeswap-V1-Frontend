module Page.Main exposing
    ( Effect(..)
    , Msg
    , Page
    , change
    , init
    , subscriptions
    , toParameter
    , toPoolInfo
    , toTab
    , update
    , view
    )

import Blockchain.Main exposing (Blockchain)
import Data.Backdrop exposing (Backdrop)
import Data.Chains exposing (Chains)
import Data.ChosenZone exposing (ChosenZone)
import Data.Deadline exposing (Deadline)
import Data.Images exposing (Images)
import Data.Offset exposing (Offset)
import Data.Pair exposing (Pair)
import Data.Parameter as Parameter exposing (Parameter)
import Data.Slippage exposing (Slippage)
import Data.Support exposing (Support(..))
import Data.Tab as Tab exposing (Tab)
import Data.TokenParam exposing (TokenParam)
import Element
    exposing
        ( Element
        , alignTop
        , centerX
        , column
        , height
        , map
        , shrink
        , spacing
        , width
        )
import Page.Route as Route
import Page.Transaction.Borrow.Main as TransactionBorrow
import Page.Transaction.Lend.Main as Lend
import Page.Transaction.Liquidity.Main as Liquidity
import Page.Transaction.PoolInfo exposing (PoolInfo)
import Time exposing (Posix)
import Url exposing (Url)


type Page
    = Lend { transaction : Lend.Transaction }
    | Borrow { transaction : TransactionBorrow.Section }
    | Liquidity { transaction : Liquidity.Transaction }


type Msg
    = LendMsg Lend.Msg
    | LiquidityMsg Liquidity.Msg


type Effect
    = OpenTokenList TokenParam
    | OpenMaturityList Pair
    | OpenChooseMaturity Pair
    | OpenSettings
    | OpenConnect
    | OpenConfirm


init :
    { model
        | time : Posix
        , chains : Chains
        , blockchain : Support userNotSupported Blockchain
    }
    -> Url
    -> ( Page, Cmd Msg )
init model url =
    Nothing
        |> construct model url


change :
    { model
        | time : Posix
        , chains : Chains
        , blockchain : Support userNotSupported Blockchain
    }
    -> Url
    -> Page
    -> ( Page, Cmd Msg )
change model url page =
    Just page
        |> construct model url


construct :
    { model
        | time : Posix
        , chains : Chains
        , blockchain : Support userNotSupported Blockchain
    }
    -> Url
    -> Maybe Page
    -> ( Page, Cmd Msg )
construct ({ chains } as model) url maybePage =
    case
        ( url |> Route.fromUrl model.blockchain chains
        , model.blockchain
        , maybePage |> Maybe.andThen toPoolInfo
        )
    of
        ( Just (Route.Lend (Just (Parameter.Pool pool))), Supported blockchain, Just poolInfo ) ->
            poolInfo
                |> Lend.initGivenPoolInfo model blockchain pool
                |> Tuple.mapBoth
                    (\transaction ->
                        { transaction = transaction }
                            |> Lend
                    )
                    (Cmd.map LendMsg)

        ( Just (Route.Lend parameter), Supported blockchain, _ ) ->
            parameter
                |> Lend.init model blockchain
                |> Tuple.mapBoth
                    (\transaction ->
                        { transaction = transaction }
                            |> Lend
                    )
                    (Cmd.map LendMsg)

        ( Just (Route.Borrow parameter), _, _ ) ->
            ( { transaction = TransactionBorrow.init parameter }
                |> Borrow
            , Cmd.none
            )

        ( Just (Route.Liquidity (Just (Parameter.Pool pool))), Supported blockchain, Just poolInfo ) ->
            poolInfo
                |> Liquidity.initGivenPoolInfo model blockchain pool
                |> Tuple.mapBoth
                    (\transaction ->
                        { transaction = transaction }
                            |> Liquidity
                    )
                    (Cmd.map LiquidityMsg)

        ( Just (Route.Liquidity parameter), Supported blockchain, _ ) ->
            parameter
                |> Liquidity.init model blockchain
                |> Tuple.mapBoth
                    (\transaction ->
                        { transaction = transaction }
                            |> Liquidity
                    )
                    (Cmd.map LiquidityMsg)

        _ ->
            ( { transaction = Lend.notSupported }
                |> Lend
            , Cmd.none
            )


update :
    { model
        | time : Posix
        , slippage : Slippage
        , deadline : Deadline
    }
    -> Blockchain
    -> Msg
    -> Page
    -> ( Page, Cmd Msg, Maybe Effect )
update model blockchain msg page =
    case ( msg, page ) of
        ( LendMsg transactionLendMsg, Lend lend ) ->
            lend.transaction
                |> Lend.update
                    model
                    blockchain
                    transactionLendMsg
                |> (\( updated, cmd, maybeEffect ) ->
                        ( { lend | transaction = updated }
                            |> Lend
                        , cmd |> Cmd.map LendMsg
                        , maybeEffect
                            |> Maybe.map lendEffects
                        )
                   )

        ( LiquidityMsg liquidityMsg, Liquidity liquidity ) ->
            liquidity.transaction
                |> Liquidity.update
                    model
                    blockchain
                    liquidityMsg
                |> (\( updated, cmd, maybeEffect ) ->
                        ( { liquidity | transaction = updated }
                            |> Liquidity
                        , cmd |> Cmd.map LiquidityMsg
                        , maybeEffect
                            |> Maybe.map liquidityEffects
                        )
                   )

        _ ->
            ( page, Cmd.none, Nothing )


lendEffects :
    Lend.Effect
    -> Effect
lendEffects effect =
    case effect of
        Lend.OpenTokenList tokenParam ->
            OpenTokenList tokenParam

        Lend.OpenMaturityList pair ->
            OpenMaturityList pair

        Lend.OpenConnect ->
            OpenConnect

        Lend.OpenSettings ->
            OpenSettings

        Lend.OpenConfirm ->
            OpenConfirm


liquidityEffects :
    Liquidity.Effect
    -> Effect
liquidityEffects effect =
    case effect of
        Liquidity.OpenTokenList tokenParam ->
            OpenTokenList tokenParam

        Liquidity.OpenMaturityList pair ->
            OpenMaturityList pair

        Liquidity.OpenChooseMaturity pair ->
            OpenChooseMaturity pair

        Liquidity.OpenConnect ->
            OpenConnect

        Liquidity.OpenSettings ->
            OpenSettings

        Liquidity.OpenConfirm ->
            OpenConfirm


subscriptions : Page -> Sub Msg
subscriptions page =
    case page of
        Lend { transaction } ->
            [ transaction
                |> Lend.subscriptions
                |> Sub.map LendMsg
            ]
                |> Sub.batch

        Liquidity { transaction } ->
            [ transaction
                |> Liquidity.subscriptions
                |> Sub.map LiquidityMsg
            ]
                |> Sub.batch

        _ ->
            Sub.none


toTab : Page -> Tab
toTab page =
    case page of
        Lend _ ->
            Tab.Lend

        Borrow _ ->
            Tab.Borrow

        Liquidity _ ->
            Tab.Liquidity


toParameter : Page -> Maybe Parameter
toParameter page =
    case page of
        Lend { transaction } ->
            transaction
                |> Lend.toParameter

        Borrow { transaction } ->
            transaction
                |> TransactionBorrow.toParameter

        Liquidity { transaction } ->
            transaction
                |> Liquidity.toParameter


toPoolInfo : Page -> Maybe PoolInfo
toPoolInfo page =
    case page of
        Lend { transaction } ->
            transaction
                |> Lend.toPoolInfo

        _ ->
            Nothing


view :
    { model
        | time : Posix
        , offset : Offset
        , chosenZone : ChosenZone
        , backdrop : Backdrop
        , images : Images
    }
    -> Blockchain
    -> Page
    -> Element Msg
view model blockchain page =
    column
        [ width shrink
        , height shrink
        , spacing 20
        , centerX
        , alignTop
        ]
        (case page of
            Lend { transaction } ->
                [ transaction
                    |> Lend.view model
                    |> map LendMsg
                ]

            Liquidity { transaction } ->
                [ transaction
                    |> Liquidity.view model
                    |> map LiquidityMsg
                ]

            _ ->
                []
        )