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
import Blockchain.User.WriteBorrow exposing (WriteBorrow)
import Blockchain.User.WriteCreate exposing (WriteCreate)
import Blockchain.User.WriteLend exposing (WriteLend)
import Blockchain.User.WriteLiquidity exposing (WriteLiquidity)
import Data.Backdrop exposing (Backdrop)
import Data.Chains exposing (Chains)
import Data.ChosenZone exposing (ChosenZone)
import Data.ERC20 exposing (ERC20)
import Data.Images exposing (Images)
import Data.Offset exposing (Offset)
import Data.Or exposing (Or(..))
import Data.Pair exposing (Pair)
import Data.Parameter as Parameter exposing (Parameter)
import Data.PriceFeed exposing (PriceFeed)
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
import Page.Transaction.Borrow.Main as Borrow
import Page.Transaction.Lend.Main as Lend
import Page.Transaction.Liquidity.Main as Liquidity
import Page.Transaction.PoolInfo exposing (PoolInfo)
import Page.Transaction.Price exposing (Price)
import Time exposing (Posix)
import Url exposing (Url)


type Page
    = LendPage { transaction : Lend.Transaction }
    | BorrowPage { transaction : Borrow.Transaction }
    | LiquidityPage { transaction : Liquidity.Transaction }


type Msg
    = LendMsg Lend.Msg
    | BorrowMsg Borrow.Msg
    | LiquidityMsg Liquidity.Msg


type Effect
    = OpenTokenList TokenParam
    | OpenMaturityList Pair
    | OpenInputMaturity Pair
    | OpenSettings
    | OpenConnect
    | Approve ERC20
    | Lend WriteLend
    | Borrow WriteBorrow
    | Liquidity WriteLiquidity
    | Create WriteCreate


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
        ( Just (Route.Lend (Just (Parameter.Pool pool))), Supported blockchain, Just (Right poolInfo) ) ->
            poolInfo
                |> Lend.initGivenPoolInfo model blockchain pool
                |> Tuple.mapBoth
                    (\transaction ->
                        { transaction = transaction }
                            |> LendPage
                    )
                    (Cmd.map LendMsg)

        ( Just (Route.Lend (Just (Parameter.Pool pool))), Supported blockchain, Just (Left spot) ) ->
            spot
                |> Lend.initGivenSpot model blockchain pool
                |> Tuple.mapBoth
                    (\transaction ->
                        { transaction = transaction }
                            |> LendPage
                    )
                    (Cmd.map LendMsg)

        ( Just (Route.Lend parameter), Supported blockchain, _ ) ->
            parameter
                |> Lend.init model blockchain
                |> Tuple.mapBoth
                    (\transaction ->
                        { transaction = transaction }
                            |> LendPage
                    )
                    (Cmd.map LendMsg)

        ( Just (Route.Borrow (Just (Parameter.Pool pool))), Supported blockchain, Just (Right poolInfo) ) ->
            poolInfo
                |> Borrow.initGivenPoolInfo model blockchain pool
                |> Tuple.mapBoth
                    (\transaction ->
                        { transaction = transaction }
                            |> BorrowPage
                    )
                    (Cmd.map BorrowMsg)

        ( Just (Route.Borrow (Just (Parameter.Pool pool))), Supported blockchain, Just (Left spot) ) ->
            spot
                |> Borrow.initGivenSpot model blockchain pool
                |> Tuple.mapBoth
                    (\transaction ->
                        { transaction = transaction }
                            |> BorrowPage
                    )
                    (Cmd.map BorrowMsg)

        ( Just (Route.Borrow parameter), Supported blockchain, _ ) ->
            parameter
                |> Borrow.init model blockchain
                |> Tuple.mapBoth
                    (\transaction ->
                        { transaction = transaction }
                            |> BorrowPage
                    )
                    (Cmd.map BorrowMsg)

        ( Just (Route.Liquidity (Just (Parameter.Pool pool))), Supported blockchain, Just (Right poolInfo) ) ->
            poolInfo
                |> Liquidity.initGivenPoolInfo model blockchain pool
                |> Tuple.mapBoth
                    (\transaction ->
                        { transaction = transaction }
                            |> LiquidityPage
                    )
                    (Cmd.map LiquidityMsg)

        ( Just (Route.Liquidity (Just (Parameter.Pool pool))), Supported blockchain, Just (Left spot) ) ->
            spot
                |> Liquidity.initGivenSpot model blockchain pool
                |> Tuple.mapBoth
                    (\transaction ->
                        { transaction = transaction }
                            |> LiquidityPage
                    )
                    (Cmd.map LiquidityMsg)

        ( Just (Route.Liquidity parameter), Supported blockchain, _ ) ->
            parameter
                |> Liquidity.init model blockchain
                |> Tuple.mapBoth
                    (\transaction ->
                        { transaction = transaction }
                            |> LiquidityPage
                    )
                    (Cmd.map LiquidityMsg)

        ( _, Supported blockchain, _ ) ->
            Nothing
                |> Lend.init model blockchain
                |> Tuple.mapBoth
                    (\transaction ->
                        { transaction = transaction }
                            |> LendPage
                    )
                    (Cmd.map LendMsg)

        ( Just (Route.Lend _), NotSupported _, _ ) ->
            ( { transaction = Lend.notSupported }
                |> LendPage
            , Cmd.none
            )

        ( Just (Route.Borrow _), NotSupported _, _ ) ->
            ( { transaction = Borrow.notSupported }
                |> BorrowPage
            , Cmd.none
            )

        ( Just (Route.Liquidity _), NotSupported _, _ ) ->
            ( { transaction = Liquidity.notSupported }
                |> LiquidityPage
            , Cmd.none
            )

        _ ->
            ( { transaction = Lend.notSupported }
                |> LendPage
            , Cmd.none
            )


update :
    { model | slippage : Slippage }
    -> Blockchain
    -> Msg
    -> Page
    -> ( Page, Cmd Msg, Maybe Effect )
update model blockchain msg page =
    case ( msg, page ) of
        ( LendMsg transactionLendMsg, LendPage lendPage ) ->
            lendPage.transaction
                |> Lend.update
                    model
                    blockchain
                    transactionLendMsg
                |> (\( updated, cmd, maybeEffect ) ->
                        ( { lendPage | transaction = updated }
                            |> LendPage
                        , cmd |> Cmd.map LendMsg
                        , maybeEffect
                            |> Maybe.map lendEffects
                        )
                   )

        ( BorrowMsg transactionBorrowMsg, BorrowPage borrowPage ) ->
            borrowPage.transaction
                |> Borrow.update
                    model
                    blockchain
                    transactionBorrowMsg
                |> (\( updated, cmd, maybeEffect ) ->
                        ( { borrowPage | transaction = updated }
                            |> BorrowPage
                        , cmd |> Cmd.map BorrowMsg
                        , maybeEffect
                            |> Maybe.map borrowEffects
                        )
                   )

        ( LiquidityMsg liquidityMsg, LiquidityPage liquidityPage ) ->
            liquidityPage.transaction
                |> Liquidity.update
                    model
                    blockchain
                    liquidityMsg
                |> (\( updated, cmd, maybeEffect ) ->
                        ( { liquidityPage | transaction = updated }
                            |> LiquidityPage
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

        Lend.Approve erc20 ->
            Approve erc20

        Lend.Lend writeLend ->
            Lend writeLend


borrowEffects :
    Borrow.Effect
    -> Effect
borrowEffects effect =
    case effect of
        Borrow.OpenTokenList tokenParam ->
            OpenTokenList tokenParam

        Borrow.OpenMaturityList pair ->
            OpenMaturityList pair

        Borrow.OpenConnect ->
            OpenConnect

        Borrow.OpenSettings ->
            OpenSettings

        Borrow.Approve erc20 ->
            Approve erc20

        Borrow.Borrow writeBorrow ->
            Borrow writeBorrow


liquidityEffects :
    Liquidity.Effect
    -> Effect
liquidityEffects effect =
    case effect of
        Liquidity.OpenTokenList tokenParam ->
            OpenTokenList tokenParam

        Liquidity.OpenMaturityList pair ->
            OpenMaturityList pair

        Liquidity.OpenInputMaturity pair ->
            OpenInputMaturity pair

        Liquidity.OpenConnect ->
            OpenConnect

        Liquidity.OpenSettings ->
            OpenSettings

        Liquidity.Approve erc20 ->
            Approve erc20

        Liquidity.Liquidity writeLiquidity ->
            Liquidity writeLiquidity

        Liquidity.Create writeCreate ->
            Create writeCreate


subscriptions : Page -> Sub Msg
subscriptions page =
    case page of
        LendPage { transaction } ->
            [ transaction
                |> Lend.subscriptions
                |> Sub.map LendMsg
            ]
                |> Sub.batch

        BorrowPage { transaction } ->
            [ transaction
                |> Borrow.subscriptions
                |> Sub.map BorrowMsg
            ]
                |> Sub.batch

        LiquidityPage { transaction } ->
            [ transaction
                |> Liquidity.subscriptions
                |> Sub.map LiquidityMsg
            ]
                |> Sub.batch


toTab : Page -> Tab
toTab page =
    case page of
        LendPage _ ->
            Tab.Lend

        BorrowPage _ ->
            Tab.Borrow

        LiquidityPage _ ->
            Tab.Liquidity


toParameter : Page -> Maybe Parameter
toParameter page =
    case page of
        LendPage { transaction } ->
            transaction
                |> Lend.toParameter

        BorrowPage { transaction } ->
            transaction
                |> Borrow.toParameter

        LiquidityPage { transaction } ->
            transaction
                |> Liquidity.toParameter


toPoolInfo : Page -> Maybe (Or Price PoolInfo)
toPoolInfo page =
    case page of
        LendPage { transaction } ->
            transaction
                |> Lend.toPoolInfo

        BorrowPage { transaction } ->
            transaction
                |> Borrow.toPoolInfo

        LiquidityPage { transaction } ->
            transaction
                |> Liquidity.toPoolInfo


view :
    { model
        | time : Posix
        , offset : Offset
        , chosenZone : ChosenZone
        , spot : PriceFeed
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
            LendPage { transaction } ->
                [ transaction
                    |> Lend.view model blockchain
                    |> map LendMsg
                ]

            BorrowPage { transaction } ->
                [ transaction
                    |> Borrow.view model blockchain
                    |> map BorrowMsg
                ]

            LiquidityPage { transaction } ->
                [ transaction
                    |> Liquidity.view model blockchain
                    |> map LiquidityMsg
                ]
        )
