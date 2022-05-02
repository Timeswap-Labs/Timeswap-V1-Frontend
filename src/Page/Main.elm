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

import Blockchain.Main as Blockchain exposing (Blockchain)
import Blockchain.User.Claim as Claim
import Blockchain.User.TokenId exposing (TokenId)
import Blockchain.User.WriteBorrow exposing (WriteBorrow)
import Blockchain.User.WriteBurn exposing (WriteBurn)
import Blockchain.User.WriteCreate exposing (WriteCreate)
import Blockchain.User.WriteLend exposing (WriteLend)
import Blockchain.User.WriteLiquidity exposing (WriteLiquidity)
import Blockchain.User.WriteWithdraw exposing (WriteWithdraw)
import Data.Backdrop exposing (Backdrop)
import Data.CDP exposing (CDP)
import Data.Chains exposing (Chains)
import Data.ChosenZone exposing (ChosenZone)
import Data.Device exposing (Device(..))
import Data.ERC20 exposing (ERC20)
import Data.Images exposing (Images)
import Data.Offset exposing (Offset)
import Data.Or exposing (Or(..))
import Data.Pair exposing (Pair)
import Data.Parameter as Parameter exposing (Parameter)
import Data.Pool exposing (Pool)
import Data.PriceFeed exposing (PriceFeed)
import Data.Slippage exposing (Slippage)
import Data.Support exposing (Support(..))
import Data.Tab as Tab exposing (Tab)
import Data.Theme exposing (Theme)
import Data.TokenParam exposing (TokenParam)
import Element
    exposing
        ( Element
        , alignTop
        , centerX
        , column
        , height
        , map
        , none
        , shrink
        , spacing
        , width
        )
import Page.Markets.Main as Markets exposing (PoolsData)
import Page.PoolInfo exposing (PoolInfo)
import Page.Position.Claim.Main as Claim
import Page.Position.Due.Main as Due
import Page.Position.Liq.Main as Liq
import Page.Positions.Claims.Main as Claims
import Page.Positions.Dues.Main as Dues
import Page.Positions.Liqs.Main as Liqs
import Page.Route as Route
import Page.Transaction.Borrow.Main as Borrow
import Page.Transaction.Lend.Main as Lend
import Page.Transaction.Liquidity.Main as Liquidity
import Page.Transaction.Price exposing (Price)
import Sort.Set exposing (Set)
import Time exposing (Posix)
import Url exposing (Url)
import Utility.Maybe as Maybe


type Page
    = LendPage
        { transaction : Lend.Transaction
        , positions : Claims.Positions
        , position : Maybe Claim.Position
        }
    | BorrowPage
        { transaction : Borrow.Transaction
        , positions : Dues.Positions
        , position : Maybe Due.Position
        }
    | LiquidityPage
        { transaction : Liquidity.Transaction
        , positions : Liqs.Positions
        , position : Maybe Liq.Position
        }
    | MarketsPage PoolsData


type Msg
    = LendMsg Lend.Msg
    | ClaimsMsg Claims.Msg
    | ClaimMsg Claim.Msg
    | BorrowMsg Borrow.Msg
    | DuesMsg Dues.Msg
    | DueMsg Due.Msg
    | LiquidityMsg Liquidity.Msg
    | LiqsMsg Liqs.Msg
    | LiqMsg Liq.Msg
    | MarketsMsg Markets.Msg


type Effect
    = OpenTokenList TokenParam
    | OpenMaturityList Pair
    | OpenInputMaturity Pair
    | OpenSettings
    | OpenConnect
    | OpenCaution WriteLend Float CDP PoolInfo
    | InputPool Pool
    | OpenPayTransaction Pool (Set TokenId)
    | Approve ERC20
    | Lend WriteLend
    | Borrow WriteBorrow
    | Liquidity WriteLiquidity
    | Create WriteCreate
    | Withdraw WriteWithdraw
    | Burn WriteBurn


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
                        { transaction = transaction
                        , positions = Claims.init
                        , position = Nothing
                        }
                            |> LendPage
                    )
                    (Cmd.map LendMsg)

        ( Just (Route.Lend (Just (Parameter.Pool pool))), Supported blockchain, Just (Left spot) ) ->
            spot
                |> Lend.initGivenSpot model blockchain pool
                |> Tuple.mapBoth
                    (\transaction ->
                        { transaction = transaction
                        , positions = Claims.init
                        , position = Nothing
                        }
                            |> LendPage
                    )
                    (Cmd.map LendMsg)

        ( Just (Route.Lend parameter), Supported blockchain, _ ) ->
            parameter
                |> Lend.init model blockchain
                |> Tuple.mapBoth
                    (\transaction ->
                        { transaction = transaction
                        , positions = Claims.init
                        , position = Nothing
                        }
                            |> LendPage
                    )
                    (Cmd.map LendMsg)

        ( Just (Route.Borrow (Just (Parameter.Pool pool))), Supported blockchain, Just (Right poolInfo) ) ->
            poolInfo
                |> Borrow.initGivenPoolInfo model blockchain pool
                |> Tuple.mapBoth
                    (\transaction ->
                        { transaction = transaction
                        , positions = Dues.init
                        , position = Nothing
                        }
                            |> BorrowPage
                    )
                    (Cmd.map BorrowMsg)

        ( Just (Route.Borrow (Just (Parameter.Pool pool))), Supported blockchain, Just (Left spot) ) ->
            spot
                |> Borrow.initGivenSpot model blockchain pool
                |> Tuple.mapBoth
                    (\transaction ->
                        { transaction = transaction
                        , positions = Dues.init
                        , position = Nothing
                        }
                            |> BorrowPage
                    )
                    (Cmd.map BorrowMsg)

        ( Just (Route.Borrow parameter), Supported blockchain, _ ) ->
            parameter
                |> Borrow.init model blockchain
                |> Tuple.mapBoth
                    (\transaction ->
                        { transaction = transaction
                        , positions = Dues.init
                        , position = Nothing
                        }
                            |> BorrowPage
                    )
                    (Cmd.map BorrowMsg)

        ( Just (Route.Liquidity (Just (Parameter.Pool pool))), Supported blockchain, Just (Right poolInfo) ) ->
            poolInfo
                |> Liquidity.initGivenPoolInfo model blockchain pool
                |> Tuple.mapBoth
                    (\transaction ->
                        { transaction = transaction
                        , positions = Liqs.init
                        , position = Nothing
                        }
                            |> LiquidityPage
                    )
                    (Cmd.map LiquidityMsg)

        ( Just (Route.Liquidity (Just (Parameter.Pool pool))), Supported blockchain, Just (Left spot) ) ->
            spot
                |> Liquidity.initGivenSpot model blockchain pool
                |> Tuple.mapBoth
                    (\transaction ->
                        { transaction = transaction
                        , positions = Liqs.init
                        , position = Nothing
                        }
                            |> LiquidityPage
                    )
                    (Cmd.map LiquidityMsg)

        ( Just (Route.Liquidity parameter), Supported blockchain, _ ) ->
            parameter
                |> Liquidity.init model blockchain
                |> Tuple.mapBoth
                    (\transaction ->
                        { transaction = transaction
                        , positions = Liqs.init
                        , position = Nothing
                        }
                            |> LiquidityPage
                    )
                    (Cmd.map LiquidityMsg)

        ( Just Route.Markets, Supported blockchain, _ ) ->
            Markets.init model blockchain chains
                |> Tuple.mapBoth
                    (\poolsData -> poolsData |> MarketsPage)
                    (Cmd.map MarketsMsg)

        ( _, Supported blockchain, _ ) ->
            Nothing
                |> Lend.init model blockchain
                |> Tuple.mapBoth
                    (\transaction ->
                        { transaction = transaction
                        , positions = Claims.init
                        , position = Nothing
                        }
                            |> LendPage
                    )
                    (Cmd.map LendMsg)

        ( Just (Route.Lend _), NotSupported _, _ ) ->
            ( { transaction = Lend.notSupported
              , positions = Claims.init
              , position = Nothing
              }
                |> LendPage
            , Cmd.none
            )

        ( Just (Route.Borrow _), NotSupported _, _ ) ->
            ( { transaction = Borrow.notSupported
              , positions = Dues.init
              , position = Nothing
              }
                |> BorrowPage
            , Cmd.none
            )

        ( Just (Route.Liquidity _), NotSupported _, _ ) ->
            ( { transaction = Liquidity.notSupported
              , positions = Liqs.init
              , position = Nothing
              }
                |> LiquidityPage
            , Cmd.none
            )

        _ ->
            ( { transaction = Lend.notSupported
              , positions = Claims.init
              , position = Nothing
              }
                |> LendPage
            , Cmd.none
            )


update :
    { model | time : Posix, slippage : Slippage, chains : Chains }
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
                            |> Maybe.map lendEffect
                        )
                   )

        ( ClaimsMsg claimsMsg, LendPage lendPage ) ->
            lendPage.positions
                |> Claims.update claimsMsg
                |> (\( updated, maybeEffect ) ->
                        maybeEffect
                            |> Maybe.map (claimsEffect model blockchain)
                            |> Maybe.map
                                (\( position, cmd ) ->
                                    ( { lendPage
                                        | positions = updated
                                        , position = position
                                      }
                                        |> LendPage
                                    , cmd
                                    , Nothing
                                    )
                                )
                            |> Maybe.withDefault
                                ( lendPage |> LendPage
                                , Cmd.none
                                , Nothing
                                )
                   )

        ( ClaimMsg claimMsg, LendPage lendPage ) ->
            Just
                (\user position ->
                    position
                        |> Claim.update blockchain user claimMsg
                        |> (\( updated, cmd, maybeEffect ) ->
                                ( { lendPage | position = updated }
                                    |> LendPage
                                , cmd |> Cmd.map ClaimMsg
                                , maybeEffect |> Maybe.map claimEffect
                                )
                           )
                )
                |> Maybe.apply (blockchain |> Blockchain.toUser)
                |> Maybe.apply lendPage.position
                |> Maybe.withDefault
                    ( page
                    , Cmd.none
                    , Nothing
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
                            |> Maybe.map borrowEffect
                        )
                   )

        ( DuesMsg duesMsg, BorrowPage borrowPage ) ->
            borrowPage.positions
                |> Dues.update duesMsg
                |> (\( updated, maybeEffect ) ->
                        maybeEffect
                            |> Maybe.map duesEffect
                            |> Maybe.map
                                (\position ->
                                    ( { borrowPage
                                        | positions = updated
                                        , position = position
                                      }
                                        |> BorrowPage
                                    , Cmd.none
                                    , Nothing
                                    )
                                )
                            |> Maybe.withDefault
                                ( borrowPage |> BorrowPage
                                , Cmd.none
                                , Nothing
                                )
                   )

        ( DueMsg dueMsg, BorrowPage borrowPage ) ->
            Just
                (\user position ->
                    position
                        |> Due.update user dueMsg
                        |> (\( updated, maybeEffect ) ->
                                ( { borrowPage | position = updated }
                                    |> BorrowPage
                                , Cmd.none
                                , maybeEffect |> Maybe.map dueEffect
                                )
                           )
                )
                |> Maybe.apply (blockchain |> Blockchain.toUser)
                |> Maybe.apply borrowPage.position
                |> Maybe.withDefault
                    ( page
                    , Cmd.none
                    , Nothing
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
                            |> Maybe.map liquidityEffect
                        )
                   )

        ( LiqsMsg liqsMsg, LiquidityPage liquidityPage ) ->
            liquidityPage.positions
                |> Liqs.update liqsMsg
                |> (\( updated, maybeEffect ) ->
                        maybeEffect
                            |> Maybe.map (liqsEffect model blockchain)
                            |> Maybe.map
                                (\( position, cmd ) ->
                                    ( { liquidityPage
                                        | positions = updated
                                        , position = position
                                      }
                                        |> LiquidityPage
                                    , cmd
                                    , Nothing
                                    )
                                )
                            |> Maybe.withDefault
                                ( liquidityPage |> LiquidityPage
                                , Cmd.none
                                , Nothing
                                )
                   )

        ( LiqMsg liqMsg, LiquidityPage liquidityPage ) ->
            Just
                (\user position ->
                    position
                        |> Liq.update model blockchain user liqMsg
                        |> (\( updated, cmd, maybeEffect ) ->
                                ( { liquidityPage | position = updated }
                                    |> LiquidityPage
                                , cmd |> Cmd.map LiqMsg
                                , maybeEffect |> Maybe.map liqEffect
                                )
                           )
                )
                |> Maybe.apply (blockchain |> Blockchain.toUser)
                |> Maybe.apply liquidityPage.position
                |> Maybe.withDefault
                    ( page
                    , Cmd.none
                    , Nothing
                    )

        ( MarketsMsg infoMsg, MarketsPage infoPage ) ->
            infoPage
                |> Markets.update
                    infoMsg
                    blockchain
                    model.chains
                |> (\( updated, cmd ) ->
                        ( updated
                            |> MarketsPage
                        , cmd |> Cmd.map MarketsMsg
                        , Nothing
                        )
                   )

        _ ->
            ( page, Cmd.none, Nothing )


lendEffect :
    Lend.Effect
    -> Effect
lendEffect effect =
    case effect of
        Lend.OpenTokenList tokenParam ->
            OpenTokenList tokenParam

        Lend.OpenMaturityList pair ->
            OpenMaturityList pair

        Lend.OpenConnect ->
            OpenConnect

        Lend.OpenSettings ->
            OpenSettings

        Lend.OpenCaution txn apr cdp poolInfo ->
            OpenCaution txn apr cdp poolInfo

        Lend.Approve erc20 ->
            Approve erc20

        Lend.Lend writeLend ->
            Lend writeLend


claimsEffect :
    { model | time : Posix }
    -> Blockchain
    -> Claims.Effect
    -> ( Maybe Claim.Position, Cmd Msg )
claimsEffect model blockchain effect =
    case effect of
        Claims.OpenClaim pool ->
            blockchain
                |> Blockchain.toUser
                |> Maybe.map
                    (\user ->
                        Claim.init model blockchain user pool
                            |> Tuple.mapBoth
                                Just
                                (Cmd.map ClaimMsg)
                    )
                |> Maybe.withDefault
                    ( Nothing
                    , Cmd.none
                    )


claimEffect :
    Claim.Effect
    -> Effect
claimEffect effect =
    case effect of
        Claim.InputPool pool ->
            InputPool pool

        Claim.Withdraw writeWithdraw ->
            Withdraw writeWithdraw


borrowEffect :
    Borrow.Effect
    -> Effect
borrowEffect effect =
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


duesEffect :
    Dues.Effect
    -> Maybe Due.Position
duesEffect effect =
    case effect of
        Dues.OpenDue pool ->
            Due.init pool
                |> Just


dueEffect :
    Due.Effect
    -> Effect
dueEffect effect =
    case effect of
        Due.InputPool pool ->
            InputPool pool

        Due.OpenPayTransaction pool set ->
            OpenPayTransaction pool set


liquidityEffect :
    Liquidity.Effect
    -> Effect
liquidityEffect effect =
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


liqsEffect :
    { model | time : Posix }
    -> Blockchain
    -> Liqs.Effect
    -> ( Maybe Liq.Position, Cmd Msg )
liqsEffect model blockchain effect =
    case effect of
        Liqs.OpenLiq pool ->
            Liq.init model blockchain pool
                |> Tuple.mapBoth
                    Just
                    (Cmd.map LiqMsg)


liqEffect :
    Liq.Effect
    -> Effect
liqEffect effect =
    case effect of
        Liq.InputPool pool ->
            InputPool pool

        Liq.Burn writeBurm ->
            Burn writeBurm


subscriptions : Page -> Sub Msg
subscriptions page =
    case page of
        LendPage { transaction, position } ->
            [ transaction
                |> Lend.subscriptions
                |> Sub.map LendMsg
            , position
                |> Maybe.map Claim.subscriptions
                |> (Maybe.map << Sub.map) ClaimMsg
                |> Maybe.withDefault Sub.none
            ]
                |> Sub.batch

        BorrowPage { transaction } ->
            [ transaction
                |> Borrow.subscriptions
                |> Sub.map BorrowMsg
            ]
                |> Sub.batch

        LiquidityPage { transaction, position } ->
            [ transaction
                |> Liquidity.subscriptions
                |> Sub.map LiquidityMsg
            , position
                |> Maybe.map Liq.subscriptions
                |> (Maybe.map << Sub.map) LiqMsg
                |> Maybe.withDefault Sub.none
            ]
                |> Sub.batch

        MarketsPage _ ->
            Sub.none


toTab : Page -> Tab
toTab page =
    case page of
        LendPage _ ->
            Tab.Lend

        BorrowPage _ ->
            Tab.Borrow

        LiquidityPage _ ->
            Tab.Liquidity

        MarketsPage _ ->
            Tab.Markets


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

        MarketsPage _ ->
            Nothing


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

        MarketsPage _ ->
            Nothing


view :
    { model
        | time : Posix
        , offset : Offset
        , chosenZone : ChosenZone
        , device : Device
        , priceFeed : PriceFeed
        , backdrop : Backdrop
        , images : Images
        , theme : Theme
        , chains : Chains
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
            LendPage lendPage ->
                lendPage.position
                    |> Maybe.map
                        (\position ->
                            Claim.view model position
                                |> map ClaimMsg
                                |> List.singleton
                        )
                    |> Maybe.withDefault
                        [ lendPage.transaction
                            |> Lend.view model blockchain
                            |> map LendMsg
                        , blockchain
                            |> Blockchain.toUser
                            |> Maybe.map
                                (\user -> Claims.view model user lendPage.positions)
                            |> (Maybe.map << map) ClaimsMsg
                            |> Maybe.withDefault none
                        ]

            BorrowPage borrowPage ->
                Just
                    (\position user ->
                        Due.view model (blockchain |> Blockchain.toChain) user position
                            |> map DueMsg
                            |> List.singleton
                    )
                    |> Maybe.apply borrowPage.position
                    |> Maybe.apply (blockchain |> Blockchain.toUser)
                    |> Maybe.withDefault
                        [ borrowPage.transaction
                            |> Borrow.view model blockchain
                            |> map BorrowMsg
                        , blockchain
                            |> Blockchain.toUser
                            |> Maybe.map
                                (\user -> Dues.view model user borrowPage.positions)
                            |> (Maybe.map << map) DuesMsg
                            |> Maybe.withDefault none
                        ]

            LiquidityPage liquidityPage ->
                Just
                    (\position user ->
                        Liq.view model user position
                            |> map LiqMsg
                            |> List.singleton
                    )
                    |> Maybe.apply liquidityPage.position
                    |> Maybe.apply (blockchain |> Blockchain.toUser)
                    |> Maybe.withDefault
                        [ liquidityPage.transaction
                            |> Liquidity.view model blockchain
                            |> map LiquidityMsg
                        , blockchain
                            |> Blockchain.toUser
                            |> Maybe.map
                                (\user -> Liqs.view model user liquidityPage.positions)
                            |> (Maybe.map << map) LiqsMsg
                            |> Maybe.withDefault none
                        ]

            MarketsPage poolsData ->
                Markets.view model poolsData
                    |> map MarketsMsg
                    |> List.singleton
        )
