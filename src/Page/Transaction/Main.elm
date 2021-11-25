module Page.Transaction.Main exposing (..)

import Blockchain.Main exposing (Blockchain)
import Data.Chains exposing (Chains)
import Data.Maturity as Maturity
import Data.Pair exposing (Pair)
import Data.Parameter as Parameter exposing (Parameter)
import Data.Pool exposing (Pool)
import Data.Remote exposing (Remote(..))
import Data.Slippage exposing (Slippage)
import Data.Token exposing (Token)
import Data.TokenParam exposing (TokenParam)
import Element
    exposing
        ( Element
        , alignTop
        , centerX
        , centerY
        , column
        , el
        , height
        , map
        , none
        , px
        , row
        , shrink
        , spacing
        , text
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Page.Transaction.Error exposing (Error)
import Page.Transaction.PoolInfo exposing (PoolInfo)
import Page.Transaction.State as State exposing (State)
import Time exposing (Posix)
import Utility.Color as Color


type Section transaction create
    = None
    | Asset Token
    | Collateral Token
    | Pair Pair
    | Pool
        { pool : Pool
        , state : Remote (Error transaction) (State transaction create)
        }


type Msg transactionMsg
    = SelectToken TokenParam
    | SelectMaturity
    | TransactionMsg transactionMsg
    | CheckMaturity Posix


type Effect transactionEffect
    = OpenTokenList TokenParam
    | OpenMaturityList Pair
    | TransactionEffect transactionEffect


init :
    { model | time : Posix }
    -> Maybe Parameter
    ->
        ( Section transaction create
        , Cmd (Msg transactionMsg)
        )
init { time } parameter =
    case parameter of
        Nothing ->
            ( None
            , Cmd.none
            )

        Just (Parameter.Asset asset) ->
            ( Asset asset
            , Cmd.none
            )

        Just (Parameter.Collateral collateral) ->
            ( Collateral collateral
            , Cmd.none
            )

        Just (Parameter.Pair pair) ->
            ( Pair pair
            , Cmd.none
            )

        Just (Parameter.Pool pool) ->
            if pool.maturity |> Maturity.isActive time then
                ( { pool = pool
                  , state = Loading
                  }
                    |> Pool
                , Debug.todo "http call"
                )

            else
                ( { pool = pool
                  , state = State.Matured |> Success
                  }
                    |> Pool
                , Cmd.none
                )


initGivenPool :
    transaction
    -> { model | time : Posix }
    -> Pool
    -> PoolInfo
    -> Section transaction create
initGivenPool initTransaction { time } pool poolInfo =
    { pool = pool
    , state =
        (if pool.maturity |> Maturity.isActive time then
            { poolInfo = poolInfo
            , transaction = initTransaction
            }
                |> State.Active

         else
            State.Matured
        )
            |> Success
    }
        |> Pool


update :
    ({ model | chains : Chains, slippage : Slippage }
     -> Blockchain
     -> Pool
     -> PoolInfo
     -> transactionMsg
     -> transaction
     -> ( transaction, Cmd transactionMsg, Maybe transactionEffect )
    )
    -> { model | chains : Chains, slippage : Slippage }
    -> Blockchain
    -> Msg transactionMsg
    -> Section transaction create
    ->
        ( Section transaction create
        , Cmd (Msg transactionMsg)
        , Maybe (Effect transactionEffect)
        )
update transactionUpdate model blockchain msg section =
    case ( msg, section ) of
        ( SelectToken tokenParam, _ ) ->
            ( section
            , Cmd.none
            , tokenParam
                |> OpenTokenList
                |> Just
            )

        ( SelectMaturity, Pair pair ) ->
            ( section
            , Cmd.none
            , pair
                |> OpenMaturityList
                |> Just
            )

        ( SelectMaturity, Pool { pool } ) ->
            ( section
            , Cmd.none
            , pool.pair
                |> OpenMaturityList
                |> Just
            )

        ( TransactionMsg transactionMsg, Pool { pool, state } ) ->
            case state of
                Success (State.Active { poolInfo, transaction }) ->
                    transaction
                        |> transactionUpdate model
                            blockchain
                            pool
                            poolInfo
                            transactionMsg
                        |> (\( updated, cmd, maybeEffect ) ->
                                ( { pool = pool
                                  , state =
                                        { poolInfo = poolInfo
                                        , transaction = updated
                                        }
                                            |> State.Active
                                            |> Success
                                  }
                                    |> Pool
                                , cmd |> Cmd.map TransactionMsg
                                , maybeEffect |> Maybe.map TransactionEffect
                                )
                           )

                _ ->
                    ( section
                    , Cmd.none
                    , Nothing
                    )

        ( CheckMaturity posix, Pool ({ pool } as state) ) ->
            ( if pool.maturity |> Maturity.isActive posix then
                state |> Pool

              else
                { state
                    | state =
                        State.Matured
                            |> Success
                }
                    |> Pool
            , Cmd.none
            , Nothing
            )

        _ ->
            ( section
            , Cmd.none
            , Nothing
            )


subscriptions :
    Sub transactionMsg
    -> Section transaction create
    -> Sub (Msg transactionMsg)
subscriptions sub section =
    [ case section of
        Pool { state } ->
            case state of
                Success (State.Active _) ->
                    Time.every 1000 CheckMaturity

                _ ->
                    Sub.none

        _ ->
            Sub.none
    , case section of
        Pool { state } ->
            case state of
                Success (State.Active _) ->
                    sub |> Sub.map TransactionMsg

                _ ->
                    Sub.none

        _ ->
            Sub.none
    ]
        |> Sub.batch


toParameter : Section transaction create -> Maybe Parameter
toParameter section =
    case section of
        None ->
            Nothing

        Asset asset ->
            Parameter.Asset asset
                |> Just

        Collateral collateral ->
            Parameter.Collateral collateral
                |> Just

        Pair pair ->
            Parameter.Pair pair
                |> Just

        Pool { pool } ->
            Parameter.Pool pool
                |> Just


toPoolInfo : Section transaction create -> Maybe PoolInfo
toPoolInfo section =
    case section of
        Pool { state } ->
            case state of
                Success (State.Active { poolInfo }) ->
                    poolInfo |> Just

                _ ->
                    Nothing

        _ ->
            Nothing


view :
    { first : Element transactionMsg, second : Element transactionMsg }
    -> Element (Msg transactionMsg)
view { first, second } =
    row
        [ width shrink
        , height shrink
        , spacing 20
        ]
        [ column
            [ width shrink
            , height shrink
            , spacing 14
            , alignTop
            ]
            [ el
                [ width <| px 335
                , height <| px 199
                , Background.color Color.light500
                , Border.rounded 8
                ]
                none
            , first |> map TransactionMsg
            ]
        , column
            [ width shrink
            , height shrink
            , alignTop
            , spacing 14
            ]
            [ second |> map TransactionMsg
            , button
            , button2
            ]
        ]


button : Element msg
button =
    el
        [ width <| px 335
        , height <| px 44
        , Background.color Color.primary500
        , Border.rounded 8
        ]
        (el [ centerX, centerY ] (text "Approve"))


button2 : Element msg
button2 =
    el
        [ width <| px 335
        , height <| px 44
        , Background.color Color.light500
        , Border.rounded 8
        ]
        (el [ centerX, centerY ] (text "Lend"))
