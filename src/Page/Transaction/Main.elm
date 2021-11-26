module Page.Transaction.Main exposing (..)

import Blockchain.Main as Blockchain exposing (Blockchain)
import Data.Chains exposing (Chains)
import Data.Deadline exposing (Deadline)
import Data.Maturity as Maturity
import Data.Pair as Pair exposing (Pair)
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
        , padding
        , px
        , row
        , shrink
        , spacing
        , text
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import Http
import Page.Transaction.Button as Button
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


type Msg transactionMsg createMsg
    = SelectToken TokenParam
    | SelectMaturity
    | ClickConnect
    | TransactionMsg transactionMsg
    | CreateMsg createMsg
    | CheckMaturity Posix


type Effect transactionEffect createEffect
    = OpenTokenList TokenParam
    | OpenMaturityList Pair
    | OpenConnect
    | TransactionEffect transactionEffect
    | CreateEffect createEffect


init :
    { model | time : Posix }
    -> Maybe Parameter
    ->
        ( Section transaction create
        , Cmd (Msg transactionMsg createMsg)
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
    { transaction :
        { model
            | time : Posix
            , chains : Chains
            , slippage : Slippage
            , deadline : Deadline
        }
        -> Blockchain
        -> Pool
        -> PoolInfo
        -> transactionMsg
        -> transaction
        -> ( transaction, Cmd transactionMsg, Maybe transactionEffect )
    , create :
        { model
            | time : Posix
            , chains : Chains
            , slippage : Slippage
            , deadline : Deadline
        }
        -> Blockchain
        -> Pool
        -> createMsg
        -> create
        -> ( create, Cmd createMsg, Maybe createEffect )
    }
    ->
        { model
            | time : Posix
            , chains : Chains
            , slippage : Slippage
            , deadline : Deadline
        }
    -> Blockchain
    -> Msg transactionMsg createMsg
    -> Section transaction create
    ->
        ( Section transaction create
        , Cmd (Msg transactionMsg createMsg)
        , Maybe (Effect transactionEffect createEffect)
        )
update updates model blockchain msg section =
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

        ( ClickConnect, _ ) ->
            ( section
            , Cmd.none
            , blockchain
                |> Blockchain.toUser
                |> Maybe.map (\_ -> Nothing)
                |> Maybe.withDefault (Just OpenConnect)
            )

        ( TransactionMsg transactionMsg, Pool { pool, state } ) ->
            case state of
                Success (State.Active { poolInfo, transaction }) ->
                    transaction
                        |> updates.transaction model
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

        ( CreateMsg createMsg, Pool { pool, state } ) ->
            case state of
                Success (State.DoesNotExist create) ->
                    create
                        |> updates.create model
                            blockchain
                            pool
                            createMsg
                        |> (\( updated, cmd, maybeEffect ) ->
                                ( { pool = pool
                                  , state =
                                        updated
                                            |> State.DoesNotExist
                                            |> Success
                                  }
                                    |> Pool
                                , cmd |> Cmd.map CreateMsg
                                , maybeEffect |> Maybe.map CreateEffect
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
    -> Sub (Msg transactionMsg createMsg)
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
    { transaction :
        Blockchain
        -> Pool
        -> transaction
        ->
            { first : Element transactionMsg
            , second : Element transactionMsg
            , buttons : Element transactionMsg
            }
    , create :
        Blockchain
        -> Pool
        -> create
        ->
            { first : Element createMsg
            , second : Element createMsg
            , buttons : Element createMsg
            }
    , disabled :
        Blockchain
        -> Pool
        -> transaction
        ->
            { first : Element Never
            , second : Element Never
            }
    , empty :
        { asset : Maybe Token
        , collateral : Maybe Token
        }
        ->
            { first : Element Never
            , second : Element Never
            }
    }
    -> Blockchain
    -> Section transaction create
    -> Element (Msg transactionMsg createMsg)
view views blockchain section =
    (case section of
        None ->
            views.empty
                { asset = Nothing
                , collateral = Nothing
                }
                |> (\{ first, second } ->
                        { first = first |> map never
                        , second = second |> map never
                        , buttons =
                            blockchain
                                |> Blockchain.toUser
                                |> Maybe.map (\_ -> Button.disabled "Incomplete")
                                |> Maybe.withDefault (Button.view ClickConnect "Connect Wallet")
                        }
                   )

        Asset asset ->
            views.empty
                { asset = Just asset
                , collateral = Nothing
                }
                |> (\{ first, second } ->
                        { first = first |> map never
                        , second = second |> map never
                        , buttons =
                            blockchain
                                |> Blockchain.toUser
                                |> Maybe.map (\_ -> Button.disabled "Incomplete")
                                |> Maybe.withDefault (Button.view ClickConnect "Connect Wallet")
                        }
                   )

        Collateral collateral ->
            views.empty
                { asset = Nothing
                , collateral = Just collateral
                }
                |> (\{ first, second } ->
                        { first = first |> map never
                        , second = second |> map never
                        , buttons =
                            blockchain
                                |> Blockchain.toUser
                                |> Maybe.map (\_ -> Button.disabled "Incomplete")
                                |> Maybe.withDefault
                                    (Button.view ClickConnect "Connect Wallet")
                        }
                   )

        Pair pair ->
            views.empty
                { asset = pair |> Pair.toAsset |> Just
                , collateral = pair |> Pair.toCollateral |> Just
                }
                |> (\{ first, second } ->
                        { first = first |> map never
                        , second = second |> map never
                        , buttons =
                            blockchain
                                |> Blockchain.toUser
                                |> Maybe.map (\_ -> Button.disabled "Incomplete")
                                |> Maybe.withDefault
                                    (Button.view ClickConnect "Connect Wallet")
                        }
                   )

        Pool { pool, state } ->
            case state of
                Loading ->
                    views.empty
                        { asset =
                            pool.pair
                                |> Pair.toAsset
                                |> Just
                        , collateral =
                            pool.pair
                                |> Pair.toCollateral
                                |> Just
                        }
                        |> (\{ first, second } ->
                                { first = first |> map never
                                , second = second |> map never
                                , buttons =
                                    blockchain
                                        |> Blockchain.toUser
                                        |> Maybe.map (\_ -> Button.disabled "Incomplete")
                                        |> Maybe.withDefault
                                            (Button.view ClickConnect "Connect Wallet")
                                }
                           )

                Failure { error, transaction } ->
                    transaction
                        |> views.disabled blockchain pool
                        |> (\{ first, second } ->
                                { first = first |> map never
                                , second = second |> map never
                                , buttons =
                                    (case error of
                                        Http.Timeout ->
                                            "Network Too Slow"

                                        Http.NetworkError ->
                                            "No Network Connection"

                                        Http.BadStatus status ->
                                            "Error Code: " ++ (status |> String.fromInt)

                                        _ ->
                                            "Error"
                                    )
                                        |> Button.error
                                }
                           )

                Success (State.Active { transaction }) ->
                    transaction
                        |> views.transaction blockchain pool
                        |> (\{ first, second, buttons } ->
                                { first = first |> map TransactionMsg
                                , second = second |> map TransactionMsg
                                , buttons = buttons |> map TransactionMsg
                                }
                           )

                Success (State.DoesNotExist create) ->
                    create
                        |> views.create blockchain pool
                        |> (\{ first, second, buttons } ->
                                { first = first |> map CreateMsg
                                , second = second |> map CreateMsg
                                , buttons = buttons |> map CreateMsg
                                }
                           )

                Success State.Matured ->
                    views.empty
                        { asset =
                            pool.pair
                                |> Pair.toAsset
                                |> Just
                        , collateral =
                            pool.pair
                                |> Pair.toCollateral
                                |> Just
                        }
                        |> (\{ first, second } ->
                                { first = first |> map never
                                , second = second |> map never
                                , buttons = Button.error "Already Matured"
                                }
                           )
    )
        |> (\{ first, second, buttons } ->
                column
                    [ width shrink
                    , height shrink
                    , padding 20
                    , spacing 20
                    , Background.color Color.light100
                    , Border.rounded 8
                    ]
                    [ el [] (text "Lend")
                    , row
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
                            , first
                            ]
                        , column
                            [ width shrink
                            , height shrink
                            , alignTop
                            , spacing 14
                            ]
                            [ second
                            , buttons
                            ]
                        ]
                    ]
           )
