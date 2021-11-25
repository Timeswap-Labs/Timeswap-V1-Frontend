module Page.Transaction.Main exposing (..)

import Data.Maturity as Maturity
import Data.Pair exposing (Pair)
import Data.Parameter as Parameter exposing (Parameter)
import Data.Pool exposing (Pool)
import Data.Remote exposing (Remote(..))
import Data.Token exposing (Token)
import Data.TokenParam exposing (TokenParam)
import Page.Transaction.Error exposing (Error)
import Page.Transaction.PoolInfo exposing (PoolInfo)
import Page.Transaction.State as State exposing (State)
import Time exposing (Posix)


type Section transaction create
    = None
    | Asset Token
    | Collateral Token
    | Pair Pair
    | Pool
        Pool
        (Remote
            (Error transaction)
            (State transaction create)
        )


type Msg
    = SelectToken TokenParam
    | SelectMaturity


type Effect
    = OpenTokenList TokenParam
    | OpenMaturityList Pair


init :
    { model | time : Posix }
    -> Maybe Parameter
    ->
        ( Section transaction create
        , Cmd Msg
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
                ( Loading |> Pool pool
                , Debug.todo "http call"
                )

            else
                ( State.Matured
                    |> Success
                    |> Pool pool
                , Cmd.none
                )


initGivenPool :
    transaction
    -> { model | time : Posix }
    -> Pool
    -> PoolInfo
    -> Section transaction create
initGivenPool initTransaction { time } pool poolInfo =
    (if pool.maturity |> Maturity.isActive time then
        initTransaction
            |> State.Active poolInfo

     else
        State.Matured
    )
        |> Success
        |> Pool pool


update :
    Msg
    -> Section transaction create
    ->
        ( Section transaction create
        , Cmd Msg
        , Maybe Effect
        )
update msg section =
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

        ( SelectMaturity, Pool pool _ ) ->
            ( section
            , Cmd.none
            , pool.pair
                |> OpenMaturityList
                |> Just
            )

        _ ->
            ( section
            , Cmd.none
            , Nothing
            )


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

        Pool pool _ ->
            Parameter.Pool pool
                |> Just


toPoolInfo : Section transaction create -> Maybe PoolInfo
toPoolInfo section =
    case section of
        Pool _ (Success (State.Active poolInfo _)) ->
            poolInfo |> Just

        _ ->
            Nothing
