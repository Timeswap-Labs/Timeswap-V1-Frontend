port module Page.Position.Liq.Main exposing
    ( Effect(..)
    , Msg
    , Position
    , init
    , subscriptions
    , update
    )

import Blockchain.Main as Blockchain exposing (Blockchain)
import Blockchain.User.Liq exposing (Liq)
import Blockchain.User.Main as User exposing (User)
import Blockchain.User.Return exposing (Return)
import Blockchain.User.WriteBurn exposing (WriteBurn)
import Data.Chain exposing (Chain)
import Data.Maturity as Maturity
import Data.Or exposing (Or(..))
import Data.Pool exposing (Pool)
import Data.Remote as Remote exposing (Remote(..))
import Data.Web exposing (Web)
import Http
import Json.Decode as Decode
import Json.Encode exposing (Value)
import Page.Answer as PoolInfoAnswer
import Page.PoolInfo exposing (PoolInfo)
import Page.Position.Liq.Error exposing (Error)
import Page.Position.Liq.Query as Query
import Page.Position.Liq.Tooltip exposing (Tooltip)
import Page.Query as PoolInfoQuery
import Process
import Sort.Dict as Dict
import Task
import Time exposing (Posix)


type Position
    = Position
        { pool : Pool
        , return : Web ( PoolInfo, Status )
        , tooltip : Maybe Tooltip
        }


type alias Status =
    Maturity.Status (Remote Error Return) (Remote Error Float)


type Msg
    = ClickAddMore
    | ClickBurn
    | ClickReturn
    | Tick Posix
    | PoolInfoQueryAgain
    | QueryAgain Posix
    | ReceivePoolInfo Chain Pool (Result Http.Error PoolInfoAnswer.Answer)
    | ReceiveAnswer Value
    | OnMouseEnter Tooltip
    | OnMouseLeave


type Effect
    = InputPool Pool
    | Burn WriteBurn


init :
    { model | time : Posix }
    -> Blockchain
    -> Pool
    -> ( Position, Cmd Msg )
init { time } blockchain pool =
    ( { pool = pool
      , return = Remote.loading
      , tooltip = Nothing
      }
        |> Position
    , if
        pool.maturity
            |> Maturity.isActive time
      then
        Cmd.none

      else
        get blockchain pool
    )


update :
    { model | time : Posix }
    -> Blockchain
    -> User
    -> Msg
    -> Position
    -> ( Maybe Position, Cmd Msg, Maybe Effect )
update { time } blockchain user msg (Position position) =
    case msg of
        ClickAddMore ->
            ( Nothing
            , Cmd.none
            , InputPool position.pool |> Just
            )

        ClickBurn ->
            ( Position position |> Just
            , Cmd.none
            , if
                position.pool.maturity
                    |> Maturity.isActive time
              then
                Nothing

              else
                user
                    |> User.getLiqs
                    |> Remote.map (Dict.get position.pool)
                    |> (Remote.map << Maybe.andThen)
                        (\liquidityIn ->
                            { pool = position.pool
                            , liquidityIn = liquidityIn
                            }
                                |> Burn
                                |> Just
                        )
                    |> Remote.withDefault Nothing
            )

        ClickReturn ->
            ( Nothing
            , Cmd.none
            , Nothing
            )

        Tick posix ->
            ( { position
                | return =
                    case position.return of
                        Success ( poolInfo, status ) ->
                            case status of
                                Maturity.Active remote ->
                                    ( poolInfo
                                    , remote
                                        |> Remote.update posix
                                        |> Maturity.Active
                                    )
                                        |> Success

                                Maturity.Matured remote ->
                                    ( poolInfo
                                    , remote
                                        |> Remote.update posix
                                        |> Maturity.Matured
                                    )
                                        |> Success

                        _ ->
                            position.return
                                |> Remote.update posix
              }
                |> Position
                |> Just
            , Cmd.none
            , Nothing
            )

        PoolInfoQueryAgain ->
            ( position
                |> Position
                |> Just
            , get blockchain position.pool
            , Nothing
            )

        QueryAgain posix ->
            ( { position
                | return =
                    case
                        ( position.pool.maturity
                            |> Maturity.isActive posix
                        , position.return
                        )
                    of
                        ( False, Success ( poolInfo, Maturity.Active _ ) ) ->
                            ( poolInfo
                            , Maturity.Matured Remote.loading
                            )
                                |> Success

                        _ ->
                            position.return
              }
                |> Position
                |> Just
            , case position.return of
                Success ( poolInfo, _ ) ->
                    user
                        |> User.getLiqs
                        |> Remote.map (Dict.get position.pool)
                        |> (Remote.map << Maybe.map)
                            (\liquidityIn ->
                                query blockchain poolInfo liquidityIn position
                            )
                        |> (Remote.map << Maybe.withDefault) Cmd.none
                        |> Remote.withDefault Cmd.none

                _ ->
                    Cmd.none
            , Nothing
            )

        ReceivePoolInfo chain pool result ->
            if
                (chain == (blockchain |> Blockchain.toChain))
                    && (pool == position.pool)
            then
                case result of
                    Ok (Right poolInfo) ->
                        ( { position
                            | return =
                                case
                                    ( position.pool.maturity
                                        |> Maturity.isActive time
                                    , position.return
                                    )
                                of
                                    ( True, Success ( _, (Maturity.Active _) as status ) ) ->
                                        ( poolInfo
                                        , status
                                        )
                                            |> Success

                                    ( False, Success ( _, (Maturity.Matured _) as status ) ) ->
                                        ( poolInfo
                                        , status
                                        )
                                            |> Success

                                    ( True, _ ) ->
                                        ( poolInfo
                                        , Maturity.Active Remote.loading
                                        )
                                            |> Success

                                    ( False, _ ) ->
                                        ( poolInfo
                                        , Maturity.Matured Remote.loading
                                        )
                                            |> Success
                          }
                            |> Position
                            |> Just
                        , Process.sleep 5000
                            |> Task.perform (\_ -> PoolInfoQueryAgain)
                        , Nothing
                        )

                    Ok (Left _) ->
                        ( position
                            |> Position
                            |> Just
                            |> Debug.log "fix"
                        , Process.sleep 5000
                            |> Task.perform (\_ -> PoolInfoQueryAgain)
                        , Nothing
                        )

                    Err error ->
                        ( { position | return = error |> Failure }
                            |> Position
                            |> Just
                        , Process.sleep 5000
                            |> Task.perform (\_ -> PoolInfoQueryAgain)
                        , Nothing
                        )

            else
                ( position
                    |> Position
                    |> Just
                , Process.sleep 5000
                    |> Task.perform (\_ -> PoolInfoQueryAgain)
                , Nothing
                )

        ReceiveAnswer value ->
            case
                ( value |> Decode.decodeValue Query.decoder
                , position.return
                )
            of
                ( Ok answer, Success ( poolInfo, status ) ) ->
                    if
                        (answer.chainId == (blockchain |> Blockchain.toChain))
                            && (answer.pool == position.pool)
                            && (answer.poolInfo == poolInfo)
                    then
                        ( { position
                            | return =
                                ( poolInfo
                                , case answer.result of
                                    Ok (Maturity.Active float) ->
                                        Success float
                                            |> Maturity.Active

                                    Ok (Maturity.Matured return) ->
                                        Success return
                                            |> Maturity.Matured

                                    Err error ->
                                        case status of
                                            Maturity.Active _ ->
                                                Failure error
                                                    |> Maturity.Active

                                            Maturity.Matured _ ->
                                                Failure error
                                                    |> Maturity.Matured
                                )
                                    |> Success
                          }
                            |> Position
                            |> Just
                        , Cmd.none
                        , Nothing
                        )

                    else
                        ( position
                            |> Position
                            |> Just
                        , Cmd.none
                        , Nothing
                        )

                _ ->
                    ( position
                        |> Position
                        |> Just
                    , Cmd.none
                    , Nothing
                    )

        OnMouseEnter tooltip ->
            ( { position | tooltip = Just tooltip }
                |> Position
                |> Just
            , Cmd.none
            , Nothing
            )

        OnMouseLeave ->
            ( { position | tooltip = Nothing }
                |> Position
                |> Just
            , Cmd.none
            , Nothing
            )


get :
    Blockchain
    -> Pool
    -> Cmd Msg
get blockchain pool =
    blockchain
        |> Blockchain.toChain
        |> (\chain ->
                Http.get
                    { url =
                        pool
                            |> PoolInfoQuery.toUrlString chain
                    , expect =
                        PoolInfoAnswer.decoder
                            |> Http.expectJson
                                (ReceivePoolInfo chain pool)
                    }
           )


query :
    Blockchain
    -> PoolInfo
    -> Liq
    ->
        { position
            | pool : Pool
        }
    -> Cmd Msg
query blockchain poolInfo liquidityIn { pool } =
    { chainId = blockchain |> Blockchain.toChain
    , pool = pool
    , poolInfo = poolInfo
    , liquidityIn = liquidityIn
    }
        |> Query.givenLiq
        |> queryLiq


port queryStake : Value -> Cmd msg


port queryLiq : Value -> Cmd msg


subscriptions : Position -> Sub Msg
subscriptions (Position { return }) =
    [ return
        |> Remote.map Tuple.second
        |> Remote.map
            (\status ->
                case status of
                    Maturity.Active remote ->
                        remote |> Remote.subscriptions Tick

                    Maturity.Matured remote ->
                        remote |> Remote.subscriptions Tick
            )
        |> Remote.withDefault
            (return
                |> Remote.subscriptions Tick
            )
    , Time.every 1000 QueryAgain
    ]
        |> Sub.batch
