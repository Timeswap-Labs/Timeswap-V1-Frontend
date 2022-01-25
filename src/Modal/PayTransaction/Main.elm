port module Modal.PayTransaction.Main exposing (Effect(..), Modal, Msg, init, update)

import Blockchain.Main as Blockchain exposing (Blockchain)
import Blockchain.User.Dues as Dues
import Blockchain.User.Main as User exposing (User)
import Blockchain.User.TokenId as TokenId exposing (TokenId)
import Blockchain.User.WritePay exposing (WritePay)
import Data.Chain exposing (Chain)
import Data.ERC20 exposing (ERC20)
import Data.Pair as Pair
import Data.Pool exposing (Pool)
import Data.Remote as Remote exposing (Remote(..))
import Data.Uint as Uint exposing (Uint)
import Json.Decode as Decode
import Json.Encode exposing (Value)
import Modal.PayTransaction.Error exposing (Error)
import Modal.PayTransaction.Query as Query
import Modal.PayTransaction.Tooltip exposing (Tooltip)
import Modal.PayTransaction.Total exposing (Total)
import Process
import Sort.Dict as Dict exposing (Dict)
import Sort.Set as Set exposing (Set)
import Task
import Utility.Maybe as Maybe


type Modal
    = Modal
        { pool : Pool
        , state : State
        , total : Remote Error Total
        , tooltip : Maybe Tooltip
        }


type State
    = Full (Set TokenId)
    | Custom (Dict TokenId CustomInfo)


type alias CustomInfo =
    { assetIn : String
    , collateralOut : Remote Error Uint
    }


type Mode
    = RepayFull
    | RepayCustom


type Msg
    = SwitchMode Mode
    | InputAssetIn TokenId String
    | QuerySum ()
    | QueryProportion TokenId ()
    | ClickApprove
    | ClickPay
    | ReceiveSum Value
    | ReceiveProportion Value
    | Exit


type Effect
    = Approve ERC20
    | Pay WritePay


init :
    Blockchain
    -> User
    -> Pool
    -> Set TokenId
    -> ( Modal, Cmd Msg )
init blockchain user pool set =
    ( { pool = pool
      , state = set |> Full
      , total = Remote.loading
      , tooltip = Nothing
      }
        |> Modal
    , user
        |> User.getDues
        |> Remote.map (Dues.getMultiple pool set)
        |> (Remote.map << Maybe.map) Dict.values
        |> (Remote.map << Maybe.map << List.map)
            (\{ debt, collateral } ->
                { assetIn = debt
                , collateralOut = collateral
                }
            )
        |> (Remote.map << Maybe.map)
            (\duesIn ->
                { chain = blockchain |> Blockchain.toChain
                , pool = pool
                , duesIn = duesIn
                }
                    |> Query.sum
                    |> querySum
            )
        |> (Remote.map << Maybe.withDefault) Cmd.none
        |> Remote.withDefault Cmd.none
    )


update :
    Blockchain
    -> User
    -> Msg
    -> Modal
    -> ( Maybe Modal, Cmd Msg, Maybe Effect )
update blockchain user msg (Modal modal) =
    case ( msg, modal.state ) of
        ( SwitchMode RepayFull, Custom dict ) ->
            dict
                |> Dict.keys
                |> Set.fromList TokenId.sorter
                |> (\set ->
                        ( { modal
                            | state = set |> Full
                            , total = Remote.loading
                          }
                            |> Modal
                            |> Just
                        , user
                            |> User.getDues
                            |> Remote.map (Dues.getMultiple modal.pool set)
                            |> (Remote.map << Maybe.map) Dict.values
                            |> (Remote.map << Maybe.map << List.map)
                                (\{ debt, collateral } ->
                                    { assetIn = debt
                                    , collateralOut = collateral
                                    }
                                )
                            |> (Remote.map << Maybe.map)
                                (\duesIn ->
                                    { chain = blockchain |> Blockchain.toChain
                                    , pool = modal.pool
                                    , duesIn = duesIn
                                    }
                                        |> Query.sum
                                        |> querySum
                                )
                            |> (Remote.map << Maybe.withDefault) Cmd.none
                            |> Remote.withDefault Cmd.none
                        , Nothing
                        )
                   )

        ( SwitchMode RepayCustom, Full set ) ->
            { modal
                | state =
                    user
                        |> User.getDues
                        |> Remote.map (Dues.getMultiple modal.pool set)
                        |> (Remote.map << Maybe.map << Dict.map)
                            (\_ { debt, collateral } ->
                                { assetIn =
                                    debt
                                        |> Uint.toAmount
                                            (modal.pool.pair |> Pair.toAsset)
                                , collateralOut = collateral |> Success
                                }
                            )
                        |> (Remote.map << Maybe.withDefault) (Dict.empty TokenId.sorter)
                        |> Remote.withDefault (Dict.empty TokenId.sorter)
                        |> Custom
            }
                |> noCmdAndEffect

        ( InputAssetIn tokenId assetIn, Custom dict ) ->
            if assetIn |> Uint.isAmount (modal.pool.pair |> Pair.toAsset) then
                ( { modal
                    | state =
                        dict
                            |> Dict.insert tokenId
                                { assetIn = assetIn
                                , collateralOut = Remote.loading
                                }
                            |> Dict.map
                                (\_ customInfo ->
                                    case customInfo.collateralOut of
                                        Failure _ ->
                                            { customInfo
                                                | collateralOut = Remote.loading
                                            }

                                        _ ->
                                            customInfo
                                )
                            |> Custom
                    , total = Remote.loading
                  }
                    |> Modal
                    |> Just
                , user
                    |> User.getDues
                    |> Remote.map (Dict.get modal.pool)
                    |> (Remote.map << Maybe.andThen) (Dict.get tokenId)
                    |> (Remote.map << Maybe.map)
                        (\due assetInUint ->
                            { chain = blockchain |> Blockchain.toChain
                            , pool = modal.pool
                            , tokenId = tokenId
                            , due = due
                            , assetIn = assetInUint
                            }
                                |> Query.proportion
                                |> queryProportion
                        )
                    |> (Remote.map << Maybe.apply)
                        (assetIn
                            |> Uint.fromAmount
                                (modal.pool.pair |> Pair.toAsset)
                        )
                    |> (Remote.map << Maybe.withDefault) Cmd.none
                    |> Remote.withDefault Cmd.none
                , Nothing
                )

            else
                modal |> noCmdAndEffect

        ( QuerySum (), Full set ) ->
            case modal.total of
                Failure _ ->
                    ( { modal
                        | total = Remote.loading
                      }
                        |> Modal
                        |> Just
                    , user
                        |> User.getDues
                        |> Remote.map (Dues.getMultiple modal.pool set)
                        |> (Remote.map << Maybe.map) Dict.values
                        |> (Remote.map << Maybe.map << List.map)
                            (\{ debt, collateral } ->
                                { assetIn = debt
                                , collateralOut = collateral
                                }
                            )
                        |> (Remote.map << Maybe.map)
                            (\duesIn ->
                                { chain = blockchain |> Blockchain.toChain
                                , pool = modal.pool
                                , duesIn = duesIn
                                }
                                    |> Query.sum
                                    |> querySum
                            )
                        |> (Remote.map << Maybe.withDefault) Cmd.none
                        |> Remote.withDefault Cmd.none
                    , Nothing
                    )

                _ ->
                    modal |> noCmdAndEffect

        ( QuerySum (), Custom dict ) ->
            case modal.total of
                Failure _ ->
                    ( { modal
                        | total = Remote.loading
                      }
                        |> Modal
                        |> Just
                    , dict
                        |> Dict.values
                        |> List.foldr
                            (\dueIn accumulator ->
                                accumulator
                                    |> Maybe.andThen
                                        (\list ->
                                            case
                                                ( dueIn.assetIn
                                                    |> Uint.fromAmount
                                                        (modal.pool.pair |> Pair.toAsset)
                                                , dueIn.collateralOut
                                                )
                                            of
                                                ( Just assetIn, Success collateralOut ) ->
                                                    { assetIn = assetIn
                                                    , collateralOut = collateralOut
                                                    }
                                                        :: list
                                                        |> Just

                                                _ ->
                                                    Nothing
                                        )
                            )
                            (Just [])
                        |> Maybe.map
                            (\duesIn ->
                                { chain = blockchain |> Blockchain.toChain
                                , pool = modal.pool
                                , duesIn = duesIn
                                }
                                    |> Query.sum
                                    |> querySum
                            )
                        |> Maybe.withDefault Cmd.none
                    , Nothing
                    )

                _ ->
                    modal |> noCmdAndEffect

        ( QueryProportion tokenId (), Custom dict ) ->
            case
                dict
                    |> Dict.get tokenId
            of
                Just customInfo ->
                    case customInfo.collateralOut of
                        Failure _ ->
                            ( { modal
                                | state =
                                    dict
                                        |> Dict.insert tokenId
                                            { customInfo
                                                | collateralOut =
                                                    Remote.loading
                                            }
                                        |> Custom
                              }
                                |> Modal
                                |> Just
                            , user
                                |> User.getDues
                                |> Remote.map (Dict.get modal.pool)
                                |> (Remote.map << Maybe.andThen) (Dict.get tokenId)
                                |> (Remote.map << Maybe.map)
                                    (\due assetInUint ->
                                        { chain = blockchain |> Blockchain.toChain
                                        , pool = modal.pool
                                        , tokenId = tokenId
                                        , due = due
                                        , assetIn = assetInUint
                                        }
                                            |> Query.proportion
                                            |> queryProportion
                                    )
                                |> (Remote.map << Maybe.apply)
                                    (customInfo.assetIn
                                        |> Uint.fromAmount
                                            (modal.pool.pair |> Pair.toAsset)
                                    )
                                |> (Remote.map << Maybe.withDefault) Cmd.none
                                |> Remote.withDefault Cmd.none
                            , Nothing
                            )

                        _ ->
                            modal |> noCmdAndEffect

                _ ->
                    modal |> noCmdAndEffect

        ( ReceiveSum value, Full set ) ->
            case value |> Decode.decodeValue Query.decoderSum of
                Ok answer ->
                    if
                        (answer.chain == (blockchain |> Blockchain.toChain))
                            && (answer.pool == modal.pool)
                            && (user
                                    |> User.getDues
                                    |> Remote.map (Dues.getMultiple modal.pool set)
                                    |> (Remote.map << Maybe.map) Dict.values
                                    |> (Remote.map << Maybe.map << List.map)
                                        (\{ debt, collateral } ->
                                            { assetIn = debt
                                            , collateralOut = collateral
                                            }
                                        )
                                    |> (Remote.map << Maybe.map) ((==) answer.duesIn)
                                    |> (Remote.map << Maybe.withDefault) False
                                    |> Remote.withDefault False
                               )
                    then
                        ( { modal
                            | total =
                                answer.result
                                    |> toRemote
                          }
                            |> Modal
                            |> Just
                        , answer.result
                            |> Result.map (\_ -> Cmd.none)
                            |> Result.withDefault
                                (Process.sleep 5000
                                    |> Task.perform QuerySum
                                )
                        , Nothing
                        )

                    else
                        modal |> noCmdAndEffect

                _ ->
                    modal |> noCmdAndEffect

        ( ReceiveSum value, Custom dict ) ->
            case value |> Decode.decodeValue Query.decoderSum of
                Ok answer ->
                    if
                        (answer.chain == (blockchain |> Blockchain.toChain))
                            && (answer.pool == modal.pool)
                            && (dict
                                    |> Dict.values
                                    |> List.foldr
                                        (\dueIn accumulator ->
                                            accumulator
                                                |> Maybe.andThen
                                                    (\list ->
                                                        case
                                                            ( dueIn.assetIn
                                                                |> Uint.fromAmount
                                                                    (modal.pool.pair |> Pair.toAsset)
                                                            , dueIn.collateralOut
                                                            )
                                                        of
                                                            ( Just assetIn, Success collateralOut ) ->
                                                                { assetIn = assetIn
                                                                , collateralOut = collateralOut
                                                                }
                                                                    :: list
                                                                    |> Just

                                                            _ ->
                                                                Nothing
                                                    )
                                        )
                                        (Just [])
                                    |> Maybe.map ((==) answer.duesIn)
                                    |> Maybe.withDefault False
                               )
                    then
                        ( { modal
                            | total =
                                answer.result
                                    |> toRemote
                          }
                            |> Modal
                            |> Just
                        , answer.result
                            |> Result.map (\_ -> Cmd.none)
                            |> Result.withDefault
                                (Process.sleep 5000
                                    |> Task.perform QuerySum
                                )
                        , Nothing
                        )

                    else
                        modal |> noCmdAndEffect

                _ ->
                    modal |> noCmdAndEffect

        ( ReceiveProportion value, Custom dict ) ->
            case value |> Decode.decodeValue Query.decoderProportion of
                Ok answer ->
                    if
                        (answer.chain == (blockchain |> Blockchain.toChain))
                            && (answer.pool == modal.pool)
                            && (user
                                    |> User.getDues
                                    |> Remote.map (Dict.get modal.pool)
                                    |> (Remote.map << Maybe.andThen)
                                        (Dict.get answer.tokenId)
                                    |> (Remote.map << Maybe.map) ((==) answer.due)
                                    |> (Remote.map << Maybe.withDefault) False
                                    |> Remote.withDefault False
                               )
                            && (dict
                                    |> Dict.get answer.tokenId
                                    |> Maybe.map
                                        (\{ assetIn } ->
                                            (answer.assetIn
                                                |> Uint.toAmount
                                                    (modal.pool.pair |> Pair.toAsset)
                                            )
                                                == assetIn
                                        )
                                    |> Maybe.withDefault False
                               )
                    then
                        ( { modal
                            | state =
                                dict
                                    |> Dict.get answer.tokenId
                                    |> Maybe.map
                                        (\customInfo ->
                                            dict
                                                |> Dict.insert answer.tokenId
                                                    { customInfo
                                                        | collateralOut =
                                                            answer.result
                                                                |> toRemote
                                                    }
                                        )
                                    |> Maybe.withDefault dict
                                    |> Custom
                          }
                            |> Modal
                            |> Just
                        , answer.result
                            |> Result.map
                                (\_ ->
                                    dict
                                        |> Dict.values
                                        |> List.foldr
                                            (\dueIn accumulator ->
                                                accumulator
                                                    |> Maybe.andThen
                                                        (\list ->
                                                            case
                                                                ( dueIn.assetIn
                                                                    |> Uint.fromAmount
                                                                        (modal.pool.pair |> Pair.toAsset)
                                                                , dueIn.collateralOut
                                                                )
                                                            of
                                                                ( Just assetIn, Success collateralOut ) ->
                                                                    { assetIn = assetIn
                                                                    , collateralOut = collateralOut
                                                                    }
                                                                        :: list
                                                                        |> Just

                                                                _ ->
                                                                    Nothing
                                                        )
                                            )
                                            (Just [])
                                        |> Maybe.map
                                            (\duesIn ->
                                                { chain = blockchain |> Blockchain.toChain
                                                , pool = modal.pool
                                                , duesIn = duesIn
                                                }
                                                    |> Query.sum
                                                    |> querySum
                                            )
                                        |> Maybe.withDefault Cmd.none
                                )
                            |> Result.withDefault
                                (Process.sleep 5000
                                    |> Task.perform (QueryProportion answer.tokenId)
                                )
                        , Nothing
                        )

                    else
                        modal |> noCmdAndEffect

                _ ->
                    modal |> noCmdAndEffect

        _ ->
            ( Nothing, Cmd.none, Nothing ) |> Debug.log "remove"


toRemote :
    Result Error answer
    -> Remote Error answer
toRemote result =
    case result of
        Ok claims ->
            Success claims

        Err error ->
            Failure error


noCmdAndEffect :
    { pool : Pool
    , state : State
    , total : Remote Error Total
    , tooltip : Maybe Tooltip
    }
    -> ( Maybe Modal, Cmd Msg, Maybe Effect )
noCmdAndEffect modal =
    ( modal
        |> Modal
        |> Just
    , Cmd.none
    , Nothing
    )


port queryProportion : Value -> Cmd msg


port querySum : Value -> Cmd msg
