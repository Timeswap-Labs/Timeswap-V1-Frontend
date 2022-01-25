port module Modal.Pay.Main exposing (Effect(..), Modal, Msg, init)

import Blockchain.User.Dues as Dues
import Blockchain.User.Main as User exposing (User)
import Blockchain.User.TokenId exposing (TokenId)
import Blockchain.User.WritePay exposing (WritePay)
import Data.Chain exposing (Chain)
import Data.ERC20 exposing (ERC20)
import Data.Pool exposing (Pool)
import Data.Remote as Remote exposing (Remote)
import Data.Uint exposing (Uint)
import Json.Encode exposing (Value)
import Modal.Pay.Query as Query
import Modal.Pay.Tooltip exposing (Tooltip)
import Sort.Dict as Dict exposing (Dict)
import Sort.Set exposing (Set)
import Time exposing (Posix)


type Modal
    = Modal
        { pool : Pool
        , mode : Mode
        , total : Remote () Uint
        , tooltip : Maybe Tooltip
        }


type Mode
    = Full Full
    | Custom Custom


type alias Full =
    Set TokenId


type alias Custom =
    Dict TokenId CustomInfo


type alias CustomInfo =
    { assetIn : String
    , collateralOut : Remote () Uint
    }


type Msg
    = SwitchMode Mode
    | InputString TokenId
    | ClickApprove
    | ClickPay
    | ReceiveProportion Value
    | ReceiveSum Value
    | CheckMaturity Posix
    | Exit


type Effect
    = Approve ERC20
    | Pay WritePay


init : Chain -> User -> Pool -> Set TokenId -> ( Modal, Cmd Msg )
init chain user pool set =
    ( { pool = pool
      , mode = set |> Full
      , total = Remote.loading
      , tooltip = Nothing
      }
        |> Modal
    , user
        |> User.getDues
        |> Remote.map (Dues.getMultiple pool set)
        |> (Remote.map << Maybe.map) Dict.values
        |> (Remote.map << Maybe.map << List.map) .collateral
        |> (Remote.map << Maybe.map)
            (\collateralsOut ->
                { chain = chain
                , pool = pool
                , collateralsOut = collateralsOut
                }
                    |> Query.sum
                    |> querySum
            )
        |> (Remote.map << Maybe.withDefault) Cmd.none
        |> Remote.withDefault Cmd.none
    )


update : Msg -> Modal -> ( Maybe Modal, Cmd Msg, Maybe Effect )
update msg (Modal modal) =
    case ( msg, modal.mode ) of
        ( SwitchMode Full, Custom _ ) ->
            ( { modal
                |
              }
                |> Modal
                |> Just
            , user
                |> User.getDues
                |> Remote.map (Dues.getMultiple modal.pool set)
                |> (Remote.map << Maybe.map) Dict.values
                |> (Remote.map << Maybe.map << List.map) .collateral
                |> (Remote.map << Maybe.map)
                    (\collateralsOut ->
                        { chain = chain
                        , pool = pool
                        , collateralsOut = collateralsOut
                        }
                            |> Query.sum
                            |> querySum
                    )
                |> (Remote.map << Maybe.withDefault) Cmd.none
                |> Remote.withDefault Cmd.none
            , Nothing
            )

        _ ->
            ( Nothing, Cmd.none, Nothing ) |> Debug.log "remove"


port queryProportion : Value -> Cmd msg


port querySum : Value -> Cmd msg
