module Modal.MaturityList.Main exposing
    ( Effect(..)
    , Modal
    , Msg
    , init
    , update
    )

import Blockchain.Main exposing (Blockchain)
import Data.Maturity exposing (Maturity)
import Data.Pair exposing (Pair)
import Data.Pool exposing (Pool)
import Data.Remote exposing (Remote(..))
import Data.Web exposing (Web)
import Modal.MaturityList.Pools exposing (Pools)
import Modal.MaturityList.Sorting as Sorting exposing (Sorting)
import Sort.Dict as Dict


type Modal
    = Modal
        { pair : Pair
        , sorting : Sorting
        , pools : Web Pools
        }


type Msg
    = GoToSortMaturity
    | GoToSortLiquidity
    | SelectMaturity Maturity
    | Exit


type Effect
    = InputPool Pool


init :
    Blockchain
    -> Pair
    -> ( Modal, Cmd Msg )
init blockchain pair =
    ( { pair = pair
      , sorting = Sorting.Liquidity
      , pools = Loading
      }
        |> Modal
    , Debug.todo "http call"
    )


update :
    Blockchain
    -> Msg
    -> Modal
    -> ( Maybe Modal, Cmd Msg, Maybe Effect )
update blockchain msg (Modal modal) =
    case ( msg, modal.sorting ) of
        ( GoToSortMaturity, Sorting.Liquidity ) ->
            ( { modal | sorting = Sorting.Maturity }
                |> Modal
                |> Just
            , Cmd.none
            , Nothing
            )

        ( GoToSortLiquidity, Sorting.Maturity ) ->
            ( { modal | sorting = Sorting.Liquidity }
                |> Modal
                |> Just
            , Cmd.none
            , Nothing
            )

        ( SelectMaturity maturity, _ ) ->
            case modal.pools of
                Success pools ->
                    if maturity |> Dict.memberOf pools then
                        ( Nothing
                        , Cmd.none
                        , InputPool
                            { pair = modal.pair
                            , maturity = maturity
                            }
                            |> Just
                        )

                    else
                        ( modal |> Modal |> Just
                        , Cmd.none
                        , Nothing
                        )

                _ ->
                    ( modal |> Modal |> Just
                    , Cmd.none
                    , Nothing
                    )

        ( Exit, _ ) ->
            ( Nothing
            , Cmd.none
            , Nothing
            )

        _ ->
            ( modal |> Modal |> Just
            , Cmd.none
            , Nothing
            )
