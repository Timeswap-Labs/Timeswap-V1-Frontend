module Modal.MaturityList.Main exposing
    ( Effect(..)
    , Modal
    , Msg
    , init
    , update
    , view
    )

import Blockchain.Main exposing (Blockchain)
import Data.Maturity exposing (Maturity)
import Data.Pair exposing (Pair)
import Data.Pool exposing (Pool)
import Data.Remote exposing (Remote(..))
import Data.Web exposing (Web)
import Element
    exposing
        ( Element
        , alpha
        , behindContent
        , centerX
        , centerY
        , column
        , el
        , fill
        , height
        , none
        , px
        , width
        )
import Element.Background as Background
import Element.Events as Events
import Element.Input as Input
import Modal.MaturityList.Pools exposing (Pools)
import Modal.MaturityList.Sorting as Sorting exposing (Sorting)
import Sort.Dict as Dict
import Utility.Color as Color


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
    , Cmd.none |> Debug.log "http call"
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


view : Modal -> Element Msg
view modal =
    el
        [ width fill
        , height fill
        , el
            [ width fill
            , height fill
            , Background.color Color.dark500
            , alpha 0.1
            , Events.onClick Exit
            ]
            none
            |> behindContent
        ]
        (column
            [ width <| px 335
            , height <| px 300
            , centerX
            , centerY
            , Background.color Color.light100
            ]
            []
        )
