module Modal.MaturityList.Main exposing
    ( Effect(..)
    , Modal
    , Msg
    , init
    , update
    , view
    )

import Blockchain.Main as Blockchain exposing (Blockchain)
import Data.Backdrop exposing (Backdrop)
import Data.Chain exposing (Chain)
import Data.Chains exposing (Chains)
import Data.ChosenZone exposing (ChosenZone)
import Data.Images exposing (Images)
import Data.Maturity exposing (Maturity)
import Data.Offset exposing (Offset)
import Data.Pair exposing (Pair)
import Data.Pool exposing (Pool)
import Data.Remote exposing (Remote(..))
import Data.Web as Web exposing (Web)
import Element
    exposing
        ( Element
        , alignRight
        , alpha
        , behindContent
        , centerX
        , centerY
        , column
        , el
        , fill
        , height
        , none
        , padding
        , paddingXY
        , px
        , row
        , shrink
        , spacing
        , text
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Http
import Modal.MaturityList.Answer as Answer exposing (Answer)
import Modal.MaturityList.Pools exposing (Pools)
import Modal.MaturityList.Query as Query
import Modal.MaturityList.Sorting as Sorting exposing (Sorting)
import Modal.MaturityList.Tooltip as Tooltip exposing (Tooltip)
import Process
import Sort.Dict as Dict
import Task
import Time exposing (Posix)
import Utility.Color as Color
import Utility.Duration as Duration
import Utility.Glass as Glass
import Utility.IconButton as IconButton
import Utility.Image as Image


type Modal
    = Modal
        { pair : Pair
        , sorting : Sorting
        , pools : Web Pools
        , tooltip : Maybe Tooltip
        }


type Msg
    = GoToSortMaturity
    | GoToSortLiquidity
    | SelectMaturity Maturity
    | QueryAgain
    | ReceiveAnswer Chain Pair (Result Http.Error Answer)
    | OnMouseEnter Tooltip
    | OnMouseLeave
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
      , tooltip = Nothing
      }
        |> Modal
    , get blockchain pair
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

        ( QueryAgain, _ ) ->
            ( modal |> Modal |> Just
            , get blockchain modal.pair
            , Nothing
            )

        ( ReceiveAnswer chain pair result, _ ) ->
            if
                (chain == (blockchain |> Blockchain.toChain))
                    && (pair == modal.pair)
            then
                ( { modal | pools = result |> Web.fromResult }
                    |> Modal
                    |> Just
                , Process.sleep 5000
                    |> Task.perform (\_ -> QueryAgain)
                , Nothing
                )

            else
                ( modal |> Modal |> Just
                , Cmd.none
                , Nothing
                )

        ( OnMouseEnter tooltip, _ ) ->
            ( { modal | tooltip = Just tooltip }
                |> Modal
                |> Just
            , Cmd.none
            , Nothing
            )

        ( OnMouseLeave, _ ) ->
            ( { modal | tooltip = Nothing }
                |> Modal
                |> Just
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


get :
    Blockchain
    -> Pair
    -> Cmd Msg
get blockchain pair =
    blockchain
        |> Blockchain.toChain
        |> (\chain ->
                Http.get
                    { url = pair |> Query.toUrlString chain
                    , expect =
                        Answer.decoder
                            |> Http.expectJson (ReceiveAnswer chain pair)
                    }
           )


view :
    { model
        | time : Posix
        , offset : Offset
        , chosenZone : ChosenZone
        , backdrop : Backdrop
        , images : Images
    }
    -> Modal
    -> Element Msg
view ({ time, offset, chosenZone } as model) (Modal ({ tooltip } as modal)) =
    Glass.outsideModal model
        { onClick = Exit
        , modal =
            column
                [ width <| px 375
                , height shrink
                , padding 24
                , spacing 16
                , centerX
                , centerY
                , Background.color Color.background
                , Border.rounded 8
                , Border.color Color.transparent100
                , Border.width 1
                ]
                [ row
                    [ width fill
                    , height shrink
                    ]
                    [ el
                        [ width shrink
                        , height shrink
                        , centerY
                        , Font.size 18
                        , paddingXY 0 3
                        , Font.color Color.light100
                        ]
                        (text "Select Maturity")
                    , IconButton.exit model Exit
                    ]
                , column
                    []
                    (case modal.pools of
                        Success pools ->
                            pools
                                |> Dict.toList
                                |> List.map
                                    (\( maturity, summary ) ->
                                        el
                                            []
                                            (Duration.viewMaturity
                                                { onMouseEnter = OnMouseEnter
                                                , onMouseLeave = OnMouseLeave
                                                , tooltip = Tooltip.Maturity maturity
                                                , opened = tooltip
                                                , time = time
                                                , offset = offset
                                                , chosenZone = chosenZone
                                                , maturity = maturity
                                                }
                                            )
                                    )

                        _ ->
                            []
                    )
                ]
        }
