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
import Data.Chains exposing (Chains)
import Data.ChosenZone exposing (ChosenZone)
import Data.Images exposing (Images)
import Data.Maturity exposing (Maturity)
import Data.Pair exposing (Pair)
import Data.Pool exposing (Pool)
import Data.Remote exposing (Remote(..))
import Data.Web exposing (Web)
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
import Time exposing (Posix, Zone)
import Utility.Color as Color
import Utility.Duration as Duration
import Utility.Glass as Glass
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
    | ReceiveAnswer (Result Http.Error Answer)
    | OnMouseEnter Tooltip
    | OnMouseLeave
    | Exit


type Effect
    = InputPool Pool


init :
    { model | chains : Chains }
    -> Blockchain
    -> Pair
    -> ( Modal, Cmd Msg )
init model blockchain pair =
    ( { pair = pair
      , sorting = Sorting.Liquidity
      , pools = Loading
      , tooltip = Nothing
      }
        |> Modal
    , get model blockchain pair
    )


update :
    { model | chains : Chains }
    -> Blockchain
    -> Msg
    -> Modal
    -> ( Maybe Modal, Cmd Msg, Maybe Effect )
update model blockchain msg (Modal modal) =
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
            , get model blockchain modal.pair
            , Nothing
            )

        ( ReceiveAnswer (Ok answer), _ ) ->
            ( (if
                (answer.chainId == (blockchain |> Blockchain.toChain))
                    && (answer.pair == modal.pair)
               then
                { modal | pools = answer.result |> Success }

               else
                modal
              )
                |> Modal
                |> Just
            , Process.sleep 5000
                |> Task.perform (\_ -> QueryAgain)
            , Nothing
            )

        ( ReceiveAnswer (Err error), _ ) ->
            ( { modal | pools = Failure error }
                |> Modal
                |> Just
            , Process.sleep 5000
                |> Task.perform (\_ -> QueryAgain)
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


get : { model | chains : Chains } -> Blockchain -> Pair -> Cmd Msg
get model blockchain pair =
    Http.get
        { url = pair |> Query.toUrlString blockchain
        , expect =
            Answer.decoder model
                |> Http.expectJson ReceiveAnswer
        }


view :
    { model
        | time : Posix
        , zone : Zone
        , chosenZone : ChosenZone
        , backdrop : Backdrop
        , images : Images
    }
    -> Modal
    -> Element Msg
view ({ time, zone, chosenZone, backdrop, images } as model) (Modal ({ tooltip } as modal)) =
    Glass.outsideModal backdrop
        Exit
        (column
            [ width <| px 375
            , height shrink
            , padding 24
            , spacing 16
            , centerX
            , centerY
            , Glass.background backdrop
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
                , Input.button
                    [ width shrink
                    , height shrink
                    , alignRight
                    , centerY
                    ]
                    { onPress = Just Exit
                    , label =
                        images
                            |> Image.close
                                [ width <| px 24
                                , height <| px 24
                                ]
                    }
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
                                            , zone = zone
                                            , chosenZone = chosenZone
                                            , maturity = maturity
                                            }
                                        )
                                )

                    _ ->
                        []
                )
            ]
        )
