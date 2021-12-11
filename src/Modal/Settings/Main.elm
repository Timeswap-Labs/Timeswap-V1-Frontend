port module Modal.Settings.Main exposing
    ( Effect(..)
    , Modal
    , Msg
    , init
    , update
    , view
    )

import Data.Backdrop exposing (Backdrop)
import Data.Deadline as Deadline exposing (Deadline)
import Data.Images exposing (Images)
import Data.Or exposing (Or(..))
import Data.Slippage as Slippage exposing (Slippage)
import Data.Spot as Spot exposing (Spot)
import Element
    exposing
        ( Element
        , centerX
        , centerY
        , column
        , el
        , fill
        , height
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
import Element.Font as Font
import Element.Input as Input
import Json.Encode exposing (Value)
import Modal.Settings.Tooltip exposing (Tooltip)
import Utility.Color as Color
import Utility.Glass as Glass
import Utility.IconButton as IconButton
import Utility.Input as Input


type Modal
    = Modal
        { slippage : Or Slippage.Option String
        , deadline : Or Deadline.Option String
        , spot : Spot
        , tooltip : Maybe Tooltip
        }


type Msg
    = ChooseSlippageOption Slippage.Option
    | ChooseDeadlineOption Deadline.Option
    | ChooseSpot Spot
    | InputSlippage String
    | InputDeadline String
    | OnMouseEnter Tooltip
    | OnMouseLeave
    | Exit


type Effect
    = UpdateSettings Slippage Deadline Spot


init :
    { model
        | slippage : Slippage
        , deadline : Deadline
        , spot : Spot
    }
    -> Modal
init { slippage, deadline, spot } =
    { slippage = slippage |> Slippage.toSettings
    , deadline = deadline |> Deadline.toSettings
    , spot = spot
    , tooltip = Nothing
    }
        |> Modal


update : Msg -> Modal -> ( Maybe Modal, Cmd Msg, Maybe Effect )
update msg (Modal modal) =
    case msg of
        ChooseSlippageOption option ->
            ( { modal | slippage = option |> Left }
                |> Modal
                |> Just
            , Cmd.none
            , Nothing
            )

        ChooseDeadlineOption option ->
            ( { modal | deadline = option |> Left }
                |> Modal
                |> Just
            , Cmd.none
            , Nothing
            )

        ChooseSpot spot ->
            ( { modal | spot = spot }
                |> Modal
                |> Just
            , Cmd.none
            , Nothing
            )

        InputSlippage input ->
            ( if input |> Input.isFloat then
                { modal | slippage = input |> Right }
                    |> Modal
                    |> Just

              else
                modal |> Modal |> Just
            , Cmd.none
            , Nothing
            )

        InputDeadline input ->
            ( if input |> Input.isFloat then
                { modal | deadline = input |> Right }
                    |> Modal
                    |> Just

              else
                modal |> Modal |> Just
            , Cmd.none
            , Nothing
            )

        OnMouseEnter tooltip ->
            ( { modal | tooltip = Just tooltip }
                |> Modal
                |> Just
            , Cmd.none
            , Nothing
            )

        OnMouseLeave ->
            ( { modal | tooltip = Nothing }
                |> Modal
                |> Just
            , Cmd.none
            , Nothing
            )

        Exit ->
            (\slippage deadline ->
                ( Nothing
                , [ slippage |> Slippage.encode |> cacheSlippage
                  , deadline |> Deadline.encode |> cacheDeadline
                  , modal.spot |> Spot.encode |> cacheSpot
                  ]
                    |> Cmd.batch
                , UpdateSettings slippage deadline modal.spot
                    |> Just
                )
            )
                (modal.slippage |> Slippage.fromSettings)
                (modal.deadline |> Deadline.fromSettings)


port cacheSlippage : Value -> Cmd msg


port cacheDeadline : Value -> Cmd msg


port cacheSpot : Value -> Cmd msg


view :
    { model
        | backdrop : Backdrop
        , images : Images
    }
    -> Modal
    -> Element Msg
view ({ backdrop } as model) modal =
    Glass.outsideModal model
        { onClick = Exit
        , modal =
            column
                [ width <| px 335
                , height <| px 300
                , centerX
                , centerY
                , Glass.background backdrop
                , Border.rounded 8
                , Border.color Color.transparent100
                , Border.width 1
                ]
                [ column
                    [ width fill
                    , height shrink
                    , spacing 16
                    ]
                    [ row
                        [ width fill
                        , height shrink
                        , spacing 16
                        ]
                        [ el
                            [ width shrink
                            , height shrink
                            , centerY
                            , Font.size 18
                            , paddingXY 0 3
                            , Font.color Color.light100
                            ]
                            (text "Settings")
                        , IconButton.exit model Exit
                        ]
                    ]
                ]
        }
