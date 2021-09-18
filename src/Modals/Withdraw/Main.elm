port module Modals.Withdraw.Main exposing
    ( Modal
    , Msg
    , fromFragment
    , getPool
    , same
    , update
    , view
    )

import Browser.Navigation as Navigation exposing (Key)
import Data.Address exposing (Address)
import Data.Backdrop exposing (Backdrop)
import Data.Device as Device exposing (Device)
import Data.Images exposing (Images)
import Data.Maturity as Maturity
import Data.Pool exposing (Pool)
import Data.Pools as Pools exposing (Pools)
import Data.Positions exposing (Positions)
import Data.TokenImages exposing (TokenImages)
import Data.Tokens exposing (Tokens)
import Element
    exposing
        ( Element
        , alignBottom
        , centerX
        , centerY
        , column
        , el
        , fill
        , height
        , inFront
        , paddingXY
        , px
        , shrink
        , spacing
        , text
        , width
        )
import Element.Font as Font
import Json.Encode exposing (Value)
import Modals.Withdraw.ClaimsIn as ClaimsIn
import Modals.Withdraw.Tooltip exposing (Tooltip)
import Modals.Withdraw.Transaction as Transaction
import Page exposing (Page)
import Time exposing (Posix)
import Utility.Color as Color
import Utility.Exit as Exit
import Utility.Glass as Glass


type Modal
    = Modal
        { pool : Pool
        , tooltip : Maybe Tooltip
        }


init : Pool -> Modal
init pool =
    { pool = pool
    , tooltip = Nothing
    }
        |> Modal


fromFragment :
    { model | time : Posix, tokens : Tokens, pools : Pools }
    -> String
    -> Maybe Modal
fromFragment { time, tokens, pools } string =
    string
        |> Pools.fromPoolFragment tokens pools
        |> Maybe.andThen
            (\({ maturity } as pool) ->
                if maturity |> Maturity.isActive time |> not then
                    Just pool

                else
                    Nothing
            )
        |> Maybe.map init


same : Modal -> Modal -> Bool
same (Modal pool1) (Modal pool2) =
    pool1 == pool2


getPool : Modal -> Pool
getPool (Modal { pool }) =
    pool


type Msg
    = Withdraw Value
    | OnMouseEnter Tooltip
    | OnMouseLeave


type alias Msgs =
    { withdraw : Value -> Msg
    , onMouseEnter : Tooltip -> Msg
    , onMouseLeave : Msg
    }


update : { model | key : Key, page : Page } -> Msg -> Modal -> ( Modal, Cmd Msg )
update { key, page } msg (Modal modal) =
    case msg of
        Withdraw value ->
            ( Modal modal
            , Cmd.batch
                [ withdraw value
                , page
                    |> Page.toUrl
                    |> Navigation.pushUrl key
                ]
            )

        OnMouseEnter tooltip ->
            ( { modal | tooltip = Just tooltip }
                |> Modal
            , Cmd.none
            )

        OnMouseLeave ->
            ( { modal | tooltip = Nothing }
                |> Modal
            , Cmd.none
            )


msgs : Msgs
msgs =
    { withdraw = Withdraw
    , onMouseEnter = OnMouseEnter
    , onMouseLeave = OnMouseLeave
    }


port withdraw : Value -> Cmd msg


view :
    { model
        | device : Device
        , backdrop : Backdrop
        , time : Posix
        , images : Images
        , tokenImages : TokenImages
    }
    -> { user | address : Address }
    -> Positions
    -> Modal
    -> Element Msg
view ({ device, backdrop, images } as model) user positions (Modal modal) =
    column
        ([ paddingXY 32 20
         , spacing 20
         , centerX
         , centerY
         , Exit.button images |> inFront
         ]
            ++ Glass.lightPrimaryModal backdrop 0
            ++ (if Device.isPhone device then
                    [ width fill
                    , height shrink
                    , alignBottom
                    ]

                else
                    [ width <| px 533
                    , height shrink
                    ]
               )
        )
        [ title
        , ClaimsIn.view msgs model positions modal
        , Transaction.view msgs model user positions modal
        ]


title : Element msg
title =
    el
        [ width shrink
        , height shrink
        , paddingXY 0 4
        , Font.bold
        , Font.size 24
        , Font.color Color.light100
        ]
        (text "Claim your assets")
