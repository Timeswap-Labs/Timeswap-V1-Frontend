module Modals.Lend.Main exposing
    ( Modal
    , Msg
    , fromFragment
    , getPool
    , same
    , update
    , view
    )

import Data.Backdrop exposing (Backdrop)
import Data.Balances exposing (Balances)
import Data.Device as Device exposing (Device)
import Data.Images exposing (Images)
import Data.Pool exposing (Pool)
import Data.Pools as Pools exposing (Pools)
import Data.Remote exposing (Remote(..))
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
import Modals.Lend.AssetIn as AssetIn
import Modals.Lend.ClaimsOut as ClaimsOut exposing (ClaimsOut)
import Utility.Color as Color
import Utility.Exit as Exit
import Utility.Glass as Glass
import Utility.Input as Input


type Modal
    = Modal
        { pool : Pool
        , assetIn : String
        , claimsOut : ClaimsOut
        , apr : Remote String
        , cf : Remote String
        }


init : Pool -> Modal
init pool =
    { pool = pool
    , assetIn = ""
    , claimsOut =
        { bond = ""
        , insurance = ""
        }
            |> Success
            |> ClaimsOut.Default
    , apr = Success ""
    , cf = Success ""
    }
        |> Modal


fromFragment : Tokens -> Pools -> String -> Maybe Modal
fromFragment tokens pools string =
    string
        |> Pools.fromPoolFragment tokens pools
        |> Maybe.map init


same : Modal -> Modal -> Bool
same (Modal modal1) (Modal modal2) =
    modal1.pool == modal2.pool


getPool : Modal -> Pool
getPool (Modal { pool }) =
    pool


type Msg
    = InputAssetIn String
    | InputMax


type alias Msgs =
    { inputAssetIn : String -> Msg
    , inputMax : Msg
    }


update : Msg -> Modal -> ( Modal, Cmd Msg )
update msg (Modal modal) =
    case msg of
        InputAssetIn string ->
            if string |> Input.isFloat then
                ( { modal
                    | assetIn = string
                    , claimsOut =
                        if string == "" then
                            modal.claimsOut |> ClaimsOut.updateEmptyAssetIn

                        else
                            modal.claimsOut |> ClaimsOut.updateAssetIn
                  }
                    |> Modal
                , Cmd.none
                  -- add command
                )

            else
                ( Modal modal, Cmd.none )

        InputMax ->
            ( Modal modal, Cmd.none )


msgs : Msgs
msgs =
    { inputAssetIn = InputAssetIn
    , inputMax = InputMax
    }


view :
    { model
        | device : Device
        , backdrop : Backdrop
        , images : Images
        , tokenImages : TokenImages
        , user : Maybe { user | balances : Remote Balances }
    }
    -> Modal
    -> Element Msg
view ({ device, backdrop, images } as model) (Modal modal) =
    column
        ([ paddingXY 32 20
         , spacing 20
         , centerX
         , centerY
         , Exit.button images |> inFront
         ]
            ++ Glass.darkPrimaryModal backdrop 0
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
        , content model modal
        ]


title : Element msg
title =
    el
        [ width shrink
        , height shrink
        , paddingXY 0 3
        , Font.bold
        , Font.size 18
        , Font.color Color.light100
        ]
        (text "Lend")


content :
    { model
        | device : Device
        , backdrop : Backdrop
        , tokenImages : TokenImages
        , user : Maybe { user | balances : Remote Balances }
    }
    -> { modal | pool : Pool, assetIn : String, claimsOut : ClaimsOut }
    -> Element Msg
content model modal =
    column
        [ width fill
        , height shrink
        , spacing 12
        ]
        [ AssetIn.view msgs model modal ]
