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
import Data.Balances as Balances exposing (Balances)
import Data.Device as Device exposing (Device)
import Data.Images exposing (Images)
import Data.Pair as Pair
import Data.Pool exposing (Pool)
import Data.Pools as Pools exposing (Pools)
import Data.Remote exposing (Remote(..))
import Data.TokenImages exposing (TokenImages)
import Data.Tokens exposing (Tokens)
import Data.Uint as Uint
import Data.ZoneInfo exposing (ZoneInfo)
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
import Modals.Lend.Warning as Warning
import Time exposing (Posix)
import Utility.Color as Color
import Utility.Exit as Exit
import Utility.Glass as Glass
import Utility.Image as Image
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
    | SwitchLendSetting Bool
    | Slide Float
    | InputBondOut String
    | InputInsuranceOut String


type alias Msgs =
    { inputAssetIn : String -> Msg
    , inputMax : Msg
    , switchLendSetting : Bool -> Msg
    , slide : Float -> Msg
    , inputBondOut : String -> Msg
    , inputInsuranceOut : String -> Msg
    }


update :
    { model | user : Maybe { user | balances : Remote Balances } }
    -> Msg
    -> Modal
    -> ( Modal, Cmd Msg )
update { user } msg (Modal modal) =
    case msg of
        InputAssetIn string ->
            if
                (string |> Input.isFloat)
                    && (string |> Uint.isAmount (modal.pool.pair |> Pair.toAsset))
            then
                ( { modal
                    | assetIn = string
                    , claimsOut =
                        if string |> Input.isZero then
                            modal.claimsOut |> ClaimsOut.updateAssetInZero

                        else
                            modal.claimsOut |> ClaimsOut.updateAssetIn
                    , apr =
                        if
                            (string |> Input.isZero)
                                || (modal.claimsOut |> ClaimsOut.hasZeroInput)
                        then
                            Success ""

                        else
                            Loading
                    , cf =
                        if
                            (string |> Input.isZero)
                                || (modal.claimsOut |> ClaimsOut.hasZeroInput)
                        then
                            Success ""

                        else
                            Loading
                  }
                    |> Modal
                , Cmd.none
                  -- add command
                )

            else
                ( Modal modal, Cmd.none )

        InputMax ->
            ( (user
                |> Maybe.map
                    (\{ balances } ->
                        case balances of
                            Loading ->
                                modal

                            Failure ->
                                modal

                            Success successBalances ->
                                successBalances
                                    |> Balances.get (modal.pool.pair |> Pair.toAsset)
                                    |> (\string ->
                                            { modal
                                                | assetIn = string
                                                , claimsOut =
                                                    if string |> Input.isZero then
                                                        modal.claimsOut |> ClaimsOut.updateAssetInZero

                                                    else
                                                        modal.claimsOut |> ClaimsOut.updateAssetIn
                                                , apr =
                                                    if
                                                        (string |> Input.isZero)
                                                            || (modal.claimsOut |> ClaimsOut.hasZeroInput)
                                                    then
                                                        Success ""

                                                    else
                                                        Loading
                                                , cf =
                                                    if
                                                        (string |> Input.isZero)
                                                            || (modal.claimsOut |> ClaimsOut.hasZeroInput)
                                                    then
                                                        Success ""

                                                    else
                                                        Loading
                                            }
                                       )
                    )
                |> Maybe.withDefault modal
              )
                |> Modal
            , Cmd.none
              -- add command
            )

        SwitchLendSetting checked ->
            ( { modal
                | claimsOut =
                    if modal.assetIn |> Input.isZero then
                        modal.claimsOut |> ClaimsOut.switchLendSettingZero checked

                    else
                        modal.claimsOut |> ClaimsOut.switchLendSetting checked
                , apr =
                    if modal.assetIn |> Input.isZero then
                        Success ""

                    else if checked || (modal |> ClaimsOut.isDefault) then
                        modal.apr

                    else
                        Loading
                , cf =
                    if modal.assetIn |> Input.isZero then
                        Success ""

                    else if checked || (modal |> ClaimsOut.isDefault) then
                        modal.cf

                    else
                        Loading
              }
                |> Modal
            , Cmd.none
              -- add command
            )

        Slide float ->
            ( { modal
                | claimsOut =
                    if modal.assetIn |> Input.isZero then
                        ClaimsOut.slideZero float

                    else
                        ClaimsOut.slide float
                , apr =
                    if modal.assetIn |> Input.isZero then
                        Success ""

                    else
                        Loading
                , cf =
                    if modal.assetIn |> Input.isZero then
                        Success ""

                    else
                        Loading
              }
                |> Modal
            , Cmd.none
              -- add command
            )

        InputBondOut string ->
            if
                (string |> Input.isFloat)
                    && (string |> Uint.isAmount (modal.pool.pair |> Pair.toAsset))
            then
                ( { modal
                    | claimsOut =
                        if modal.assetIn |> Input.isZero then
                            modal.claimsOut |> ClaimsOut.updateBondOutZero string

                        else
                            modal.claimsOut |> ClaimsOut.updateBondOut string
                    , apr =
                        if
                            (modal.assetIn |> Input.isZero)
                                || (string |> Input.isZero)
                        then
                            Success ""

                        else
                            Loading
                    , cf =
                        if
                            (modal.assetIn |> Input.isZero)
                                || (string |> Input.isZero)
                        then
                            Success ""

                        else
                            Loading
                  }
                    |> Modal
                , Cmd.none
                  -- add command
                )

            else
                ( Modal modal, Cmd.none )

        InputInsuranceOut string ->
            if
                (string |> Input.isFloat)
                    && (string |> Uint.isAmount (modal.pool.pair |> Pair.toCollateral))
            then
                ( { modal
                    | claimsOut =
                        if modal.assetIn |> Input.isZero then
                            modal.claimsOut |> ClaimsOut.updateInsuranceOutZero string

                        else
                            modal.claimsOut |> ClaimsOut.updateInsuranceOut string
                    , apr =
                        if (modal.assetIn |> Input.isZero) || (string |> Input.isZero) then
                            Success ""

                        else
                            Loading
                    , cf =
                        if (modal.assetIn |> Input.isZero) || (string |> Input.isZero) then
                            Success ""

                        else
                            Loading
                  }
                    |> Modal
                , Cmd.none
                  -- add command
                )

            else
                ( Modal modal, Cmd.none )


msgs : Msgs
msgs =
    { inputAssetIn = InputAssetIn
    , inputMax = InputMax
    , switchLendSetting = SwitchLendSetting
    , slide = Slide
    , inputBondOut = InputBondOut
    , inputInsuranceOut = InputInsuranceOut
    }


view :
    { model
        | device : Device
        , time : Posix
        , zoneInfo : Maybe ZoneInfo
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
        , time : Posix
        , zoneInfo : Maybe ZoneInfo
        , backdrop : Backdrop
        , images : Images
        , tokenImages : TokenImages
        , user : Maybe { user | balances : Remote Balances }
    }
    ->
        { modal
            | pool : Pool
            , assetIn : String
            , claimsOut : ClaimsOut
            , apr : Remote String
            , cf : Remote String
        }
    -> Element Msg
content ({ images } as model) modal =
    column
        [ width fill
        , height shrink
        , spacing 12
        ]
        [ AssetIn.view msgs model modal
        , Image.arrowDown images
            [ width <| px 14
            , centerX
            ]
        , ClaimsOut.view msgs model modal
        , Warning.view model modal
        ]
