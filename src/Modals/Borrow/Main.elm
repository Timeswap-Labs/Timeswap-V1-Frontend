port module Modals.Borrow.Main exposing
    ( Modal
    , Msg
    , fromFragment
    , getPool
    , same
    , subscriptions
    , update
    , view
    )

import Browser.Navigation as Navigation exposing (Key)
import Data.Address exposing (Address)
import Data.Allowances exposing (Allowances)
import Data.Backdrop exposing (Backdrop)
import Data.Balances as Balances exposing (Balances)
import Data.Deadline exposing (Deadline)
import Data.Device as Device exposing (Device)
import Data.Images exposing (Images)
import Data.Maturity as Maturity
import Data.Pair as Pair
import Data.Percent as Percent
import Data.Pool exposing (Pool)
import Data.Pools as Pools exposing (Pools)
import Data.Remote exposing (Remote(..))
import Data.Slippage exposing (Slippage)
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
import Json.Decode as Decode
import Json.Encode exposing (Value)
import Modals.Borrow.AssetOut as AssetOut
import Modals.Borrow.DuesOut as DuesOut exposing (DuesOut)
import Modals.Borrow.Query as Query
import Modals.Borrow.Tooltip exposing (Tooltip)
import Modals.Borrow.Transaction as Transaction
import Modals.Borrow.Warning as Warning
import Page exposing (Page)
import Time exposing (Posix)
import Utility.Color as Color
import Utility.Exit as Exit
import Utility.Glass as Glass
import Utility.Image as Image
import Utility.Input as Input


type Modal
    = Modal
        { pool : Pool
        , assetOut : String
        , duesOut : DuesOut
        , apr : Remote String
        , cf : Remote String
        , tooltip : Maybe Tooltip
        }


init : Pool -> Modal
init pool =
    { pool = pool
    , assetOut = ""
    , duesOut = DuesOut.init
    , apr = Success ""
    , cf = Success ""
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
                if maturity |> Maturity.isActive time then
                    Just pool

                else
                    Nothing
            )
        |> Maybe.map init


same : Modal -> Modal -> Bool
same (Modal modal1) (Modal modal2) =
    modal1.pool == modal2.pool


getPool : Modal -> Pool
getPool (Modal { pool }) =
    pool


type Msg
    = InputAssetOut String
    | SwitchBorrowSetting Bool
    | Slide Float
    | InputDebtIn String
    | InputCollateralIn String
    | InputMax
    | ApproveBorrow Value
    | Borrow Value
    | ReceiveTime Posix
    | SdkBorrowMsg Value
    | OnMouseEnter Tooltip
    | OnMouseLeave


type alias Msgs =
    { inputAssetOut : String -> Msg
    , switchBorrowSetting : Bool -> Msg
    , slide : Float -> Msg
    , inputDebtIn : String -> Msg
    , inputCollateralIn : String -> Msg
    , inputMax : Msg
    , approveBorrow : Value -> Msg
    , borrow : Value -> Msg
    , onMouseEnter : Tooltip -> Msg
    , onMouseLeave : Msg
    }


update :
    { model
        | key : Key
        , slippage : Slippage
        , tokens : Tokens
        , pools : Pools
        , user : Maybe { user | balances : Remote Balances }
        , page : Page
    }
    -> Msg
    -> Modal
    -> ( Modal, Cmd Msg )
update { key, slippage, tokens, pools, user, page } msg (Modal modal) =
    case msg of
        InputAssetOut string ->
            if
                (string |> Input.isFloat)
                    && (string |> Uint.isAmount (modal.pool.pair |> Pair.toAsset))
            then
                ( { modal
                    | assetOut = string
                    , duesOut =
                        if string |> Input.isZero then
                            modal.duesOut |> DuesOut.updateAssetOutZero

                        else
                            modal.duesOut |> DuesOut.updateAssetOut
                    , apr =
                        if
                            (string |> Input.isZero)
                                || (modal.duesOut |> DuesOut.hasZeroInput)
                        then
                            Success ""

                        else
                            Loading
                    , cf =
                        if
                            (string |> Input.isZero)
                                || (modal.duesOut |> DuesOut.hasZeroInput)
                        then
                            Success ""

                        else
                            Loading
                  }
                    |> Modal
                , (case modal.duesOut of
                    DuesOut.Default _ ->
                        Query.givenPercent modal.pool string Percent.init slippage

                    DuesOut.Slider { percent } ->
                        Query.givenPercent modal.pool string percent slippage

                    DuesOut.Debt { debt } ->
                        Query.givenDebt modal.pool string debt slippage

                    DuesOut.Collateral { collateral } ->
                        Query.givenCollateral modal.pool string collateral slippage
                  )
                    |> Maybe.map queryBorrow
                    |> Maybe.withDefault Cmd.none
                )

            else
                ( Modal modal, Cmd.none )

        SwitchBorrowSetting checked ->
            ( { modal
                | duesOut =
                    if modal.assetOut |> Input.isZero then
                        modal.duesOut |> DuesOut.switchBorrowSettingZero checked

                    else
                        modal.duesOut |> DuesOut.switchBorrowSetting checked
                , apr =
                    if modal.assetOut |> Input.isZero then
                        Success ""

                    else if checked || (modal |> DuesOut.isDefault) then
                        modal.apr

                    else
                        Loading
                , cf =
                    if modal.assetOut |> Input.isZero then
                        Success ""

                    else if checked || (modal |> DuesOut.isDefault) then
                        modal.cf

                    else
                        Loading
              }
                |> Modal
            , if checked then
                Cmd.none

              else
                Query.givenPercent modal.pool modal.assetOut Percent.init slippage
                    |> Maybe.map queryBorrow
                    |> Maybe.withDefault Cmd.none
            )

        Slide float ->
            ( { modal
                | duesOut =
                    if modal.assetOut |> Input.isZero then
                        DuesOut.slideZero float

                    else
                        DuesOut.slide float
                , apr =
                    if modal.assetOut |> Input.isZero then
                        Success ""

                    else
                        Loading
                , cf =
                    if modal.assetOut |> Input.isZero then
                        Success ""

                    else
                        Loading
              }
                |> Modal
            , Query.givenPercent modal.pool modal.assetOut (float |> Percent.fromFloat) slippage
                |> Maybe.map queryBorrow
                |> Maybe.withDefault Cmd.none
            )

        InputDebtIn string ->
            if
                (string |> Input.isFloat)
                    && (string |> Uint.isAmount (modal.pool.pair |> Pair.toAsset))
            then
                ( { modal
                    | duesOut =
                        if modal.assetOut |> Input.isZero then
                            modal.duesOut |> DuesOut.updateDebtInZero string

                        else
                            modal.duesOut |> DuesOut.updateDebtIn string
                    , apr =
                        if
                            (modal.assetOut |> Input.isZero)
                                || (string |> Input.isZero)
                        then
                            Success ""

                        else
                            Loading
                    , cf =
                        if
                            (modal.assetOut |> Input.isZero)
                                || (string |> Input.isZero)
                        then
                            Success ""

                        else
                            Loading
                  }
                    |> Modal
                , Query.givenDebt modal.pool modal.assetOut string slippage
                    |> Maybe.map queryBorrow
                    |> Maybe.withDefault Cmd.none
                )

            else
                ( Modal modal, Cmd.none )

        InputCollateralIn string ->
            if
                (string |> Input.isFloat)
                    && (string |> Uint.isAmount (modal.pool.pair |> Pair.toCollateral))
            then
                ( { modal
                    | duesOut =
                        if modal.assetOut |> Input.isZero then
                            modal.duesOut |> DuesOut.updateCollateralInZero string

                        else
                            modal.duesOut |> DuesOut.updateCollateralIn string
                    , apr =
                        if (modal.assetOut |> Input.isZero) || (string |> Input.isZero) then
                            Success ""

                        else
                            Loading
                    , cf =
                        if (modal.assetOut |> Input.isZero) || (string |> Input.isZero) then
                            Success ""

                        else
                            Loading
                  }
                    |> Modal
                , Query.givenCollateral modal.pool modal.assetOut string slippage
                    |> Maybe.map queryBorrow
                    |> Maybe.withDefault Cmd.none
                )

            else
                ( Modal modal, Cmd.none )

        InputMax ->
            user
                |> Maybe.map
                    (\{ balances } ->
                        case balances of
                            Loading ->
                                ( Modal modal, Cmd.none )

                            Failure ->
                                ( Modal modal, Cmd.none )

                            Success successBalances ->
                                successBalances
                                    |> Balances.get (modal.pool.pair |> Pair.toCollateral)
                                    |> (\string ->
                                            ( { modal
                                                | duesOut =
                                                    if modal.assetOut |> Input.isZero then
                                                        modal.duesOut |> DuesOut.updateCollateralInZero string

                                                    else
                                                        modal.duesOut |> DuesOut.updateCollateralIn string
                                                , apr =
                                                    if (modal.assetOut |> Input.isZero) || (string |> Input.isZero) then
                                                        Success ""

                                                    else
                                                        Loading
                                                , cf =
                                                    if (modal.assetOut |> Input.isZero) || (string |> Input.isZero) then
                                                        Success ""

                                                    else
                                                        Loading
                                              }
                                                |> Modal
                                            , Query.givenCollateral modal.pool modal.assetOut string slippage
                                                |> Maybe.map queryBorrow
                                                |> Maybe.withDefault Cmd.none
                                            )
                                       )
                    )
                |> Maybe.withDefault ( Modal modal, Cmd.none )

        ApproveBorrow value ->
            ( Modal modal, approveBorrow value )

        Borrow value ->
            ( Modal modal
            , Cmd.batch
                [ borrow value
                , page
                    |> Page.toUrl
                    |> Navigation.pushUrl key
                ]
            )

        ReceiveTime posix ->
            ( Modal modal
            , if modal.pool.maturity |> Maturity.isActive posix then
                (case modal.duesOut of
                    DuesOut.Default _ ->
                        Query.givenPercent modal.pool modal.assetOut Percent.init slippage

                    DuesOut.Slider { percent } ->
                        Query.givenPercent modal.pool modal.assetOut percent slippage

                    DuesOut.Debt { debt } ->
                        Query.givenDebt modal.pool modal.assetOut debt slippage

                    DuesOut.Collateral { collateral } ->
                        Query.givenCollateral modal.pool modal.assetOut collateral slippage
                )
                    |> Maybe.map queryBorrowPerSecond
                    |> Maybe.withDefault Cmd.none

              else
                page
                    |> Page.toUrl
                    |> Navigation.pushUrl key
            )

        SdkBorrowMsg value ->
            ( value
                |> Decode.decodeValue (Query.decoder pools tokens)
                |> (\result ->
                        case result of
                            Ok query ->
                                case query of
                                    Query.GivenPercent { pool, assetOut, percent, dues } ->
                                        case modal.duesOut of
                                            DuesOut.Default _ ->
                                                if
                                                    (modal.pool == pool)
                                                        && (modal.assetOut
                                                                |> Uint.fromAmount (modal.pool.pair |> Pair.toAsset)
                                                                |> Maybe.map ((==) assetOut)
                                                                |> Maybe.withDefault False
                                                           )
                                                        && (Percent.init == percent)
                                                then
                                                    { modal
                                                        | duesOut =
                                                            modal.duesOut |> Query.updateDefaultQuery modal dues
                                                    }
                                                        |> Modal

                                                else
                                                    Modal modal

                                            DuesOut.Slider sliderInput ->
                                                if
                                                    (modal.pool == pool)
                                                        && (modal.assetOut
                                                                |> Uint.fromAmount (modal.pool.pair |> Pair.toAsset)
                                                                |> Maybe.map ((==) assetOut)
                                                                |> Maybe.withDefault False
                                                           )
                                                        && (sliderInput.percent == percent)
                                                then
                                                    { modal
                                                        | duesOut =
                                                            modal.duesOut |> Query.updateSliderQuery modal dues
                                                    }
                                                        |> Modal

                                                else
                                                    Modal modal

                                            _ ->
                                                Modal modal

                                    Query.GivenDebt { pool, assetOut, debt, dues } ->
                                        case modal.duesOut of
                                            DuesOut.Debt debtInput ->
                                                if
                                                    (modal.pool == pool)
                                                        && (modal.assetOut
                                                                |> Uint.fromAmount (modal.pool.pair |> Pair.toAsset)
                                                                |> Maybe.map ((==) assetOut)
                                                                |> Maybe.withDefault False
                                                           )
                                                        && (debtInput.debt
                                                                |> Uint.fromAmount (modal.pool.pair |> Pair.toAsset)
                                                                |> Maybe.map ((==) debt)
                                                                |> Maybe.withDefault False
                                                           )
                                                then
                                                    { modal
                                                        | duesOut =
                                                            modal.duesOut |> Query.updateDebtQuery modal dues
                                                    }
                                                        |> Modal

                                                else
                                                    Modal modal

                                            _ ->
                                                Modal modal

                                    Query.GivenCollateral { pool, assetOut, collateral, dues } ->
                                        case modal.duesOut of
                                            DuesOut.Collateral collateralInput ->
                                                if
                                                    (modal.pool == pool)
                                                        && (modal.assetOut
                                                                |> Uint.fromAmount (modal.pool.pair |> Pair.toAsset)
                                                                |> Maybe.map ((==) assetOut)
                                                                |> Maybe.withDefault False
                                                           )
                                                        && (collateralInput.collateral
                                                                |> Uint.fromAmount (modal.pool.pair |> Pair.toCollateral)
                                                                |> Maybe.map ((==) collateral)
                                                                |> Maybe.withDefault False
                                                           )
                                                then
                                                    { modal
                                                        | duesOut =
                                                            modal.duesOut |> Query.updateCollateralQuery modal dues
                                                    }
                                                        |> Modal

                                                else
                                                    Modal modal

                                            _ ->
                                                Modal modal

                            Err _ ->
                                Modal modal
                   )
            , Cmd.none
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
    { inputAssetOut = InputAssetOut
    , switchBorrowSetting = SwitchBorrowSetting
    , slide = Slide
    , inputDebtIn = InputDebtIn
    , inputCollateralIn = InputCollateralIn
    , inputMax = InputMax
    , approveBorrow = ApproveBorrow
    , borrow = Borrow
    , onMouseEnter = OnMouseEnter
    , onMouseLeave = OnMouseLeave
    }


port queryBorrow : Value -> Cmd msg


port queryBorrowPerSecond : Value -> Cmd msg


port approveBorrow : Value -> Cmd msg


port borrow : Value -> Cmd msg


port sdkBorrowMsg : (Value -> msg) -> Sub msg


subscriptions : { model | time : Posix } -> Modal -> Sub Msg
subscriptions { time } (Modal { pool }) =
    if pool.maturity |> Maturity.isActive time then
        Sub.batch
            [ Time.every 1000 ReceiveTime
            , sdkBorrowMsg SdkBorrowMsg
            ]

    else
        Sub.none


view :
    { model
        | device : Device
        , time : Posix
        , zoneInfo : Maybe ZoneInfo
        , backdrop : Backdrop
        , deadline : Deadline
        , images : Images
        , tokenImages : TokenImages
        , user :
            Maybe
                { user
                    | address : Address
                    , balances : Remote Balances
                    , allowances : Remote Allowances
                }
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
        , content model modal
        , Transaction.view msgs model modal
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
        (text "Borrow")


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
            , assetOut : String
            , duesOut : DuesOut
            , apr : Remote String
            , cf : Remote String
            , tooltip : Maybe Tooltip
        }
    -> Element Msg
content ({ images } as model) modal =
    column
        [ width fill
        , height shrink
        , spacing 12
        ]
        [ AssetOut.view msgs model modal
        , Image.arrowDown images
            [ width <| px 14
            , centerX
            ]
        , DuesOut.view msgs model modal
        , Warning.view model modal
        ]
