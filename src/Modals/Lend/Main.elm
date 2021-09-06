port module Modals.Lend.Main exposing
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
import Modals.Lend.AssetIn as AssetIn
import Modals.Lend.ClaimsOut as ClaimsOut exposing (ClaimsOut)
import Modals.Lend.Query as Query
import Modals.Lend.Tooltip exposing (Tooltip)
import Modals.Lend.Transaction as Transaction
import Modals.Lend.Warning as Warning
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
        , assetIn : String
        , claimsOut : ClaimsOut
        , apr : Remote String
        , cf : Remote String
        , tooltip : Maybe Tooltip
        }


init : Pool -> Modal
init pool =
    { pool = pool
    , assetIn = ""
    , claimsOut = ClaimsOut.init
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
    = InputAssetIn String
    | InputMax
    | SwitchLendSetting Bool
    | Slide Float
    | InputBondOut String
    | InputInsuranceOut String
    | ApproveLend Value
    | Lend Value
    | ReceiveTime Posix
    | SdkLendMsg Value
    | OnMouseEnter Tooltip
    | OnMouseLeave


type alias Msgs =
    { inputAssetIn : String -> Msg
    , inputMax : Msg
    , switchLendSetting : Bool -> Msg
    , slide : Float -> Msg
    , inputBondOut : String -> Msg
    , inputInsuranceOut : String -> Msg
    , approveLend : Value -> Msg
    , lend : Value -> Msg
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
                , (case modal.claimsOut of
                    ClaimsOut.Default _ ->
                        Query.givenPercent modal.pool string Percent.init slippage

                    ClaimsOut.Slider { percent } ->
                        Query.givenPercent modal.pool string percent slippage

                    ClaimsOut.Bond { bond } ->
                        Query.givenBond modal.pool string bond slippage

                    ClaimsOut.Insurance { insurance } ->
                        Query.givenInsurance modal.pool string insurance slippage
                  )
                    |> Maybe.map queryLend
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
                                    |> Balances.get (modal.pool.pair |> Pair.toAsset)
                                    |> (\string ->
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
                                            , (case modal.claimsOut of
                                                ClaimsOut.Default _ ->
                                                    Query.givenPercent modal.pool string Percent.init slippage

                                                ClaimsOut.Slider { percent } ->
                                                    Query.givenPercent modal.pool string percent slippage

                                                ClaimsOut.Bond { bond } ->
                                                    Query.givenBond modal.pool string bond slippage

                                                ClaimsOut.Insurance { insurance } ->
                                                    Query.givenInsurance modal.pool string insurance slippage
                                              )
                                                |> Maybe.map queryLend
                                                |> Maybe.withDefault Cmd.none
                                            )
                                       )
                    )
                |> Maybe.withDefault ( Modal modal, Cmd.none )

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
            , if checked then
                Cmd.none

              else
                Query.givenPercent modal.pool modal.assetIn Percent.init slippage
                    |> Maybe.map queryLend
                    |> Maybe.withDefault Cmd.none
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
            , Query.givenPercent modal.pool modal.assetIn (float |> Percent.fromFloat) slippage
                |> Maybe.map queryLend
                |> Maybe.withDefault Cmd.none
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
                , Query.givenBond modal.pool modal.assetIn string slippage
                    |> Maybe.map queryLend
                    |> Maybe.withDefault Cmd.none
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
                , Query.givenInsurance modal.pool modal.assetIn string slippage
                    |> Maybe.map queryLend
                    |> Maybe.withDefault Cmd.none
                )

            else
                ( Modal modal, Cmd.none )

        ApproveLend value ->
            ( Modal modal, approveLend value )

        Lend value ->
            ( Modal modal
            , Cmd.batch
                [ lend value
                , page
                    |> Page.toUrl
                    |> Navigation.pushUrl key
                ]
            )

        ReceiveTime posix ->
            ( Modal modal
            , if modal.pool.maturity |> Maturity.isActive posix then
                (case modal.claimsOut of
                    ClaimsOut.Default _ ->
                        Query.givenPercent modal.pool modal.assetIn Percent.init slippage

                    ClaimsOut.Slider { percent } ->
                        Query.givenPercent modal.pool modal.assetIn percent slippage

                    ClaimsOut.Bond { bond } ->
                        Query.givenBond modal.pool modal.assetIn bond slippage

                    ClaimsOut.Insurance { insurance } ->
                        Query.givenInsurance modal.pool modal.assetIn insurance slippage
                )
                    |> Maybe.map queryLendPerSecond
                    |> Maybe.withDefault Cmd.none

              else
                page
                    |> Page.toUrl
                    |> Navigation.pushUrl key
            )

        SdkLendMsg value ->
            ( value
                |> Decode.decodeValue (Query.decoder pools tokens)
                |> (\result ->
                        case result of
                            Ok query ->
                                case query of
                                    Query.GivenPercent { pool, assetIn, percent, claims } ->
                                        case modal.claimsOut of
                                            ClaimsOut.Default _ ->
                                                if
                                                    (modal.pool == pool)
                                                        && (modal.assetIn
                                                                |> Uint.fromAmount (modal.pool.pair |> Pair.toAsset)
                                                                |> Maybe.map ((==) assetIn)
                                                                |> Maybe.withDefault False
                                                           )
                                                        && (Percent.init == percent)
                                                then
                                                    { modal
                                                        | claimsOut =
                                                            modal.claimsOut |> Query.updateDefaultQuery modal claims
                                                    }
                                                        |> Modal

                                                else
                                                    Modal modal

                                            ClaimsOut.Slider sliderInput ->
                                                if
                                                    (modal.pool == pool)
                                                        && (modal.assetIn
                                                                |> Uint.fromAmount (modal.pool.pair |> Pair.toAsset)
                                                                |> Maybe.map ((==) assetIn)
                                                                |> Maybe.withDefault False
                                                           )
                                                        && (sliderInput.percent == percent)
                                                then
                                                    { modal
                                                        | claimsOut =
                                                            modal.claimsOut |> Query.updateSliderQuery modal claims
                                                    }
                                                        |> Modal

                                                else
                                                    Modal modal

                                            _ ->
                                                Modal modal

                                    Query.GivenBond { pool, assetIn, bond, claims } ->
                                        case modal.claimsOut of
                                            ClaimsOut.Bond bondInput ->
                                                if
                                                    (modal.pool == pool)
                                                        && (modal.assetIn
                                                                |> Uint.fromAmount (modal.pool.pair |> Pair.toAsset)
                                                                |> Maybe.map ((==) assetIn)
                                                                |> Maybe.withDefault False
                                                           )
                                                        && (bondInput.bond
                                                                |> Uint.fromAmount (modal.pool.pair |> Pair.toAsset)
                                                                |> Maybe.map ((==) bond)
                                                                |> Maybe.withDefault False
                                                           )
                                                then
                                                    { modal
                                                        | claimsOut =
                                                            modal.claimsOut |> Query.updateBondQuery modal claims
                                                    }
                                                        |> Modal

                                                else
                                                    Modal modal

                                            _ ->
                                                Modal modal

                                    Query.GivenInsurance { pool, assetIn, insurance, claims } ->
                                        case modal.claimsOut of
                                            ClaimsOut.Insurance insuranceInput ->
                                                if
                                                    (modal.pool == pool)
                                                        && (modal.assetIn
                                                                |> Uint.fromAmount (modal.pool.pair |> Pair.toAsset)
                                                                |> Maybe.map ((==) assetIn)
                                                                |> Maybe.withDefault False
                                                           )
                                                        && (insuranceInput.insurance
                                                                |> Uint.fromAmount (modal.pool.pair |> Pair.toCollateral)
                                                                |> Maybe.map ((==) insurance)
                                                                |> Maybe.withDefault False
                                                           )
                                                then
                                                    { modal
                                                        | claimsOut =
                                                            modal.claimsOut |> Query.updateInsuranceQuery modal claims
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
    { inputAssetIn = InputAssetIn
    , inputMax = InputMax
    , switchLendSetting = SwitchLendSetting
    , slide = Slide
    , inputBondOut = InputBondOut
    , inputInsuranceOut = InputInsuranceOut
    , approveLend = ApproveLend
    , lend = Lend
    , onMouseEnter = OnMouseEnter
    , onMouseLeave = OnMouseLeave
    }


port queryLend : Value -> Cmd msg


port queryLendPerSecond : Value -> Cmd msg


port approveLend : Value -> Cmd msg


port lend : Value -> Cmd msg


port sdkLendMsg : (Value -> msg) -> Sub msg


subscriptions : { model | time : Posix } -> Modal -> Sub Msg
subscriptions { time } (Modal { pool }) =
    if pool.maturity |> Maturity.isActive time then
        Sub.batch
            [ Time.every 1000 ReceiveTime
            , sdkLendMsg SdkLendMsg
            ]

    else
        Sub.none


view :
    { model
        | device : Device
        , time : Posix
        , zoneInfo : Maybe ZoneInfo
        , backdrop : Backdrop
        , slippage : Slippage
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
            , tooltip : Maybe Tooltip
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
