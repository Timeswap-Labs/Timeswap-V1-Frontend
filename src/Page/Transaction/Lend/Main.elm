port module Page.Transaction.Lend.Main exposing
    ( Effect(..)
    , Msg
    , Transaction
    , disabled
    , disabledDoesNotExist
    , doesNotExist
    , empty
    , init
    , refresh
    , subscriptions
    , update
    , view
    )

import Blockchain.Main as Blockchain exposing (Blockchain)
import Blockchain.User.Main as User exposing (User)
import Data.Backdrop exposing (Backdrop)
import Data.Chains exposing (Chains)
import Data.Deadline exposing (Deadline)
import Data.Images exposing (Images)
import Data.Pair as Pair exposing (Pair)
import Data.Percent as Percent exposing (Percent)
import Data.Pool exposing (Pool)
import Data.Remote as Remote exposing (Remote(..))
import Data.Slippage exposing (Slippage)
import Data.Token as Token exposing (Token)
import Data.Uint as Uint exposing (Uint)
import Element
    exposing
        ( Element
        , alignLeft
        , alignRight
        , alpha
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
import Element.Region as Region
import Json.Decode as Decode
import Json.Encode exposing (Value)
import Page.Approve as Approve
import Page.Transaction.Button as Button
import Page.Transaction.Info as Info
import Page.Transaction.Lend.Answer as Answer
    exposing
        ( ClaimsGivenBond
        , ClaimsGivenInsurance
        , ClaimsGivenPercent
        )
import Page.Transaction.Lend.Error exposing (Error)
import Page.Transaction.Lend.Query as Query
import Page.Transaction.Lend.Tooltip as Tooltip exposing (Tooltip)
import Page.Transaction.Lend.Write as Write
import Page.Transaction.PoolInfo exposing (PoolInfo)
import Page.Transaction.Slider as Slider
import Page.Transaction.Switch as Switch exposing (Mode)
import Page.Transaction.Textbox as Textbox
import Time exposing (Posix)
import Utility.Color as Color
import Utility.Direction as Direction
import Utility.FontStyle as FontStyle
import Utility.Image as Image
import Utility.Input as Input
import Utility.Truncate as Truncate


type Transaction
    = Transaction
        { assetIn : String
        , claimsOut : ClaimsOut
        , tooltip : Maybe Tooltip
        }


type ClaimsOut
    = Default (Remote Error ClaimsGivenPercent)
    | Slider SliderInput
    | Bond BondInput
    | Insurance InsuranceInput


type alias SliderInput =
    { percent : Percent
    , claims : Remote Error ClaimsGivenPercent
    }


type alias BondInput =
    { percent : Percent
    , bond : String
    , claims : Remote Error ClaimsGivenBond
    }


type alias InsuranceInput =
    { percent : Percent
    , claims : Remote Error ClaimsGivenInsurance
    , insurance : String
    }


type Msg
    = InputAssetIn String
    | InputMax
    | SwitchMode Mode
    | ClickSlider
    | Slide Float
    | ClickBondOut
    | InputBondOut String
    | ClickInsuranceOut
    | InputInsuranceOut String
    | QueryAgain Posix
    | ClickConnect
    | ClickApprove
    | ClickLend
    | ReceiveAnswer Value
    | OnMouseEnter Tooltip
    | OnMouseLeave


type Effect
    = OpenConnect
    | OpenConfirm


init : Transaction
init =
    { assetIn = ""
    , claimsOut =
        Answer.initGivenPercent
            |> Success
            |> Default
    , tooltip = Nothing
    }
        |> Transaction


refresh : Transaction -> Transaction
refresh (Transaction transaction) =
    { transaction
        | claimsOut =
            if transaction.assetIn |> Input.isZero then
                transaction.claimsOut

            else
                case transaction.claimsOut of
                    Default _ ->
                        Default Loading

                    Slider slider ->
                        { slider | claims = Loading }
                            |> Slider

                    Bond bond ->
                        { bond | claims = Loading }
                            |> Bond

                    Insurance insurance ->
                        { insurance | claims = Loading }
                            |> Insurance
        , tooltip = Nothing
    }
        |> Transaction


update :
    { model
        | time : Posix
        , chains : Chains
        , slippage : Slippage
        , deadline : Deadline
    }
    -> Blockchain
    -> Pool
    -> PoolInfo
    -> Msg
    -> Transaction
    -> ( Transaction, Cmd Msg, Maybe Effect )
update model blockchain pool poolInfo msg (Transaction transaction) =
    case msg of
        InputAssetIn assetIn ->
            if assetIn |> Uint.isAmount (pool.pair |> Pair.toAsset) then
                { transaction
                    | assetIn = assetIn
                    , claimsOut =
                        if assetIn |> Input.isZero then
                            transaction.claimsOut |> updateGivenAssetInZero

                        else
                            transaction.claimsOut |> updateGivenAssetIn
                }
                    |> query model blockchain pool poolInfo

            else
                ( transaction |> Transaction
                , Cmd.none
                , Nothing
                )

        InputMax ->
            blockchain
                |> Blockchain.toUser
                |> Maybe.andThen
                    (\user ->
                        user
                            |> User.getBalance
                                (pool.pair |> Pair.toAsset)
                            |> Maybe.map
                                (Uint.toAmount
                                    (pool.pair |> Pair.toAsset)
                                )
                    )
                |> Maybe.map
                    (\assetIn ->
                        { transaction
                            | assetIn = assetIn
                            , claimsOut =
                                if assetIn |> Input.isZero then
                                    transaction.claimsOut |> updateGivenAssetInZero

                                else
                                    transaction.claimsOut |> updateGivenAssetIn
                        }
                            |> query model blockchain pool poolInfo
                    )
                |> Maybe.withDefault
                    ( transaction |> Transaction
                    , Cmd.none
                    , Nothing
                    )

        SwitchMode Switch.Pro ->
            ( { transaction
                | claimsOut =
                    if transaction.assetIn |> Input.isZero then
                        transaction.claimsOut |> switchToAdvanceZero

                    else
                        transaction.claimsOut |> switchToAdvance
              }
                |> Transaction
            , Cmd.none
            , Nothing
            )

        SwitchMode Switch.Recommended ->
            { transaction
                | claimsOut =
                    if transaction.assetIn |> Input.isZero then
                        switchToBasicZero

                    else
                        transaction.claimsOut |> switchToBasic
            }
                |> (\updated ->
                        case transaction.claimsOut of
                            Slider { percent } ->
                                if (percent |> Percent.toFloat) == 64 then
                                    ( updated |> Transaction
                                    , Cmd.none
                                    , Nothing
                                    )

                                else
                                    query model blockchain pool poolInfo updated

                            Default _ ->
                                ( updated |> Transaction
                                , Cmd.none
                                , Nothing
                                )

                            _ ->
                                query model blockchain pool poolInfo updated
                   )

        ClickSlider ->
            (case transaction.claimsOut of
                Bond { percent } ->
                    percent |> Just

                Insurance { percent } ->
                    percent |> Just

                _ ->
                    Nothing
            )
                |> Maybe.map
                    (\percent ->
                        { transaction
                            | claimsOut =
                                if transaction.assetIn |> Input.isZero then
                                    percent
                                        |> Percent.toFloat
                                        |> slideZero

                                else
                                    percent
                                        |> Percent.toFloat
                                        |> slide
                        }
                            |> query model blockchain pool poolInfo
                    )
                |> Maybe.withDefault
                    ( transaction |> Transaction
                    , Cmd.none
                    , Nothing
                    )

        Slide float ->
            case transaction.claimsOut of
                Default _ ->
                    ( transaction |> Transaction
                    , Cmd.none
                    , Nothing
                    )

                _ ->
                    { transaction
                        | claimsOut =
                            if transaction.assetIn |> Input.isZero then
                                float |> slideZero

                            else
                                float |> slide
                    }
                        |> query model blockchain pool poolInfo

        ClickBondOut ->
            (case transaction.claimsOut of
                Slider { claims } ->
                    case claims of
                        Success { bond } ->
                            bond
                                |> Uint.toAmount (pool.pair |> Pair.toAsset)
                                |> Just

                        _ ->
                            "" |> Just

                Insurance { claims } ->
                    case claims of
                        Success { bond } ->
                            bond
                                |> Uint.toAmount (pool.pair |> Pair.toAsset)
                                |> Just

                        _ ->
                            "" |> Just

                _ ->
                    Nothing
            )
                |> Maybe.map
                    (\bondOut ->
                        { transaction
                            | claimsOut =
                                if transaction.assetIn |> Input.isZero then
                                    transaction.claimsOut
                                        |> updateGivenBondOutZero bondOut

                                else
                                    transaction.claimsOut
                                        |> updateGivenBondOut bondOut
                        }
                            |> query model blockchain pool poolInfo
                    )
                |> Maybe.withDefault
                    ( transaction |> Transaction
                    , Cmd.none
                    , Nothing
                    )

        InputBondOut bondOut ->
            if bondOut |> Uint.isAmount (pool.pair |> Pair.toAsset) then
                { transaction
                    | claimsOut =
                        if transaction.assetIn |> Input.isZero then
                            transaction.claimsOut
                                |> updateGivenBondOutZero bondOut

                        else
                            transaction.claimsOut
                                |> updateGivenBondOut bondOut
                }
                    |> query model blockchain pool poolInfo

            else
                ( transaction |> Transaction
                , Cmd.none
                , Nothing
                )

        ClickInsuranceOut ->
            (case transaction.claimsOut of
                Slider { claims } ->
                    case claims of
                        Success { insurance } ->
                            insurance
                                |> Uint.toAmount (pool.pair |> Pair.toCollateral)
                                |> Just

                        _ ->
                            "" |> Just

                Bond { claims } ->
                    case claims of
                        Success { insurance } ->
                            insurance
                                |> Uint.toAmount (pool.pair |> Pair.toCollateral)
                                |> Just

                        _ ->
                            "" |> Just

                _ ->
                    Nothing
            )
                |> Maybe.map
                    (\insuranceOut ->
                        { transaction
                            | claimsOut =
                                if transaction.assetIn |> Input.isZero then
                                    transaction.claimsOut
                                        |> updateGivenInsuranceOutZero insuranceOut

                                else
                                    transaction.claimsOut
                                        |> updateGivenInsuranceOut insuranceOut
                        }
                            |> query model blockchain pool poolInfo
                    )
                |> Maybe.withDefault
                    ( transaction |> Transaction
                    , Cmd.none
                    , Nothing
                    )

        InputInsuranceOut insuranceOut ->
            if insuranceOut |> Uint.isAmount (pool.pair |> Pair.toCollateral) then
                { transaction
                    | claimsOut =
                        if transaction.assetIn |> Input.isZero then
                            transaction.claimsOut
                                |> updateGivenInsuranceOutZero insuranceOut

                        else
                            transaction.claimsOut
                                |> updateGivenInsuranceOut insuranceOut
                }
                    |> query model blockchain pool poolInfo

            else
                ( transaction |> Transaction
                , Cmd.none
                , Nothing
                )

        QueryAgain _ ->
            transaction
                |> queryPerSecond model blockchain pool poolInfo

        ClickConnect ->
            blockchain
                |> Blockchain.toUser
                |> Maybe.map
                    (\_ ->
                        ( transaction |> Transaction
                        , Cmd.none
                        , Nothing
                        )
                    )
                |> Maybe.withDefault
                    ( transaction |> Transaction
                    , Cmd.none
                    , OpenConnect |> Just
                    )

        ClickApprove ->
            case
                ( blockchain |> Blockchain.toUser
                , transaction.assetIn
                    |> Uint.fromAmount
                        (pool.pair |> Pair.toAsset)
                , pool.pair
                    |> Pair.toAsset
                    |> Token.toERC20
                )
            of
                ( Just user, Just assetIn, Just erc20 ) ->
                    if
                        (user
                            |> User.hasEnoughBalance
                                (pool.pair |> Pair.toAsset)
                                assetIn
                        )
                            && (user
                                    |> User.hasEnoughAllowance
                                        erc20
                                        assetIn
                                    |> not
                               )
                    then
                        ( transaction |> Transaction
                        , erc20
                            |> Approve.encode blockchain user
                            |> approve
                        , OpenConfirm |> Just
                        )

                    else
                        ( transaction |> Transaction
                        , Cmd.none
                        , Nothing
                        )

                _ ->
                    ( transaction |> Transaction
                    , Cmd.none
                    , Nothing
                    )

        ClickLend ->
            case
                ( blockchain |> Blockchain.toUser
                , transaction.assetIn
                    |> Uint.fromAmount
                        (pool.pair |> Pair.toAsset)
                )
            of
                ( Just user, Just assetIn ) ->
                    (case
                        ( transaction.claimsOut
                        , user
                            |> User.hasEnoughBalance
                                (pool.pair |> Pair.toAsset)
                                assetIn
                        , pool.pair
                            |> Pair.toAsset
                            |> Token.toERC20
                            |> Maybe.map
                                (\erc20 ->
                                    user
                                        |> User.hasEnoughAllowance
                                            erc20
                                            assetIn
                                )
                            |> Maybe.withDefault True
                        )
                     of
                        ( Default (Success answer), True, True ) ->
                            ( { transaction
                                | assetIn = ""
                                , claimsOut =
                                    Answer.initGivenPercent
                                        |> Success
                                        |> Default
                              }
                                |> Transaction
                            , { pool = pool
                              , assetIn = assetIn
                              , percent = Percent.init
                              , minBond = answer.minBond
                              , minInsurance = answer.minInsurance
                              }
                                |> Write.GivenPercent
                                |> Write.encode model blockchain user
                                |> lend
                            , OpenConfirm |> Just
                            )
                                |> Just

                        ( Slider { percent, claims }, True, True ) ->
                            claims
                                |> Remote.map
                                    (\answer ->
                                        ( { transaction
                                            | assetIn = ""
                                            , claimsOut =
                                                { percent = Percent.init
                                                , claims =
                                                    Answer.initGivenPercent
                                                        |> Success
                                                }
                                                    |> Slider
                                          }
                                            |> Transaction
                                        , { pool = pool
                                          , assetIn = assetIn
                                          , percent = percent
                                          , minBond = answer.minBond
                                          , minInsurance = answer.minInsurance
                                          }
                                            |> Write.GivenPercent
                                            |> Write.encode model blockchain user
                                            |> lend
                                        , OpenConfirm |> Just
                                        )
                                            |> Just
                                    )
                                |> Remote.withDefault Nothing

                        ( Bond { bond, claims }, True, True ) ->
                            case
                                ( claims
                                , bond
                                    |> Uint.fromAmount (pool.pair |> Pair.toAsset)
                                )
                            of
                                ( Success answer, Just bondOut ) ->
                                    ( { transaction
                                        | assetIn = ""
                                        , claimsOut =
                                            { percent = Percent.init
                                            , bond = ""
                                            , claims =
                                                Answer.initGivenBond
                                                    |> Success
                                            }
                                                |> Bond
                                      }
                                        |> Transaction
                                    , { pool = pool
                                      , assetIn = assetIn
                                      , bondOut = bondOut
                                      , minInsurance = answer.minInsurance
                                      }
                                        |> Write.GivenBond
                                        |> Write.encode model blockchain user
                                        |> lend
                                    , OpenConfirm |> Just
                                    )
                                        |> Just

                                _ ->
                                    Nothing

                        ( Insurance { insurance, claims }, True, True ) ->
                            case
                                ( claims
                                , insurance
                                    |> Uint.fromAmount (pool.pair |> Pair.toCollateral)
                                )
                            of
                                ( Success answer, Just insuranceOut ) ->
                                    ( { transaction
                                        | assetIn = ""
                                        , claimsOut =
                                            { percent = Percent.init
                                            , insurance = ""
                                            , claims =
                                                Answer.initGivenInsurance
                                                    |> Success
                                            }
                                                |> Insurance
                                      }
                                        |> Transaction
                                    , { pool = pool
                                      , assetIn = assetIn
                                      , insuranceOut = insuranceOut
                                      , minBond = answer.minBond
                                      }
                                        |> Write.GivenInsurance
                                        |> Write.encode model blockchain user
                                        |> lend
                                    , OpenConfirm |> Just
                                    )
                                        |> Just

                                _ ->
                                    Nothing

                        _ ->
                            Nothing
                    )
                        |> Maybe.withDefault
                            ( transaction |> Transaction
                            , Cmd.none
                            , Nothing
                            )

                _ ->
                    ( transaction |> Transaction
                    , Cmd.none
                    , Nothing
                    )

        ReceiveAnswer value ->
            ( case
                ( value
                    |> Decode.decodeValue (Answer.decoder model)
                , transaction.claimsOut
                )
              of
                ( Ok (Answer.GivenPercent answer), Default _ ) ->
                    if
                        (answer.chainId == (blockchain |> Blockchain.toChain))
                            && (answer.pool == pool)
                            && (answer.poolInfo == poolInfo)
                            && (Just answer.assetIn
                                    == (transaction.assetIn
                                            |> Uint.fromAmount
                                                (pool.pair |> Pair.toAsset)
                                       )
                               )
                            && (answer.percent == Percent.init)
                            && (answer.slippage == model.slippage)
                    then
                        { transaction
                            | claimsOut =
                                answer.result
                                    |> Answer.toClaimsGivenPercent
                                    |> Default
                        }
                            |> Transaction

                    else
                        transaction |> Transaction

                ( Ok (Answer.GivenPercent answer), Slider slider ) ->
                    if
                        (answer.chainId == (blockchain |> Blockchain.toChain))
                            && (answer.pool == pool)
                            && (answer.poolInfo == poolInfo)
                            && (Just answer.assetIn
                                    == (transaction.assetIn
                                            |> Uint.fromAmount
                                                (pool.pair |> Pair.toAsset)
                                       )
                               )
                            && (answer.percent == slider.percent)
                            && (answer.slippage == model.slippage)
                    then
                        { transaction
                            | claimsOut =
                                { slider
                                    | claims =
                                        answer.result
                                            |> Answer.toClaimsGivenPercent
                                }
                                    |> Slider
                        }
                            |> Transaction

                    else
                        transaction |> Transaction

                ( Ok (Answer.GivenBond answer), Bond bond ) ->
                    if
                        (answer.chainId == (blockchain |> Blockchain.toChain))
                            && (answer.pool == pool)
                            && (answer.poolInfo == poolInfo)
                            && (Just answer.assetIn
                                    == (transaction.assetIn
                                            |> Uint.fromAmount
                                                (pool.pair |> Pair.toAsset)
                                       )
                               )
                            && (Just answer.bondOut
                                    == (bond.bond
                                            |> Uint.fromAmount
                                                (pool.pair |> Pair.toAsset)
                                       )
                               )
                            && (answer.slippage == model.slippage)
                    then
                        { transaction
                            | claimsOut =
                                { bond
                                    | percent =
                                        answer.result
                                            |> Result.map .percent
                                            |> Result.withDefault Percent.init
                                    , claims =
                                        answer.result
                                            |> Answer.toClaimsGivenBond
                                }
                                    |> Bond
                        }
                            |> Transaction

                    else
                        transaction |> Transaction

                ( Ok (Answer.GivenInsurance answer), Insurance insurance ) ->
                    if
                        (answer.chainId == (blockchain |> Blockchain.toChain))
                            && (answer.pool == pool)
                            && (answer.poolInfo == poolInfo)
                            && (Just answer.assetIn
                                    == (transaction.assetIn
                                            |> Uint.fromAmount
                                                (pool.pair |> Pair.toAsset)
                                       )
                               )
                            && (Just answer.insuranceOut
                                    == (insurance.insurance
                                            |> Uint.fromAmount
                                                (pool.pair |> Pair.toCollateral)
                                       )
                               )
                            && (answer.slippage == model.slippage)
                    then
                        { transaction
                            | claimsOut =
                                { insurance
                                    | percent =
                                        answer.result
                                            |> Result.map .percent
                                            |> Result.withDefault Percent.init
                                    , claims =
                                        answer.result
                                            |> Answer.toClaimsGivenInsurance
                                }
                                    |> Insurance
                        }
                            |> Transaction

                    else
                        transaction |> Transaction

                _ ->
                    transaction |> Transaction
            , Cmd.none
            , Nothing
            )

        OnMouseEnter tooltip ->
            ( { transaction | tooltip = Just tooltip }
                |> Transaction
            , Cmd.none
            , Nothing
            )

        OnMouseLeave ->
            ( { transaction | tooltip = Nothing }
                |> Transaction
            , Cmd.none
            , Nothing
            )


updateGivenAssetInZero : ClaimsOut -> ClaimsOut
updateGivenAssetInZero claimsOut =
    case claimsOut of
        Default _ ->
            Answer.initGivenPercent
                |> Success
                |> Default

        Slider slider ->
            { slider
                | claims =
                    Answer.initGivenPercent |> Success
            }
                |> Slider

        Bond bond ->
            { bond
                | claims =
                    Answer.initGivenBond |> Success
            }
                |> Bond

        Insurance insurance ->
            { insurance
                | claims =
                    Answer.initGivenInsurance |> Success
            }
                |> Insurance


updateGivenAssetIn : ClaimsOut -> ClaimsOut
updateGivenAssetIn claimsOut =
    case claimsOut of
        Default _ ->
            Default Loading

        Slider slider ->
            { slider | claims = Loading }
                |> Slider

        Bond bond ->
            { bond
                | claims =
                    if bond.bond |> Input.isZero then
                        Answer.initGivenBond |> Success

                    else
                        Loading
            }
                |> Bond

        Insurance insurance ->
            { insurance
                | claims =
                    if insurance.insurance |> Input.isZero then
                        Answer.initGivenInsurance |> Success

                    else
                        Loading
            }
                |> Insurance


switchToAdvanceZero : ClaimsOut -> ClaimsOut
switchToAdvanceZero claimsOut =
    case claimsOut of
        Default _ ->
            { percent = Percent.init
            , claims =
                Answer.initGivenPercent |> Success
            }
                |> Slider

        _ ->
            claimsOut


switchToAdvance : ClaimsOut -> ClaimsOut
switchToAdvance claimsOut =
    case claimsOut of
        Default claims ->
            { percent = Percent.init
            , claims = claims
            }
                |> Slider

        _ ->
            claimsOut


switchToBasicZero : ClaimsOut
switchToBasicZero =
    Answer.initGivenPercent
        |> Success
        |> Default


switchToBasic : ClaimsOut -> ClaimsOut
switchToBasic claimsOut =
    case claimsOut of
        Slider { percent, claims } ->
            if (percent |> Percent.toFloat) == 64 then
                Default claims

            else
                Default Loading

        Default _ ->
            claimsOut

        _ ->
            Default Loading


slideZero : Float -> ClaimsOut
slideZero float =
    { percent = float |> Percent.fromFloat
    , claims = Answer.initGivenPercent |> Success
    }
        |> Slider


slide : Float -> ClaimsOut
slide float =
    { percent = float |> Percent.fromFloat
    , claims = Loading
    }
        |> Slider


updateGivenBondOutZero : String -> ClaimsOut -> ClaimsOut
updateGivenBondOutZero input claimsOut =
    (case claimsOut of
        Slider { percent } ->
            percent |> Just

        Bond { percent } ->
            percent |> Just

        Insurance { percent } ->
            percent |> Just

        _ ->
            Nothing
    )
        |> Maybe.map
            (\percent ->
                { percent = percent
                , bond = input
                , claims = Answer.initGivenBond |> Success
                }
                    |> Bond
            )
        |> Maybe.withDefault claimsOut


updateGivenBondOut : String -> ClaimsOut -> ClaimsOut
updateGivenBondOut input claimsOut =
    (case claimsOut of
        Slider { percent } ->
            percent |> Just

        Bond { percent } ->
            percent |> Just

        Insurance { percent } ->
            percent |> Just

        _ ->
            Nothing
    )
        |> Maybe.map
            (\percent ->
                { percent = percent
                , bond = input
                , claims =
                    if input |> Input.isZero then
                        Answer.initGivenBond |> Success

                    else
                        Loading
                }
                    |> Bond
            )
        |> Maybe.withDefault claimsOut


updateGivenInsuranceOutZero : String -> ClaimsOut -> ClaimsOut
updateGivenInsuranceOutZero input claimsOut =
    (case claimsOut of
        Slider { percent } ->
            percent |> Just

        Bond { percent } ->
            percent |> Just

        Insurance { percent } ->
            percent |> Just

        _ ->
            Nothing
    )
        |> Maybe.map
            (\percent ->
                { percent = percent
                , insurance = input
                , claims = Answer.initGivenInsurance |> Success
                }
                    |> Insurance
            )
        |> Maybe.withDefault claimsOut


updateGivenInsuranceOut : String -> ClaimsOut -> ClaimsOut
updateGivenInsuranceOut input claimsOut =
    (case claimsOut of
        Slider { percent } ->
            percent |> Just

        Bond { percent } ->
            percent |> Just

        Insurance { percent } ->
            percent |> Just

        _ ->
            Nothing
    )
        |> Maybe.map
            (\percent ->
                { percent = percent
                , insurance = input
                , claims =
                    if input |> Input.isZero then
                        Answer.initGivenInsurance |> Success

                    else
                        Loading
                }
                    |> Insurance
            )
        |> Maybe.withDefault claimsOut


query :
    { model | slippage : Slippage }
    -> Blockchain
    -> Pool
    -> PoolInfo
    ->
        { assetIn : String
        , claimsOut : ClaimsOut
        , tooltip : Maybe Tooltip
        }
    -> ( Transaction, Cmd Msg, Maybe Effect )
query =
    constructQuery queryLend


queryPerSecond :
    { model | slippage : Slippage }
    -> Blockchain
    -> Pool
    -> PoolInfo
    ->
        { assetIn : String
        , claimsOut : ClaimsOut
        , tooltip : Maybe Tooltip
        }
    -> ( Transaction, Cmd Msg, Maybe Effect )
queryPerSecond =
    constructQuery queryLendPerSecond


constructQuery :
    (Value -> Cmd Msg)
    -> { model | slippage : Slippage }
    -> Blockchain
    -> Pool
    -> PoolInfo
    ->
        { assetIn : String
        , claimsOut : ClaimsOut
        , tooltip : Maybe Tooltip
        }
    -> ( Transaction, Cmd Msg, Maybe Effect )
constructQuery givenCmd { slippage } blockchain pool poolInfo transaction =
    (case
        ( transaction.assetIn |> Input.isZero
        , transaction.assetIn
            |> Uint.fromAmount
                (pool.pair |> Pair.toAsset)
        )
     of
        ( True, _ ) ->
            Nothing

        ( False, Just assetIn ) ->
            case transaction.claimsOut of
                Default _ ->
                    { chainId = blockchain |> Blockchain.toChain
                    , pool = pool
                    , poolInfo = poolInfo
                    , assetIn = assetIn
                    , percent = Percent.init
                    , slippage = slippage
                    }
                        |> Query.givenPercent
                        |> Just

                Slider { percent } ->
                    { chainId = blockchain |> Blockchain.toChain
                    , pool = pool
                    , poolInfo = poolInfo
                    , assetIn = assetIn
                    , percent = percent
                    , slippage = slippage
                    }
                        |> Query.givenPercent
                        |> Just

                Bond { bond } ->
                    if bond |> Input.isZero then
                        Nothing

                    else
                        bond
                            |> Uint.fromAmount
                                (pool.pair |> Pair.toAsset)
                            |> Maybe.map
                                (\bondOut ->
                                    { chainId = blockchain |> Blockchain.toChain
                                    , pool = pool
                                    , poolInfo = poolInfo
                                    , assetIn = assetIn
                                    , bondOut = bondOut
                                    , slippage = slippage
                                    }
                                        |> Query.givenBond
                                )

                Insurance { insurance } ->
                    if insurance |> Input.isZero then
                        Nothing

                    else
                        insurance
                            |> Uint.fromAmount
                                (pool.pair |> Pair.toCollateral)
                            |> Maybe.map
                                (\insuranceOut ->
                                    { chainId = blockchain |> Blockchain.toChain
                                    , pool = pool
                                    , poolInfo = poolInfo
                                    , assetIn = assetIn
                                    , insuranceOut = insuranceOut
                                    , slippage = slippage
                                    }
                                        |> Query.givenInsurance
                                )

        _ ->
            Nothing
    )
        |> Maybe.map givenCmd
        |> Maybe.withDefault Cmd.none
        |> (\cmd ->
                ( transaction |> Transaction
                , cmd
                , Nothing
                )
           )


port queryLend : Value -> Cmd msg


port queryLendPerSecond : Value -> Cmd msg


port approve : Value -> Cmd msg


port lend : Value -> Cmd msg


port receiveAnswer : (Value -> msg) -> Sub msg


subscriptions : Transaction -> Sub Msg
subscriptions (Transaction { assetIn }) =
    if assetIn |> Input.isZero then
        Sub.none

    else
        [ Time.every 1000 QueryAgain
        , receiveAnswer ReceiveAnswer
        ]
            |> Sub.batch


view :
    { model | images : Images }
    -> Blockchain
    -> Pool
    -> Transaction
    ->
        { first : Element Msg
        , second : Element Msg
        , buttons : Element Msg
        }
view model blockchain pool (Transaction transaction) =
    { first =
        transaction
            |> viewAssetIn model
                blockchain
                (pool.pair |> Pair.toAsset)
    , second =
        el
            [ width <| px 335
            , height <| px 392
            , Background.color Color.light500
            , Border.rounded 8
            ]
            none
    , buttons = none
    }


viewAssetIn :
    { model | images : Images }
    -> Blockchain
    -> Token
    -> { transaction | assetIn : String, tooltip : Maybe Tooltip }
    -> Element Msg
viewAssetIn model blockchain asset transaction =
    column
        [ Region.description "lend asset"
        , width <| px 343
        , height shrink
        , padding 16
        , spacing 10
        , Background.color Color.light500
        , Border.rounded 8
        ]
        [ row
            [ width fill
            , height shrink
            , spacing 6
            , centerY
            ]
            (el
                [ width shrink
                , height shrink
                , Font.size 14
                ]
                (text "Amount to Lend")
                :: (blockchain
                        |> Blockchain.toUser
                        |> Maybe.map
                            (\user ->
                                [ userBalance transaction user asset
                                , maxButton
                                ]
                            )
                        |> Maybe.withDefault
                            []
                   )
            )
        , Textbox.view model
            { tooltip =
                { align = Direction.Left ()
                , move = Direction.Left 0 |> Debug.log "move"
                , onMouseEnterMsg = OnMouseEnter
                , onMouseLeaveMsg = OnMouseLeave
                , given = Tooltip.AssetInSymbol
                , opened = transaction.tooltip
                }
            , main =
                { onChange = InputAssetIn
                , token = asset
                , text = transaction.assetIn
                , description = "lend asset textbox"
                }
            }
        ]


userBalance :
    { transaction | tooltip : Maybe Tooltip }
    -> User
    -> Token
    -> Element Msg
userBalance { tooltip } user asset =
    user
        |> User.getBalance asset
        |> Maybe.map
            (\balance ->
                row
                    [ width shrink
                    , height shrink
                    , alignRight
                    , centerY
                    ]
                    [ el
                        [ width shrink
                        , height shrink
                        , Font.size 12
                        , paddingXY 0 2
                        , Font.color Color.transparent300
                        ]
                        (text "Bal: ")
                    , Truncate.view
                        { tooltip =
                            { align = Direction.Right ()
                            , move = Direction.Right 0 |> Debug.log "add"
                            , onMouseEnterMsg = OnMouseEnter
                            , onMouseLeaveMsg = OnMouseLeave
                            , given = Tooltip.Balance
                            , opened = tooltip
                            }
                        , main =
                            { fontSize = 12
                            , fontPadding = 2
                            , fontColor = Color.transparent300
                            , texts =
                                Truncate.fromBalance asset balance
                            }
                        }
                    ]
            )
        |> Maybe.withDefault none


maxButton : Element Msg
maxButton =
    Input.button
        [ Region.description "max asset lend"
        , width shrink
        , height shrink
        , alignRight
        , centerY
        , Font.size 12
        , paddingXY 0 2
        , Font.color Color.warning400
        , Font.bold
        ]
        { onPress = Just InputMax
        , label = text "MAX"
        }


viewClaims :
    { model | images : Images }
    -> Pool
    -> { transaction | claimsOut : ClaimsOut, tooltip : Maybe Tooltip }
    -> Element Msg
viewClaims model pool ({ claimsOut } as transaction) =
    column
        [ Region.description "claims"
        , width <| px 343
        , height shrink
        , padding 16
        , spacing 12
        , Background.color Color.primary100
        , Border.rounded 8
        ]
        [ (case claimsOut of
            Default _ ->
                Switch.Recommended

            _ ->
                Switch.Pro
          )
            |> (\mode ->
                    Switch.view
                        { onChange = SwitchMode
                        , mode = mode
                        }
               )
        , (case claimsOut of
            Default _ ->
                Nothing

            Slider { percent } ->
                Just percent

            Bond { percent } ->
                Just percent

            Insurance { percent } ->
                Just percent
          )
            |> Maybe.map viewSlider
            |> Maybe.withDefault none
        , transaction
            |> viewClaimsOut model pool.pair
        ]


viewSlider : Percent -> Element Msg
viewSlider percent =
    column
        [ width fill
        , height shrink
        , spacing 10
        ]
        [ row
            [ width fill
            , height shrink
            , paddingXY 0 3
            , Font.size 14
            ]
            [ el
                [ alignLeft
                , Font.regular
                , Font.color Color.transparent500
                ]
                (text "Adjust your APR")
            ]
        , column
            [ width fill
            , height shrink
            , spacing 6
            ]
            [ Slider.view
                { onChange = Slide
                , click = ClickSlider
                , percent = percent
                }
            , row
                [ width fill
                , height shrink
                , paddingXY 0 2
                , Font.size 12
                , Font.color Color.transparent300
                ]
                [ el
                    [ alignLeft
                    , Font.regular
                    ]
                    (text "Low")
                , el
                    [ alignRight
                    , Font.regular
                    ]
                    (text "High")
                ]
            ]
        ]


viewClaimsOut :
    { model | images : Images }
    -> Pair
    -> { transaction | claimsOut : ClaimsOut, tooltip : Maybe Tooltip }
    -> Element Msg
viewClaimsOut model pair ({ claimsOut } as transaction) =
    case claimsOut of
        Default default ->
            column
                [ width fill
                , height shrink
                , padding 12
                , spacing 12
                , Background.color Color.primary100
                , Border.rounded 8
                ]
                (case default of
                    Success { bond, insurance } ->
                        [ bond
                            |> Just
                            |> recommendedBondOut model
                                (pair |> Pair.toAsset)
                                transaction
                        , insurance
                            |> Just
                            |> recommendedInsuranceOut model
                                (pair |> Pair.toCollateral)
                                transaction
                        ]

                    _ ->
                        [ Nothing
                            |> recommendedBondOut model
                                (pair |> Pair.toAsset)
                                transaction
                        , Nothing
                            |> recommendedInsuranceOut model
                                (pair |> Pair.toCollateral)
                                transaction
                        ]
                )

        _ ->
            none |> Debug.log "later"


recommendedBondOut :
    { model | images : Images }
    -> Token
    -> { transaction | tooltip : Maybe Tooltip }
    -> Maybe Uint
    -> Element Msg
recommendedBondOut { images } asset { tooltip } uint =
    column
        [ width fill
        , height shrink
        , spacing 10
        ]
        [ el
            [ width shrink
            , height shrink
            , Font.size 14
            , Font.color Color.primary400
            ]
            (text "Amount to Receive")
        , row
            [ width fill
            , height shrink
            , spacing 6
            , centerY
            ]
            [ images
                |> Image.viewToken
                    [ width <| px 24
                    , height <| px 24
                    ]
                    asset
            , Truncate.view
                { tooltip =
                    { align = Direction.Left ()
                    , move = Direction.Left 0 |> Debug.log "later"
                    , onMouseEnterMsg = OnMouseEnter
                    , onMouseLeaveMsg = OnMouseLeave
                    , given = Tooltip.BondOutSymbol
                    , opened = tooltip
                    }
                , main =
                    { fontSize = 18
                    , fontPadding = 3
                    , fontColor = Color.light100
                    , texts =
                        asset
                            |> Truncate.fromSymbol
                    }
                }
            , el
                [ width fill
                , height shrink
                , Font.size 18
                , paddingXY 0 3
                , Font.color Color.light100
                ]
                (uint
                    |> Maybe.map (Uint.toAmount asset)
                    |> Maybe.withDefault ""
                    |> text
                )
            ]
        ]


recommendedInsuranceOut :
    { model | images : Images }
    -> Token
    -> { transaction | tooltip : Maybe Tooltip }
    -> Maybe Uint
    -> Element Msg
recommendedInsuranceOut { images } collateral { tooltip } uint =
    column
        [ width fill
        , height shrink
        , spacing 10
        ]
        [ el
            [ width shrink
            , height shrink
            , Font.size 14
            , Font.color Color.primary400
            ]
            (text "Amount Protecting")
        , row
            [ width fill
            , height shrink
            , spacing 6
            , centerY
            ]
            [ images
                |> Image.viewToken
                    [ width <| px 24
                    , height <| px 24
                    ]
                    collateral
            , Truncate.view
                { tooltip =
                    { align = Direction.Left ()
                    , move = Direction.Left 0 |> Debug.log "later"
                    , onMouseEnterMsg = OnMouseEnter
                    , onMouseLeaveMsg = OnMouseLeave
                    , given = Tooltip.BondOutSymbol
                    , opened = tooltip
                    }
                , main =
                    { fontSize = 18
                    , fontPadding = 3
                    , fontColor = Color.light100
                    , texts =
                        collateral
                            |> Truncate.fromSymbol
                    }
                }
            , el
                [ width fill
                , height shrink
                , Font.size 18
                , paddingXY 0 3
                , Font.color Color.light100
                ]
                (uint
                    |> Maybe.map (Uint.toAmount collateral)
                    |> Maybe.withDefault ""
                    |> text
                )
            ]
        ]


disabled :
    { model | images : Images }
    -> Blockchain
    -> Pool
    -> Transaction
    ->
        { first : Element Never
        , second : Element Never
        }
disabled model blockchain pool (Transaction transaction) =
    { first =
        transaction
            |> disabledAssetIn
                model
                blockchain
                (pool.pair |> Pair.toAsset)
    , second =
        transaction
            |> disabledClaims model pool
    }


disabledAssetIn :
    { model | images : Images }
    -> Blockchain
    -> Token
    -> { transaction | assetIn : String }
    -> Element Never
disabledAssetIn model blockchain asset transaction =
    column
        [ Region.description "lend asset"
        , width <| px 343
        , height shrink
        , padding 16
        , spacing 10
        , alpha 0.2
        , Background.color Color.primary100
        , Border.rounded 8
        ]
        [ row
            [ width fill
            , height shrink
            , spacing 6
            , centerY
            ]
            (el
                [ width shrink
                , height shrink
                , Font.size 14
                , paddingXY 0 3
                , Font.color Color.primary400
                ]
                (text "Amount to Lend")
                :: (blockchain
                        |> Blockchain.toUser
                        |> Maybe.map
                            (\user ->
                                [ disabledUserBalance user asset
                                , disabledMaxButton
                                ]
                            )
                        |> Maybe.withDefault
                            []
                   )
            )
        , Textbox.disabled model
            { token = asset
            , text = transaction.assetIn
            , description = "lend asset textbox"
            }
        ]


disabledUserBalance :
    User
    -> Token
    -> Element Never
disabledUserBalance user asset =
    user
        |> User.getBalance asset
        |> Maybe.map
            (\balance ->
                row
                    [ width shrink
                    , height shrink
                    , alignRight
                    , centerY
                    ]
                    [ el
                        [ width shrink
                        , height shrink
                        , Font.size 12
                        , paddingXY 0 2
                        , Font.color Color.transparent300
                        ]
                        (text "Bal: ")
                    , Truncate.disabled
                        { fontSize = 12
                        , fontPadding = 2
                        , fontColor = Color.transparent300
                        , texts =
                            Truncate.fromBalance asset balance
                        }
                    ]
            )
        |> Maybe.withDefault none


disabledMaxButton : Element Never
disabledMaxButton =
    el
        [ Region.description "max asset lend"
        , width shrink
        , height shrink
        , alignRight
        , centerY
        , Font.size 12
        , paddingXY 0 2
        , Font.color Color.warning400
        , Font.bold
        ]
        (text "MAX")


disabledClaims :
    { model | images : Images }
    -> Pool
    -> { transaction | claimsOut : ClaimsOut }
    -> Element Never
disabledClaims model pool ({ claimsOut } as transaction) =
    column
        [ Region.description "claims"
        , width <| px 343
        , height shrink
        , padding 16
        , spacing 12
        , alpha 0.2
        , Background.color Color.primary100
        , Border.rounded 8
        ]
        [ (case claimsOut of
            Default _ ->
                Switch.Recommended

            _ ->
                Switch.Pro
          )
            |> Switch.disabled
        , (case claimsOut of
            Default _ ->
                Nothing

            Slider { percent } ->
                Just percent

            Bond { percent } ->
                Just percent

            Insurance { percent } ->
                Just percent
          )
            |> Maybe.map disabledSlider
            |> Maybe.withDefault none
        , row
            [ width fill
            , height shrink
            , spacing 16
            ]
            [ Info.emptyAPR
            , Info.emptyCDP
            ]
        , disabledClaimsOut model pool.pair transaction
        ]


disabledSlider : Percent -> Element Never
disabledSlider percent =
    column
        [ width fill
        , height shrink
        , spacing 10
        ]
        [ row
            [ width fill
            , height shrink
            , paddingXY 0 3
            , Font.size 14
            ]
            [ el
                [ alignLeft
                , Font.regular
                , Font.color Color.transparent500
                ]
                (text "Adjust your APR")
            ]
        , column
            [ width fill
            , height shrink
            , spacing 6
            ]
            [ Slider.disabled percent
            , row
                [ width fill
                , height shrink
                , paddingXY 0 2
                , Font.size 12
                , Font.color Color.transparent300
                ]
                [ el
                    [ alignLeft
                    , Font.regular
                    ]
                    (text "Low")
                , el
                    [ alignRight
                    , Font.regular
                    ]
                    (text "High")
                ]
            ]
        ]


disabledClaimsOut :
    { model | images : Images }
    -> Pair
    -> { transaction | claimsOut : ClaimsOut }
    -> Element Never
disabledClaimsOut model pair { claimsOut } =
    case claimsOut of
        Default _ ->
            column
                [ width fill
                , height shrink
                , padding 12
                , spacing 12
                , Background.color Color.primary100
                , Border.rounded 8
                ]
                [ pair
                    |> Pair.toAsset
                    |> Just
                    |> emptyBondOut model
                , pair
                    |> Pair.toCollateral
                    |> Just
                    |> emptyInsuranceOut model
                ]

        Slider _ ->
            column
                [ width fill
                , height shrink
                , spacing 12
                ]
                [ Nothing
                    |> disabledBondOut model
                        (pair |> Pair.toAsset)
                , Nothing
                    |> disabledInsuranceOut model
                        (pair |> Pair.toCollateral)
                ]

        Bond { bond } ->
            column
                [ width fill
                , height shrink
                , spacing 12
                ]
                [ bond
                    |> Just
                    |> disabledBondOut model
                        (pair |> Pair.toAsset)
                , Nothing
                    |> disabledInsuranceOut model
                        (pair |> Pair.toCollateral)
                ]

        Insurance { insurance } ->
            column
                [ width fill
                , height shrink
                , spacing 12
                ]
                [ Nothing
                    |> disabledBondOut model
                        (pair |> Pair.toAsset)
                , insurance
                    |> Just
                    |> disabledInsuranceOut model
                        (pair |> Pair.toCollateral)
                ]


disabledBondOut :
    { model | images : Images }
    -> Token
    -> Maybe String
    -> Element Never
disabledBondOut model asset input =
    column
        [ width fill
        , height shrink
        , spacing 10
        ]
        [ el
            [ width shrink
            , height shrink
            , Font.size 14
            , Font.color Color.primary400
            ]
            (text "Amount to Receive")
        , Textbox.disabled model
            { token = asset
            , text = input |> Maybe.withDefault ""
            , description = "bond out input"
            }
        ]


disabledInsuranceOut :
    { model | images : Images }
    -> Token
    -> Maybe String
    -> Element Never
disabledInsuranceOut model collateral input =
    column
        [ width fill
        , height shrink
        , spacing 10
        ]
        [ el
            [ width shrink
            , height shrink
            , Font.size 14
            , Font.color Color.primary400
            ]
            (text "Amount Protecting")
        , Textbox.disabled model
            { token = collateral
            , text = input |> Maybe.withDefault ""
            , description = "insurance out input"
            }
        ]


doesNotExist :
    { model | backdrop : Backdrop, images : Images }
    -> Pool
    -> ()
    ->
        { first : Element Never
        , second : Element Never
        , buttons : Element Never
        }
doesNotExist model pool () =
    empty model
        { asset = pool.pair |> Pair.toAsset |> Just
        , collateral = pool.pair |> Pair.toCollateral |> Just
        }
        |> (\{ first, second } ->
                { first = first
                , second = second
                , buttons = Button.error "Pool Does Not Exist"
                }
           )


disabledDoesNotExist :
    { model | images : Images }
    -> Pool
    -> ()
    ->
        { first : Element Never
        , second : Element Never
        }
disabledDoesNotExist model { pair } () =
    empty model
        { asset =
            pair
                |> Pair.toAsset
                |> Just
        , collateral =
            pair
                |> Pair.toCollateral
                |> Just
        }


empty :
    { model | images : Images }
    ->
        { asset : Maybe Token
        , collateral : Maybe Token
        }
    ->
        { first : Element Never
        , second : Element Never
        }
empty model { asset, collateral } =
    { first = emptyAssetIn model asset
    , second = emptyClaims model asset collateral
    }


emptyAssetIn :
    { model | images : Images }
    -> Maybe Token
    -> Element Never
emptyAssetIn model token =
    column
        [ Region.description "lend asset"
        , width <| px 343
        , height shrink
        , padding 16
        , spacing 10
        , alpha 0.2
        , Background.color Color.primary100
        , Border.rounded 8
        ]
        [ el
            [ width shrink
            , height shrink
            , Font.size 14
            , paddingXY 0 3
            , Font.color Color.primary400
            ]
            (text "Amount to Lend")
        , Textbox.empty model
            { token = token
            , description = "lend asset textbox"
            }
        ]


emptyClaims :
    { model | images : Images }
    -> Maybe Token
    -> Maybe Token
    -> Element Never
emptyClaims model asset collateral =
    column
        [ Region.description "claims"
        , width <| px 343
        , height shrink
        , padding 16
        , spacing 12
        , alpha 0.2
        , Background.color Color.primary100
        , Border.rounded 8
        ]
        [ Switch.empty
        , row
            [ width fill
            , height shrink
            , spacing 16
            ]
            [ Info.emptyAPR
            , Info.emptyCDP
            ]
        , emptyClaimsOut model asset collateral
        ]


emptyClaimsOut :
    { model | images : Images }
    -> Maybe Token
    -> Maybe Token
    -> Element Never
emptyClaimsOut model asset collateral =
    column
        [ width fill
        , height shrink
        , padding 12
        , spacing 12
        , Background.color Color.primary100
        , Border.rounded 8
        ]
        [ emptyBondOut model asset
        , emptyInsuranceOut model collateral
        ]


emptyBondOut :
    { model | images : Images }
    -> Maybe Token
    -> Element Never
emptyBondOut { images } asset =
    column
        [ width fill
        , height shrink
        , spacing 10
        ]
        [ el
            [ width shrink
            , height shrink
            , Font.size 14
            , Font.color Color.primary400
            ]
            (text "Amount to Receive")
        , asset
            |> Maybe.map
                (\token ->
                    row
                        [ width fill
                        , height shrink
                        , spacing 6
                        , centerY
                        ]
                        [ images
                            |> Image.viewToken
                                [ width <| px 24
                                , height <| px 24
                                ]
                                token
                        , Truncate.disabled
                            { fontSize = 18
                            , fontPadding = 3
                            , fontColor = Color.light100
                            , texts =
                                token
                                    |> Truncate.fromSymbol
                            }
                        ]
                )
            |> Maybe.withDefault
                (el
                    [ width fill
                    , height <| px 24
                    ]
                    none
                )
        ]


emptyInsuranceOut :
    { model | images : Images }
    -> Maybe Token
    -> Element Never
emptyInsuranceOut { images } collateral =
    column
        [ width fill
        , height shrink
        , spacing 10
        ]
        [ el
            [ width shrink
            , height shrink
            , Font.size 14
            , Font.color Color.primary400
            ]
            (text "Amount Protecting")
        , collateral
            |> Maybe.map
                (\token ->
                    row
                        [ width fill
                        , height shrink
                        , spacing 6
                        , centerY
                        ]
                        [ images
                            |> Image.viewToken
                                [ width <| px 24
                                , height <| px 24
                                ]
                                token
                        , Truncate.disabled
                            { fontSize = 18
                            , fontPadding = 3
                            , fontColor = Color.light100
                            , texts =
                                token
                                    |> Truncate.fromSymbol
                            }
                        ]
                )
            |> Maybe.withDefault
                (el
                    [ width fill
                    , height <| px 24
                    ]
                    none
                )
        ]
