port module Page.Transaction.Lend.Lend.Main exposing
    ( Effect(..)
    , Msg
    , Transaction
    , fromLendError
    , init
    , subscriptions
    , toLendError
    , update
    , view
    )

import Blockchain.Main as Blockchain exposing (Blockchain)
import Blockchain.User.Main as User
import Data.CDP as CDP exposing (CDP)
import Data.Deadline exposing (Deadline)
import Data.Images exposing (Images)
import Data.Or exposing (Or(..))
import Data.Pair as Pair
import Data.Percent as Percent exposing (Percent)
import Data.Pool exposing (Pool)
import Data.Remote as Remote exposing (Remote(..))
import Data.Slippage exposing (Slippage)
import Data.Spot exposing (Spot)
import Data.Token as Token exposing (Token)
import Data.Uint as Uint exposing (Uint)
import Element
    exposing
        ( Element
        , alignLeft
        , alignRight
        , centerY
        , column
        , el
        , fill
        , height
        , map
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
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Json.Decode as Decode
import Json.Encode exposing (Value)
import Page.Approve as Approve
import Page.Transaction.Button as Button
import Page.Transaction.Info as Info
import Page.Transaction.Lend.Lend.Answer as Answer
import Page.Transaction.Lend.Lend.Disabled as Disabled
import Page.Transaction.Lend.Lend.Error exposing (Error)
import Page.Transaction.Lend.Lend.Query as Query
import Page.Transaction.Lend.Lend.Tooltip as Tooltip exposing (Tooltip)
import Page.Transaction.Lend.Lend.Write as Write
import Page.Transaction.MaxButton as MaxButton
import Page.Transaction.Output as Output
import Page.Transaction.PoolInfo exposing (PoolInfo)
import Page.Transaction.Slider as Slider
import Page.Transaction.Switch as Switch exposing (Mode)
import Page.Transaction.Textbox as Textbox
import Time exposing (Posix)
import Utility.Color as Color
import Utility.Input as Input


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
    , bondOut : String
    , claims : Remote Error ClaimsGivenBond
    }


type alias InsuranceInput =
    { percent : Percent
    , claims : Remote Error ClaimsGivenInsurance
    , insuranceOut : String
    }


type alias ClaimsGivenPercent =
    { bondOut : Uint
    , insuranceOut : Uint
    , minBond : Uint
    , minInsurance : Uint
    , apr : Float
    , cdp : CDP
    }


type alias ClaimsGivenBond =
    { insuranceOut : Uint
    , minInsurance : Uint
    , apr : Float
    , cdp : CDP
    }


type alias ClaimsGivenInsurance =
    { bondOut : Uint
    , minBond : Uint
    , apr : Float
    , cdp : CDP
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
        initGivenPercent
            |> Success
            |> Default
    , tooltip = Nothing
    }
        |> Transaction


initGivenPercent : ClaimsGivenPercent
initGivenPercent =
    { bondOut = Uint.zero
    , insuranceOut = Uint.zero
    , minBond = Uint.zero
    , minInsurance = Uint.zero
    , apr = 0
    , cdp = CDP.init
    }


initGivenBond : ClaimsGivenBond
initGivenBond =
    { insuranceOut = Uint.zero
    , minInsurance = Uint.zero
    , apr = 0
    , cdp = CDP.init
    }


initGivenInsurance : ClaimsGivenInsurance
initGivenInsurance =
    { bondOut = Uint.zero
    , minBond = Uint.zero
    , apr = 0
    , cdp = CDP.init
    }


fromLendError : Disabled.Transaction -> Transaction
fromLendError { assetIn, claimsOut } =
    { assetIn = assetIn
    , claimsOut =
        case claimsOut of
            Disabled.Default ->
                Default Loading

            Disabled.Slider percent ->
                { percent = percent
                , claims = Loading
                }
                    |> Slider

            Disabled.Bond { percent, bondOut } ->
                { percent = percent
                , bondOut = bondOut
                , claims = Loading
                }
                    |> Bond

            Disabled.Insurance { percent, insuranceOut } ->
                { percent = percent
                , insuranceOut = insuranceOut
                , claims = Loading
                }
                    |> Insurance
    , tooltip = Nothing
    }
        |> Transaction


toLendError : Transaction -> Disabled.Transaction
toLendError (Transaction { assetIn, claimsOut }) =
    { assetIn = assetIn
    , claimsOut =
        case claimsOut of
            Default _ ->
                Disabled.Default

            Slider { percent } ->
                Disabled.Slider percent

            Bond { percent, bondOut } ->
                { percent = percent
                , bondOut = bondOut
                }
                    |> Disabled.Bond

            Insurance { percent, insuranceOut } ->
                { percent = percent
                , insuranceOut = insuranceOut
                }
                    |> Disabled.Insurance
    }


update :
    { model
        | time : Posix
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
                transaction |> noCmdAndEffect

        InputMax ->
            blockchain
                |> Blockchain.toUser
                |> Maybe.andThen
                    (\user ->
                        user
                            |> User.getBalance
                                (pool.pair |> Pair.toAsset)
                            |> (Maybe.map << Remote.map)
                                (Uint.toAmount
                                    (pool.pair |> Pair.toAsset)
                                )
                            |> (Maybe.map << Remote.withDefault) ""
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
                |> Maybe.withDefault (transaction |> noCmdAndEffect)

        SwitchMode Switch.Advanced ->
            { transaction
                | claimsOut =
                    if transaction.assetIn |> Input.isZero then
                        transaction.claimsOut |> switchToAdvanceZero

                    else
                        transaction.claimsOut |> switchToAdvance
            }
                |> noCmdAndEffect

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
                                    updated |> noCmdAndEffect

                                else
                                    updated |> query model blockchain pool poolInfo

                            Default _ ->
                                updated |> noCmdAndEffect

                            _ ->
                                updated |> query model blockchain pool poolInfo
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
                |> Maybe.withDefault (transaction |> noCmdAndEffect)

        Slide float ->
            case transaction.claimsOut of
                Default _ ->
                    transaction |> noCmdAndEffect

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
                        Success { bondOut } ->
                            bondOut
                                |> Uint.toAmount (pool.pair |> Pair.toAsset)
                                |> Just

                        _ ->
                            "" |> Just

                Insurance { claims } ->
                    case claims of
                        Success { bondOut } ->
                            bondOut
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
                |> Maybe.withDefault (transaction |> noCmdAndEffect)

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
                transaction |> noCmdAndEffect

        ClickInsuranceOut ->
            (case transaction.claimsOut of
                Slider { claims } ->
                    case claims of
                        Success { insuranceOut } ->
                            insuranceOut
                                |> Uint.toAmount (pool.pair |> Pair.toCollateral)
                                |> Just

                        _ ->
                            "" |> Just

                Bond { claims } ->
                    case claims of
                        Success { insuranceOut } ->
                            insuranceOut
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
                |> Maybe.withDefault (transaction |> noCmdAndEffect)

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
                transaction |> noCmdAndEffect

        QueryAgain _ ->
            transaction
                |> queryPerSecond model blockchain pool poolInfo

        ClickConnect ->
            blockchain
                |> Blockchain.toUser
                |> Maybe.map (\_ -> transaction |> noCmdAndEffect)
                |> Maybe.withDefault
                    ( transaction |> Transaction
                    , Cmd.none
                    , OpenConnect |> Just
                    )

        ClickApprove ->
            (case
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
                            |> approveLend
                        , OpenConfirm |> Just
                        )
                            |> Just

                    else
                        Nothing

                _ ->
                    Nothing
            )
                |> Maybe.withDefault (transaction |> noCmdAndEffect)

        ClickLend ->
            (case
                ( blockchain |> Blockchain.toUser
                , transaction.assetIn
                    |> Uint.fromAmount
                        (pool.pair |> Pair.toAsset)
                )
             of
                ( Just user, Just assetIn ) ->
                    case
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
                                    initGivenPercent
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
                                                    initGivenPercent
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

                        ( Bond bond, True, True ) ->
                            case
                                ( bond.claims
                                , bond.bondOut
                                    |> Uint.fromAmount (pool.pair |> Pair.toAsset)
                                )
                            of
                                ( Success answer, Just bondOut ) ->
                                    ( { transaction
                                        | assetIn = ""
                                        , claimsOut =
                                            { percent = Percent.init
                                            , bondOut = ""
                                            , claims = initGivenBond |> Success
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

                        ( Insurance insurance, True, True ) ->
                            case
                                ( insurance.claims
                                , insurance.insuranceOut
                                    |> Uint.fromAmount (pool.pair |> Pair.toCollateral)
                                )
                            of
                                ( Success answer, Just insuranceOut ) ->
                                    ( { transaction
                                        | assetIn = ""
                                        , claimsOut =
                                            { percent = Percent.init
                                            , insuranceOut = ""
                                            , claims = initGivenInsurance |> Success
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

                _ ->
                    Nothing
            )
                |> Maybe.withDefault (transaction |> noCmdAndEffect)

        ReceiveAnswer value ->
            (case
                ( value
                    |> Decode.decodeValue Answer.decoder
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
                                    |> toClaimsGivenPercent
                                    |> Default
                        }
                            |> Just

                    else
                        Nothing

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
                                            |> toClaimsGivenPercent
                                }
                                    |> Slider
                        }
                            |> Just

                    else
                        Nothing

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
                                    == (bond.bondOut
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
                                            |> toClaimsGivenBond
                                }
                                    |> Bond
                        }
                            |> Just

                    else
                        Nothing

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
                                    == (insurance.insuranceOut
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
                                            |> toClaimsGivenInsurance
                                }
                                    |> Insurance
                        }
                            |> Just

                    else
                        Nothing

                _ ->
                    Nothing
            )
                |> Maybe.map noCmdAndEffect
                |> Maybe.withDefault (transaction |> noCmdAndEffect)

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
            initGivenPercent
                |> Success
                |> Default

        Slider slider ->
            { slider
                | claims =
                    initGivenPercent |> Success
            }
                |> Slider

        Bond bond ->
            { bond
                | claims =
                    initGivenBond |> Success
            }
                |> Bond

        Insurance insurance ->
            { insurance
                | claims =
                    initGivenInsurance |> Success
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
                    if bond.bondOut |> Input.isZero then
                        initGivenBond |> Success

                    else
                        Loading
            }
                |> Bond

        Insurance insurance ->
            { insurance
                | claims =
                    if insurance.insuranceOut |> Input.isZero then
                        initGivenInsurance |> Success

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
                initGivenPercent |> Success
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
    initGivenPercent
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
    , claims = initGivenPercent |> Success
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
                , bondOut = input
                , claims = initGivenBond |> Success
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
                , bondOut = input
                , claims =
                    if input |> Input.isZero then
                        initGivenBond |> Success

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
                , insuranceOut = input
                , claims = initGivenInsurance |> Success
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
                , insuranceOut = input
                , claims =
                    if input |> Input.isZero then
                        initGivenInsurance |> Success

                    else
                        Loading
                }
                    |> Insurance
            )
        |> Maybe.withDefault claimsOut


toClaimsGivenPercent :
    Result Error Answer.ResultPercent
    -> Remote Error ClaimsGivenPercent
toClaimsGivenPercent result =
    case result of
        Ok claims ->
            Success claims

        Err error ->
            Failure error


toClaimsGivenBond :
    Result Error Answer.ResultBond
    -> Remote Error ClaimsGivenBond
toClaimsGivenBond result =
    case result of
        Ok { insuranceOut, minInsurance, apr, cdp } ->
            { insuranceOut = insuranceOut
            , minInsurance = minInsurance
            , apr = apr
            , cdp = cdp
            }
                |> Success

        Err error ->
            Failure error


toClaimsGivenInsurance :
    Result Error Answer.ResultInsurance
    -> Remote Error ClaimsGivenInsurance
toClaimsGivenInsurance result =
    case result of
        Ok { bondOut, minBond, apr, cdp } ->
            { bondOut = bondOut
            , minBond = minBond
            , apr = apr
            , cdp = cdp
            }
                |> Success

        Err error ->
            Failure error


noCmdAndEffect :
    { assetIn : String
    , claimsOut : ClaimsOut
    , tooltip : Maybe Tooltip
    }
    -> ( Transaction, Cmd Msg, Maybe Effect )
noCmdAndEffect transaction =
    ( transaction |> Transaction
    , Cmd.none
    , Nothing
    )


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

                Bond bond ->
                    if bond.bondOut |> Input.isZero then
                        Nothing

                    else
                        bond.bondOut
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

                Insurance insurance ->
                    if insurance.insuranceOut |> Input.isZero then
                        Nothing

                    else
                        insurance.insuranceOut
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


port approveLend : Value -> Cmd msg


port lend : Value -> Cmd msg


port receiveLendAnswer : (Value -> msg) -> Sub msg


subscriptions : Transaction -> Sub Msg
subscriptions (Transaction { assetIn, claimsOut }) =
    if (assetIn |> Input.isZero) && (claimsOut |> hasInputZero) then
        Sub.none

    else
        [ Time.every 1000 QueryAgain
        , receiveLendAnswer ReceiveAnswer
        ]
            |> Sub.batch


hasInputZero : ClaimsOut -> Bool
hasInputZero claimsOut =
    case claimsOut of
        Bond { bondOut } ->
            bondOut |> Input.isZero

        Insurance { insuranceOut } ->
            insuranceOut |> Input.isZero

        _ ->
            False


view :
    { model | spot : Spot, images : Images }
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
            |> assetInSection model
                blockchain
                (pool.pair |> Pair.toAsset)
    , second =
        transaction
            |> claimsOutSection model pool
    , buttons = none
    }


assetInSection :
    { model | images : Images }
    -> Blockchain
    -> Token
    -> { transaction | assetIn : String, tooltip : Maybe Tooltip }
    -> Element Msg
assetInSection model blockchain asset { assetIn, tooltip } =
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
            [ el
                [ width shrink
                , height shrink
                , Font.size 14
                ]
                (text "Amount to Lend")
            , blockchain
                |> Blockchain.toUser
                |> Maybe.andThen (User.getBalance asset)
                |> Maybe.map
                    (\balance ->
                        MaxButton.view
                            { onPress = InputMax
                            , onMouseEnter = OnMouseEnter
                            , onMouseLeave = OnMouseLeave
                            , tooltip = Tooltip.Balance
                            , opened = tooltip
                            , token = asset
                            , balance = balance
                            }
                    )
                |> Maybe.withDefault none
            ]
        , Textbox.view model
            { onMouseEnter = OnMouseEnter
            , onMouseLeave = OnMouseLeave
            , tooltip = Tooltip.AssetInSymbol
            , opened = tooltip
            , token = asset
            , onClick = Nothing
            , onChange = InputAssetIn
            , text = Left assetIn
            , description = "asset in textbox"
            }
        ]


claimsOutSection :
    { model | spot : Spot, images : Images }
    -> Pool
    -> { transaction | claimsOut : ClaimsOut, tooltip : Maybe Tooltip }
    -> Element Msg
claimsOutSection model pool ({ claimsOut, tooltip } as transaction) =
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
                Switch.Advanced
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
            |> Maybe.map sliderSection
            |> Maybe.withDefault none
        , row
            [ width fill
            , height shrink
            , spacing 16
            ]
            ((case claimsOut of
                Default default ->
                    case default of
                        Success { apr, cdp } ->
                            ( apr, cdp ) |> Just

                        _ ->
                            Nothing

                Slider { claims } ->
                    case claims of
                        Success { apr, cdp } ->
                            ( apr, cdp ) |> Just

                        _ ->
                            Nothing

                Bond { claims } ->
                    case claims of
                        Success { apr, cdp } ->
                            ( apr, cdp ) |> Just

                        _ ->
                            Nothing

                Insurance { claims } ->
                    case claims of
                        Success { apr, cdp } ->
                            ( apr, cdp ) |> Just

                        _ ->
                            Nothing
             )
                |> Maybe.map
                    (\( apr, cdp ) ->
                        [ Info.lendAPR apr
                        , Info.lendCDP model
                            { onMouseEnter = OnMouseEnter
                            , onMouseLeave = OnMouseLeave
                            , cdpTooltip = Tooltip.CDP
                            , symbolTooltip = Tooltip.CDPSymbol
                            , opened = tooltip
                            , pair = pool.pair
                            , cdp = cdp
                            }
                        ]
                    )
                |> Maybe.withDefault
                    [ Info.emptyAPR |> map never
                    , Info.emptyCDP |> map never
                    ]
            )
        , case claimsOut of
            Default default ->
                column
                    [ width fill
                    , height shrink
                    , spacing 12
                    ]
                    [ default
                        |> Remote.map .bondOut
                        |> bondOutSection model
                            (pool.pair |> Pair.toAsset)
                            transaction
                    , default
                        |> Remote.map .insuranceOut
                        |> insuranceOutSection model
                            (pool.pair |> Pair.toCollateral)
                            transaction
                    ]

            Slider { claims } ->
                column
                    [ width fill
                    , height shrink
                    , spacing 12
                    ]
                    [ claims
                        |> Remote.map .bondOut
                        |> Right
                        |> advancedBondOutSection model
                            (pool.pair |> Pair.toAsset)
                            transaction
                    , claims
                        |> Remote.map .insuranceOut
                        |> Right
                        |> advancedInsuranceOutSection model
                            (pool.pair |> Pair.toCollateral)
                            transaction
                    ]

            Bond { bondOut, claims } ->
                column
                    [ width fill
                    , height shrink
                    , spacing 12
                    ]
                    [ bondOut
                        |> Left
                        |> advancedBondOutSection model
                            (pool.pair |> Pair.toAsset)
                            transaction
                    , claims
                        |> Remote.map .insuranceOut
                        |> Right
                        |> advancedInsuranceOutSection model
                            (pool.pair |> Pair.toCollateral)
                            transaction
                    ]

            Insurance { insuranceOut, claims } ->
                column
                    [ width fill
                    , height shrink
                    , spacing 12
                    ]
                    [ claims
                        |> Remote.map .bondOut
                        |> Right
                        |> advancedBondOutSection model
                            (pool.pair |> Pair.toAsset)
                            transaction
                    , insuranceOut
                        |> Left
                        |> advancedInsuranceOutSection model
                            (pool.pair |> Pair.toCollateral)
                            transaction
                    ]
        ]


sliderSection : Percent -> Element Msg
sliderSection percent =
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


bondOutSection :
    { model | images : Images }
    -> Token
    -> { transaction | tooltip : Maybe Tooltip }
    -> Remote Error Uint
    -> Element Msg
bondOutSection model asset { tooltip } output =
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
        , Output.view model
            { onMouseEnter = OnMouseEnter
            , onMouseLeave = OnMouseLeave
            , tooltip = Tooltip.BondOutSymbol
            , opened = tooltip
            , token = asset
            , output = output
            , description = "bond output"
            }
        ]


insuranceOutSection :
    { model | images : Images }
    -> Token
    -> { transaction | tooltip : Maybe Tooltip }
    -> Remote Error Uint
    -> Element Msg
insuranceOutSection model collateral { tooltip } output =
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
        , Output.view model
            { onMouseEnter = OnMouseEnter
            , onMouseLeave = OnMouseLeave
            , tooltip = Tooltip.BondOutSymbol
            , opened = tooltip
            , token = collateral
            , output = output
            , description = "insurance output"
            }
        ]


advancedBondOutSection :
    { model | images : Images }
    -> Token
    -> { transaction | tooltip : Maybe Tooltip }
    -> Or String (Remote Error Uint)
    -> Element Msg
advancedBondOutSection model asset { tooltip } or =
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
        , Textbox.view model
            { onMouseEnter = OnMouseEnter
            , onMouseLeave = OnMouseLeave
            , tooltip = Tooltip.BondOutSymbol
            , opened = tooltip
            , token = asset
            , onClick = Just ClickBondOut
            , onChange = InputBondOut
            , text =
                case or of
                    Left string ->
                        Left string

                    Right (Success uint) ->
                        Right uint

                    Right _ ->
                        Left ""
            , description = "bond output"
            }
        ]


advancedInsuranceOutSection :
    { model | images : Images }
    -> Token
    -> { transaction | tooltip : Maybe Tooltip }
    -> Or String (Remote Error Uint)
    -> Element Msg
advancedInsuranceOutSection model collateral { tooltip } or =
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
        , Textbox.view model
            { onMouseEnter = OnMouseEnter
            , onMouseLeave = OnMouseLeave
            , tooltip = Tooltip.BondOutSymbol
            , opened = tooltip
            , token = collateral
            , onClick = Just ClickInsuranceOut
            , onChange = InputInsuranceOut
            , text =
                case or of
                    Left string ->
                        Left string

                    Right (Success uint) ->
                        Right uint

                    Right _ ->
                        Left ""
            , description = "insurance output"
            }
        ]
