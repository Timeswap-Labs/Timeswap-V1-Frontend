port module Page.Transaction.Lend.Main exposing
    ( Effect(..)
    , Msg
    , Transaction
    , init
    , subscriptions
    , update
    , viewEmpty
    )

import Blockchain.Main as Blockchain exposing (Blockchain)
import Blockchain.User.Main as User
import Data.Chains exposing (Chains)
import Data.Deadline exposing (Deadline)
import Data.Pair as Pair
import Data.Percent as Percent exposing (Percent)
import Data.Pool exposing (Pool)
import Data.Remote as Remote exposing (Remote(..))
import Data.Slippage exposing (Slippage)
import Data.Token as Token
import Data.Uint as Uint
import Element
    exposing
        ( Element
        , el
        , height
        , none
        , px
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Json.Decode as Decode
import Json.Encode exposing (Value)
import Page.Transaction.Lend.Answer as Answer
    exposing
        ( ClaimsGivenBond
        , ClaimsGivenInsurance
        , ClaimsGivenPercent
        )
import Page.Transaction.Lend.Error exposing (Error)
import Page.Transaction.Lend.Query as Query
import Page.Transaction.Lend.Tooltip exposing (Tooltip)
import Page.Transaction.Lend.Write as Write
import Page.Transaction.PoolInfo exposing (PoolInfo)
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
    | SwitchMode Bool
    | ClickSlider
    | Slide Float
    | ClickBondOut
    | InputBondOut String
    | ClickInsuranceOut
    | InputInsuranceOut String
    | QueryAgain Posix
    | Lend
    | ReceiveAnswer Value
    | OnMouseEnter Tooltip
    | OnMouseLeave


type Effect
    = OpenPending


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
                (if assetIn |> Input.isZero then
                    transaction.claimsOut |> updateGivenAssetInZero

                 else
                    transaction.claimsOut |> updateGivenAssetIn
                )
                    |> (\claimsOut ->
                            ( { transaction
                                | assetIn = assetIn
                                , claimsOut = claimsOut
                              }
                                |> Transaction
                            , query
                                model
                                blockchain
                                pool
                                poolInfo
                                assetIn
                                claimsOut
                            , Nothing
                            )
                       )

            else
                ( transaction |> Transaction
                , Cmd.none
                , Nothing
                )

        InputMax ->
            blockchain
                |> Blockchain.toUser
                |> Maybe.andThen
                    (User.getStringBalance
                        (pool.pair |> Pair.toAsset)
                    )
                |> Maybe.map
                    (\assetIn ->
                        (if assetIn |> Input.isZero then
                            transaction.claimsOut |> updateGivenAssetInZero

                         else
                            transaction.claimsOut |> updateGivenAssetIn
                        )
                            |> (\claimsOut ->
                                    ( { transaction
                                        | assetIn = assetIn
                                        , claimsOut = claimsOut
                                      }
                                        |> Transaction
                                    , query
                                        model
                                        blockchain
                                        pool
                                        poolInfo
                                        assetIn
                                        claimsOut
                                    , Nothing
                                    )
                               )
                    )
                |> Maybe.withDefault
                    ( transaction |> Transaction
                    , Cmd.none
                    , Nothing
                    )

        SwitchMode True ->
            (if transaction.assetIn |> Input.isZero then
                transaction.claimsOut |> switchToAdvanceZero

             else
                transaction.claimsOut |> switchToAdvance
            )
                |> (\claimsOut ->
                        ( { transaction
                            | claimsOut = claimsOut
                          }
                            |> Transaction
                        , Cmd.none
                        , Nothing
                        )
                   )

        SwitchMode False ->
            (if transaction.assetIn |> Input.isZero then
                switchToBasicZero

             else
                transaction.claimsOut |> switchToBasic
            )
                |> (\claimsOut ->
                        ( { transaction
                            | claimsOut = claimsOut
                          }
                            |> Transaction
                        , case transaction.claimsOut of
                            Slider { percent } ->
                                if (percent |> Percent.toFloat) == 64 then
                                    Cmd.none

                                else
                                    query
                                        model
                                        blockchain
                                        pool
                                        poolInfo
                                        transaction.assetIn
                                        claimsOut

                            Default _ ->
                                Cmd.none

                            _ ->
                                query
                                    model
                                    blockchain
                                    pool
                                    poolInfo
                                    transaction.assetIn
                                    claimsOut
                        , Nothing
                        )
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
                        if transaction.assetIn |> Input.isZero then
                            percent
                                |> Percent.toFloat
                                |> slideZero

                        else
                            percent
                                |> Percent.toFloat
                                |> slide
                    )
                |> Maybe.map
                    (\claimsOut ->
                        ( { transaction
                            | claimsOut = claimsOut
                          }
                            |> Transaction
                        , query
                            model
                            blockchain
                            pool
                            poolInfo
                            transaction.assetIn
                            claimsOut
                        , Nothing
                        )
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
                    (if transaction.assetIn |> Input.isZero then
                        float |> slideZero

                     else
                        float |> slide
                    )
                        |> (\claimsOut ->
                                ( { transaction
                                    | claimsOut = claimsOut
                                  }
                                    |> Transaction
                                , query
                                    model
                                    blockchain
                                    pool
                                    poolInfo
                                    transaction.assetIn
                                    claimsOut
                                , Nothing
                                )
                           )

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
                        if transaction.assetIn |> Input.isZero then
                            transaction.claimsOut
                                |> updateGivenBondOutZero bondOut

                        else
                            transaction.claimsOut
                                |> updateGivenBondOut bondOut
                    )
                |> Maybe.map
                    (\claimsOut ->
                        ( { transaction
                            | claimsOut = claimsOut
                          }
                            |> Transaction
                        , query
                            model
                            blockchain
                            pool
                            poolInfo
                            transaction.assetIn
                            claimsOut
                        , Nothing
                        )
                    )
                |> Maybe.withDefault
                    ( transaction |> Transaction
                    , Cmd.none
                    , Nothing
                    )

        InputBondOut bondOut ->
            if bondOut |> Uint.isAmount (pool.pair |> Pair.toAsset) then
                (if transaction.assetIn |> Input.isZero then
                    transaction.claimsOut
                        |> updateGivenBondOutZero bondOut

                 else
                    transaction.claimsOut
                        |> updateGivenBondOut bondOut
                )
                    |> (\claimsOut ->
                            ( { transaction
                                | claimsOut = claimsOut
                              }
                                |> Transaction
                            , query
                                model
                                blockchain
                                pool
                                poolInfo
                                transaction.assetIn
                                claimsOut
                            , Nothing
                            )
                       )

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
                        if transaction.assetIn |> Input.isZero then
                            transaction.claimsOut
                                |> updateGivenInsuranceOutZero insuranceOut

                        else
                            transaction.claimsOut
                                |> updateGivenInsuranceOut insuranceOut
                    )
                |> Maybe.map
                    (\claimsOut ->
                        ( { transaction
                            | claimsOut = claimsOut
                          }
                            |> Transaction
                        , query
                            model
                            blockchain
                            pool
                            poolInfo
                            transaction.assetIn
                            claimsOut
                        , Nothing
                        )
                    )
                |> Maybe.withDefault
                    ( transaction |> Transaction
                    , Cmd.none
                    , Nothing
                    )

        InputInsuranceOut insuranceOut ->
            if insuranceOut |> Uint.isAmount (pool.pair |> Pair.toCollateral) then
                (if transaction.assetIn |> Input.isZero then
                    transaction.claimsOut
                        |> updateGivenInsuranceOutZero insuranceOut

                 else
                    transaction.claimsOut
                        |> updateGivenInsuranceOut insuranceOut
                )
                    |> (\claimsOut ->
                            ( { transaction
                                | claimsOut = claimsOut
                              }
                                |> Transaction
                            , query
                                model
                                blockchain
                                pool
                                poolInfo
                                transaction.assetIn
                                claimsOut
                            , Nothing
                            )
                       )

            else
                ( transaction |> Transaction
                , Cmd.none
                , Nothing
                )

        QueryAgain _ ->
            ( transaction |> Transaction
            , queryPerSecond
                model
                blockchain
                pool
                poolInfo
                transaction.assetIn
                transaction.claimsOut
            , Nothing
            )

        Lend ->
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
                            , OpenPending |> Just
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
                                        , OpenPending |> Just
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
                                    , OpenPending |> Just
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
                                    , OpenPending |> Just
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
    -> String
    -> ClaimsOut
    -> Cmd Msg
query =
    constructQuery queryLend


queryPerSecond :
    { model | slippage : Slippage }
    -> Blockchain
    -> Pool
    -> PoolInfo
    -> String
    -> ClaimsOut
    -> Cmd Msg
queryPerSecond =
    constructQuery queryLendPerSecond


constructQuery :
    (Value -> Cmd Msg)
    -> { model | slippage : Slippage }
    -> Blockchain
    -> Pool
    -> PoolInfo
    -> String
    -> ClaimsOut
    -> Cmd Msg
constructQuery cmd { slippage } blockchain pool poolInfo string claimsOut =
    (case
        ( string |> Input.isZero
        , string
            |> Uint.fromAmount
                (pool.pair |> Pair.toAsset)
        )
     of
        ( True, _ ) ->
            Nothing

        ( False, Just assetIn ) ->
            case claimsOut of
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
        |> Maybe.map cmd
        |> Maybe.withDefault Cmd.none


port queryLend : Value -> Cmd msg


port queryLendPerSecond : Value -> Cmd msg


port lend : Value -> Cmd msg


port receiveAnswer : (Value -> msg) -> Sub msg


subscriptions : Sub Msg
subscriptions =
    [ Time.every 1000 QueryAgain
    , receiveAnswer ReceiveAnswer
    ]
        |> Sub.batch


viewEmpty :
    { first : Element msg
    , second : Element msg
    }
viewEmpty =
    { first =
        el
            [ width <| px 335
            , height <| px 109
            , Background.color Color.light500
            , Border.rounded 8
            ]
            none
    , second =
        el
            [ width <| px 335
            , height <| px 392
            , Background.color Color.light500
            , Border.rounded 8
            ]
            none
    }
