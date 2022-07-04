port module Page.Transaction.Lend.Lend.Main exposing
    ( Effect(..)
    , Msg
    , Transaction
    , fromDisabled
    , init
    , subscriptions
    , toDisabled
    , update
    , view
    )

import Blockchain.Main as Blockchain exposing (Blockchain)
import Blockchain.User.Main as User
import Blockchain.User.WriteLend as WriteLend exposing (WriteLend)
import Data.CDP as CDP exposing (CDP)
import Data.ERC20 exposing (ERC20)
import Data.Images exposing (Images)
import Data.Mode as Mode exposing (Mode)
import Data.Or exposing (Or(..))
import Data.Pair as Pair
import Data.Percent as Percent exposing (Percent)
import Data.Pool exposing (Pool)
import Data.PriceFeed exposing (PriceFeed)
import Data.Remote as Remote exposing (Remote(..))
import Data.Slippage exposing (Slippage)
import Data.Theme as Theme exposing (Theme)
import Data.Token as Token exposing (Token)
import Data.Uint as Uint exposing (Uint)
import Element
    exposing
        ( Element
        , alignRight
        , below
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
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Json.Decode as Decode
import Json.Encode exposing (Value)
import Page.PoolInfo exposing (PoolInfo)
import Page.Transaction.Button as Button
import Page.Transaction.Info as Info
import Page.Transaction.Lend.Lend.Disabled as Disabled
import Page.Transaction.Lend.Lend.Error as Error exposing (Error)
import Page.Transaction.Lend.Lend.Query as Query
import Page.Transaction.Lend.Lend.Tooltip as Tooltip exposing (Tooltip)
import Page.Transaction.MaxButton as MaxButton
import Page.Transaction.Output as Output
import Page.Transaction.Slider as Slider
import Page.Transaction.Switch as Switch
import Page.Transaction.Textbox as Textbox
import Time exposing (Posix)
import Url.Builder as Builder
import Utility.Calculate as Calculate
import Utility.Color as Color
import Utility.Id as Id
import Utility.Image as Image
import Utility.Input as Input
import Utility.Loading as Loading
import Utility.ThemeColor as ThemeColor
import Utility.Tooltip as Tooltip
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
    , bondOut : String
    , claims : Remote Error ClaimsGivenBond
    }


type alias InsuranceInput =
    { percent : Percent
    , insuranceOut : String
    , claims : Remote Error ClaimsGivenInsurance
    }


type alias ClaimsGivenPercent =
    { bondOut : Uint
    , insuranceOut : Uint
    , minBond : Uint
    , minInsurance : Uint
    , apr : Float
    , cdp : CDP
    , futureApr : Float
    , futureCdp : CDP
    , txnFee : Uint
    }


type alias ClaimsGivenBond =
    { insuranceOut : Uint
    , minInsurance : Uint
    , apr : Float
    , cdp : CDP
    , futureApr : Float
    , futureCdp : CDP
    , txnFee : Uint
    }


type alias ClaimsGivenInsurance =
    { bondOut : Uint
    , minBond : Uint
    , apr : Float
    , cdp : CDP
    , futureApr : Float
    , futureCdp : CDP
    , txnFee : Uint
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
    | ClickApproveAndLend
    | ClickLend
    | ReceiveAnswer Value
    | Tick Posix
    | OnMouseEnter Tooltip
    | OnMouseLeave


type Effect
    = OpenConnect
    | OpenCaution WriteLend Float CDP PoolInfo Bool
    | Approve ERC20
    | Lend WriteLend


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
    , futureApr = 0
    , futureCdp = CDP.init
    , txnFee = Uint.zero
    }


initGivenBond : ClaimsGivenBond
initGivenBond =
    { insuranceOut = Uint.zero
    , minInsurance = Uint.zero
    , apr = 0
    , cdp = CDP.init
    , futureApr = 0
    , futureCdp = CDP.init
    , txnFee = Uint.zero
    }


initGivenInsurance : ClaimsGivenInsurance
initGivenInsurance =
    { bondOut = Uint.zero
    , minBond = Uint.zero
    , apr = 0
    , cdp = CDP.init
    , futureApr = 0
    , futureCdp = CDP.init
    , txnFee = Uint.zero
    }


fromDisabled :
    { model | slippage : Slippage }
    -> Blockchain
    -> Pool
    -> PoolInfo
    -> Disabled.Transaction
    -> ( Transaction, Cmd Msg )
fromDisabled model blockchain pool poolInfo ({ assetIn } as transaction) =
    (case ( transaction.claimsOut, assetIn |> Input.isZero ) of
        ( Disabled.Default, True ) ->
            initGivenPercent
                |> Success
                |> Default
                |> Left

        ( Disabled.Default, False ) ->
            Default Remote.loading
                |> Right

        ( Disabled.Slider percent, True ) ->
            { percent = percent
            , claims =
                initGivenPercent
                    |> Success
            }
                |> Slider
                |> Left

        ( Disabled.Slider percent, False ) ->
            { percent = percent
            , claims = Remote.loading
            }
                |> Slider
                |> Right

        ( Disabled.Bond { percent, bondOut }, True ) ->
            { percent = percent
            , bondOut = bondOut
            , claims =
                initGivenBond
                    |> Success
            }
                |> Bond
                |> Left

        ( Disabled.Bond { percent, bondOut }, False ) ->
            { percent = percent
            , bondOut = bondOut
            , claims = Remote.loading
            }
                |> Bond
                |> Right

        ( Disabled.Insurance { percent, insuranceOut }, True ) ->
            { percent = percent
            , insuranceOut = insuranceOut
            , claims =
                initGivenInsurance
                    |> Success
            }
                |> Insurance
                |> Left

        ( Disabled.Insurance { percent, insuranceOut }, False ) ->
            { percent = percent
            , insuranceOut = insuranceOut
            , claims = Remote.loading
            }
                |> Insurance
                |> Right
    )
        |> (\or ->
                case or of
                    Left claimsOut ->
                        { assetIn = assetIn
                        , claimsOut = claimsOut
                        , tooltip = Nothing
                        }
                            |> noCmd

                    Right claimsOut ->
                        { assetIn = assetIn
                        , claimsOut = claimsOut
                        , tooltip = Nothing
                        }
                            |> initQuery model blockchain pool poolInfo
           )


toDisabled : Transaction -> Disabled.Transaction
toDisabled (Transaction { assetIn, claimsOut }) =
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
    { model | slippage : Slippage }
    -> Blockchain
    -> Pool
    -> PoolInfo
    -> Msg
    -> Transaction
    -> ( Transaction, Cmd Msg, Maybe Effect )
update model blockchain pool poolInfo msg (Transaction transaction) =
    case ( msg, transaction.claimsOut ) of
        ( InputAssetIn assetIn, Default _ ) ->
            if assetIn |> Uint.isAmount (pool.pair |> Pair.toAsset) then
                { transaction
                    | assetIn = assetIn
                    , claimsOut =
                        assetIn |> updateDefault
                }
                    |> query model blockchain pool poolInfo

            else
                transaction |> noCmdAndEffect

        ( InputAssetIn assetIn, Slider { percent } ) ->
            if assetIn |> Uint.isAmount (pool.pair |> Pair.toAsset) then
                { transaction
                    | assetIn = assetIn
                    , claimsOut =
                        assetIn |> updateSlider percent
                }
                    |> query model blockchain pool poolInfo

            else
                transaction |> noCmdAndEffect

        ( InputAssetIn assetIn, Bond { bondOut, percent } ) ->
            if assetIn |> Uint.isAmount (pool.pair |> Pair.toAsset) then
                { transaction
                    | assetIn = assetIn
                    , claimsOut =
                        assetIn |> updateBond percent bondOut
                }
                    |> query model blockchain pool poolInfo

            else
                transaction |> noCmdAndEffect

        ( InputAssetIn assetIn, Insurance { insuranceOut, percent } ) ->
            if assetIn |> Uint.isAmount (pool.pair |> Pair.toAsset) then
                { transaction
                    | assetIn = assetIn
                    , claimsOut =
                        assetIn |> updateInsurance percent insuranceOut
                }
                    |> query model blockchain pool poolInfo

            else
                transaction |> noCmdAndEffect

        ( InputMax, Default _ ) ->
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
                                assetIn |> updateDefault
                        }
                            |> query model blockchain pool poolInfo
                    )
                |> Maybe.withDefault (transaction |> noCmdAndEffect)

        ( InputMax, Slider { percent } ) ->
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
                                assetIn |> updateSlider percent
                        }
                            |> query model blockchain pool poolInfo
                    )
                |> Maybe.withDefault (transaction |> noCmdAndEffect)

        ( InputMax, Bond { bondOut, percent } ) ->
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
                                assetIn |> updateBond percent bondOut
                        }
                            |> query model blockchain pool poolInfo
                    )
                |> Maybe.withDefault (transaction |> noCmdAndEffect)

        ( InputMax, Insurance { insuranceOut, percent } ) ->
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
                                assetIn |> updateInsurance percent insuranceOut
                        }
                            |> query model blockchain pool poolInfo
                    )
                |> Maybe.withDefault (transaction |> noCmdAndEffect)

        ( SwitchMode Mode.Advanced, Default _ ) ->
            { transaction
                | claimsOut =
                    transaction.assetIn |> updateSlider Percent.init
            }
                |> noCmdAndEffect

        ( SwitchMode Mode.Recommended, Slider { percent } ) ->
            { transaction
                | claimsOut =
                    transaction.assetIn |> updateDefault
            }
                |> (\updated ->
                        if (percent |> Percent.toFloat) == 64 then
                            updated |> noCmdAndEffect

                        else
                            updated |> query model blockchain pool poolInfo
                   )

        ( SwitchMode Mode.Recommended, Bond _ ) ->
            { transaction
                | claimsOut =
                    transaction.assetIn |> updateDefault
            }
                |> query model blockchain pool poolInfo

        ( SwitchMode Mode.Recommended, Insurance _ ) ->
            { transaction
                | claimsOut =
                    transaction.assetIn |> updateDefault
            }
                |> query model blockchain pool poolInfo

        ( ClickSlider, Bond { percent } ) ->
            { transaction
                | claimsOut =
                    transaction.assetIn |> updateSlider percent
            }
                |> query model blockchain pool poolInfo

        ( ClickSlider, Insurance { percent } ) ->
            { transaction
                | claimsOut =
                    transaction.assetIn |> updateSlider percent
            }
                |> query model blockchain pool poolInfo

        ( Slide float, Slider _ ) ->
            { transaction
                | claimsOut =
                    transaction.assetIn
                        |> updateSlider
                            (float |> Percent.fromFloat)
            }
                |> query model blockchain pool poolInfo

        ( Slide float, Bond _ ) ->
            { transaction
                | claimsOut =
                    transaction.assetIn
                        |> updateSlider
                            (float |> Percent.fromFloat)
            }
                |> query model blockchain pool poolInfo

        ( Slide float, Insurance _ ) ->
            { transaction
                | claimsOut =
                    transaction.assetIn
                        |> updateSlider
                            (float |> Percent.fromFloat)
            }
                |> query model blockchain pool poolInfo

        ( ClickBondOut, Slider { percent, claims } ) ->
            (case claims of
                Success { bondOut } ->
                    if transaction.assetIn |> Input.isZero then
                        ""

                    else
                        bondOut
                            |> Uint.toAmount
                                (pool.pair |> Pair.toAsset)

                _ ->
                    ""
            )
                |> (\bondOut ->
                        { transaction
                            | claimsOut =
                                transaction.assetIn
                                    |> updateBond percent bondOut
                        }
                            |> query model blockchain pool poolInfo
                   )

        ( ClickBondOut, Insurance { percent, claims } ) ->
            (case claims of
                Success { bondOut } ->
                    if transaction.assetIn |> Input.isZero then
                        ""

                    else
                        bondOut
                            |> Uint.toAmount
                                (pool.pair |> Pair.toAsset)

                _ ->
                    ""
            )
                |> (\bondOut ->
                        { transaction
                            | claimsOut =
                                transaction.assetIn
                                    |> updateBond percent bondOut
                        }
                            |> query model blockchain pool poolInfo
                   )

        ( InputBondOut bondOut, Slider { percent } ) ->
            if bondOut |> Uint.isAmount (pool.pair |> Pair.toAsset) then
                { transaction
                    | claimsOut =
                        transaction.assetIn |> updateBond percent bondOut
                }
                    |> query model blockchain pool poolInfo

            else
                transaction |> noCmdAndEffect

        ( InputBondOut bondOut, Bond { percent } ) ->
            if bondOut |> Uint.isAmount (pool.pair |> Pair.toAsset) then
                { transaction
                    | claimsOut =
                        transaction.assetIn |> updateBond percent bondOut
                }
                    |> query model blockchain pool poolInfo

            else
                transaction |> noCmdAndEffect

        ( InputBondOut bondOut, Insurance { percent } ) ->
            if bondOut |> Uint.isAmount (pool.pair |> Pair.toAsset) then
                { transaction
                    | claimsOut =
                        transaction.assetIn |> updateBond percent bondOut
                }
                    |> query model blockchain pool poolInfo

            else
                transaction |> noCmdAndEffect

        ( ClickInsuranceOut, Slider { percent, claims } ) ->
            (case claims of
                Success { insuranceOut } ->
                    if transaction.assetIn |> Input.isZero then
                        ""

                    else
                        insuranceOut
                            |> Uint.toAmount
                                (pool.pair |> Pair.toCollateral)

                _ ->
                    ""
            )
                |> (\insuranceOut ->
                        { transaction
                            | claimsOut =
                                transaction.assetIn
                                    |> updateInsurance percent insuranceOut
                        }
                            |> query model blockchain pool poolInfo
                   )

        ( ClickInsuranceOut, Bond { percent, claims } ) ->
            (case claims of
                Success { insuranceOut } ->
                    if transaction.assetIn |> Input.isZero then
                        ""

                    else
                        insuranceOut
                            |> Uint.toAmount
                                (pool.pair |> Pair.toCollateral)

                _ ->
                    ""
            )
                |> (\insuranceOut ->
                        { transaction
                            | claimsOut =
                                transaction.assetIn
                                    |> updateInsurance percent insuranceOut
                        }
                            |> query model blockchain pool poolInfo
                   )

        ( InputInsuranceOut insuranceOut, Slider { percent } ) ->
            if insuranceOut |> Uint.isAmount (pool.pair |> Pair.toCollateral) then
                { transaction
                    | claimsOut =
                        transaction.assetIn
                            |> updateInsurance percent insuranceOut
                }
                    |> query model blockchain pool poolInfo

            else
                transaction |> noCmdAndEffect

        ( InputInsuranceOut insuranceOut, Bond { percent } ) ->
            if insuranceOut |> Uint.isAmount (pool.pair |> Pair.toCollateral) then
                { transaction
                    | claimsOut =
                        transaction.assetIn
                            |> updateInsurance percent insuranceOut
                }
                    |> query model blockchain pool poolInfo

            else
                transaction |> noCmdAndEffect

        ( InputInsuranceOut insuranceOut, Insurance { percent } ) ->
            if insuranceOut |> Uint.isAmount (pool.pair |> Pair.toCollateral) then
                { transaction
                    | claimsOut =
                        transaction.assetIn
                            |> updateInsurance percent insuranceOut
                }
                    |> query model blockchain pool poolInfo

            else
                transaction |> noCmdAndEffect

        ( QueryAgain _, _ ) ->
            transaction
                |> queryPerSecond model blockchain pool poolInfo

        ( ClickConnect, _ ) ->
            blockchain
                |> Blockchain.toUser
                |> Maybe.map (\_ -> transaction |> noCmdAndEffect)
                |> Maybe.withDefault
                    ( transaction |> Transaction
                    , Cmd.none
                    , OpenConnect |> Just
                    )

        ( ClickApprove, _ ) ->
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
                        , Cmd.none
                        , erc20
                            |> Approve
                            |> Just
                        )
                            |> Just

                    else
                        Nothing

                _ ->
                    Nothing
            )
                |> Maybe.withDefault (transaction |> noCmdAndEffect)

        ( ClickApproveAndLend, Default (Success answer) ) ->
            (case
                ( blockchain |> Blockchain.toUser
                , transaction.assetIn
                    |> Uint.fromAmount
                        (pool.pair |> Pair.toAsset)
                )
             of
                ( Just user, Just assetIn ) ->
                    if
                        (user
                            |> User.hasEnoughBalance
                                (pool.pair |> Pair.toAsset)
                                assetIn
                        )
                            && (pool.pair
                                    |> Pair.toAsset
                                    |> Token.toERC20
                                    |> Maybe.map
                                        (\erc20 ->
                                            user
                                                |> User.hasEnoughAllowance
                                                    erc20
                                                    assetIn
                                                |> not
                                        )
                                    |> Maybe.withDefault True
                               )
                    then
                        ( transaction |> Transaction
                        , Cmd.none
                        , OpenCaution
                            ({ pool = pool
                             , assetIn = assetIn
                             , percent = Percent.init
                             , minBond = answer.minBond
                             , minInsurance = answer.minInsurance
                             }
                                |> WriteLend.GivenPercent
                            )
                            answer.apr
                            answer.cdp
                            poolInfo
                            False
                            |> Just
                        )
                            |> Just

                    else
                        Nothing

                _ ->
                    Nothing
            )
                |> Maybe.withDefault (transaction |> noCmdAndEffect)

        ( ClickApproveAndLend, Slider { percent, claims } ) ->
            (case
                ( claims
                , blockchain |> Blockchain.toUser
                , transaction.assetIn
                    |> Uint.fromAmount
                        (pool.pair |> Pair.toAsset)
                )
             of
                ( Success answer, Just user, Just assetIn ) ->
                    if
                        (user
                            |> User.hasEnoughBalance
                                (pool.pair |> Pair.toAsset)
                                assetIn
                        )
                            && (pool.pair
                                    |> Pair.toAsset
                                    |> Token.toERC20
                                    |> Maybe.map
                                        (\erc20 ->
                                            user
                                                |> User.hasEnoughAllowance
                                                    erc20
                                                    assetIn
                                                |> not
                                        )
                                    |> Maybe.withDefault True
                               )
                    then
                        ( transaction |> Transaction
                        , Cmd.none
                        , OpenCaution
                            ({ pool = pool
                             , assetIn = assetIn
                             , percent = percent
                             , minBond = answer.minBond
                             , minInsurance = answer.minInsurance
                             }
                                |> WriteLend.GivenPercent
                            )
                            answer.apr
                            answer.cdp
                            poolInfo
                            False
                            |> Just
                        )
                            |> Just

                    else
                        Nothing

                _ ->
                    Nothing
            )
                |> Maybe.withDefault (transaction |> noCmdAndEffect)

        ( ClickApproveAndLend, Bond bond ) ->
            (case
                ( bond.claims
                , blockchain |> Blockchain.toUser
                , ( transaction.assetIn
                        |> Uint.fromAmount
                            (pool.pair |> Pair.toAsset)
                  , bond.bondOut
                        |> Uint.fromAmount
                            (pool.pair |> Pair.toAsset)
                  )
                )
             of
                ( Success answer, Just user, ( Just assetIn, Just bondOut ) ) ->
                    if
                        (user
                            |> User.hasEnoughBalance
                                (pool.pair |> Pair.toAsset)
                                assetIn
                        )
                            && (pool.pair
                                    |> Pair.toAsset
                                    |> Token.toERC20
                                    |> Maybe.map
                                        (\erc20 ->
                                            user
                                                |> User.hasEnoughAllowance
                                                    erc20
                                                    assetIn
                                                |> not
                                        )
                                    |> Maybe.withDefault True
                               )
                    then
                        ( transaction |> Transaction
                        , Cmd.none
                        , OpenCaution
                            ({ pool = pool
                             , assetIn = assetIn
                             , bondOut = bondOut
                             , minInsurance = answer.minInsurance
                             }
                                |> WriteLend.GivenBond
                            )
                            answer.apr
                            answer.cdp
                            poolInfo
                            False
                            |> Just
                        )
                            |> Just

                    else
                        Nothing

                _ ->
                    Nothing
            )
                |> Maybe.withDefault (transaction |> noCmdAndEffect)

        ( ClickApproveAndLend, Insurance insurance ) ->
            (case
                ( insurance.claims
                , blockchain |> Blockchain.toUser
                , ( transaction.assetIn
                        |> Uint.fromAmount
                            (pool.pair |> Pair.toAsset)
                  , insurance.insuranceOut
                        |> Uint.fromAmount
                            (pool.pair |> Pair.toCollateral)
                  )
                )
             of
                ( Success answer, Just user, ( Just assetIn, Just insuranceOut ) ) ->
                    if
                        (user
                            |> User.hasEnoughBalance
                                (pool.pair |> Pair.toAsset)
                                assetIn
                        )
                            && (pool.pair
                                    |> Pair.toAsset
                                    |> Token.toERC20
                                    |> Maybe.map
                                        (\erc20 ->
                                            user
                                                |> User.hasEnoughAllowance
                                                    erc20
                                                    assetIn
                                                |> not
                                        )
                                    |> Maybe.withDefault True
                               )
                    then
                        ( transaction |> Transaction
                        , Cmd.none
                        , OpenCaution
                            ({ pool = pool
                             , assetIn = assetIn
                             , insuranceOut = insuranceOut
                             , minBond = answer.minBond
                             }
                                |> WriteLend.GivenInsurance
                            )
                            answer.apr
                            answer.cdp
                            poolInfo
                            False
                            |> Just
                        )
                            |> Just

                    else
                        Nothing

                _ ->
                    Nothing
            )
                |> Maybe.withDefault (transaction |> noCmdAndEffect)

        ( ClickLend, Default (Success answer) ) ->
            (case
                ( blockchain |> Blockchain.toUser
                , transaction.assetIn
                    |> Uint.fromAmount
                        (pool.pair |> Pair.toAsset)
                )
             of
                ( Just user, Just assetIn ) ->
                    if
                        (user
                            |> User.hasEnoughBalance
                                (pool.pair |> Pair.toAsset)
                                assetIn
                        )
                            && (pool.pair
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
                    then
                        ( transaction |> Transaction
                        , Cmd.none
                        , OpenCaution
                            ({ pool = pool
                             , assetIn = assetIn
                             , percent = Percent.init
                             , minBond = answer.minBond
                             , minInsurance = answer.minInsurance
                             }
                                |> WriteLend.GivenPercent
                            )
                            answer.apr
                            answer.cdp
                            poolInfo
                            True
                            |> Just
                        )
                            |> Just

                    else
                        Nothing

                _ ->
                    Nothing
            )
                |> Maybe.withDefault (transaction |> noCmdAndEffect)

        ( ClickLend, Slider { percent, claims } ) ->
            (case
                ( claims
                , blockchain |> Blockchain.toUser
                , transaction.assetIn
                    |> Uint.fromAmount
                        (pool.pair |> Pair.toAsset)
                )
             of
                ( Success answer, Just user, Just assetIn ) ->
                    if
                        (user
                            |> User.hasEnoughBalance
                                (pool.pair |> Pair.toAsset)
                                assetIn
                        )
                            && (pool.pair
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
                    then
                        ( transaction |> Transaction
                        , Cmd.none
                        , OpenCaution
                            ({ pool = pool
                             , assetIn = assetIn
                             , percent = percent
                             , minBond = answer.minBond
                             , minInsurance = answer.minInsurance
                             }
                                |> WriteLend.GivenPercent
                            )
                            answer.apr
                            answer.cdp
                            poolInfo
                            True
                            |> Just
                        )
                            |> Just

                    else
                        Nothing

                _ ->
                    Nothing
            )
                |> Maybe.withDefault (transaction |> noCmdAndEffect)

        ( ClickLend, Bond bond ) ->
            (case
                ( bond.claims
                , blockchain |> Blockchain.toUser
                , ( transaction.assetIn
                        |> Uint.fromAmount
                            (pool.pair |> Pair.toAsset)
                  , bond.bondOut
                        |> Uint.fromAmount
                            (pool.pair |> Pair.toAsset)
                  )
                )
             of
                ( Success answer, Just user, ( Just assetIn, Just bondOut ) ) ->
                    if
                        (user
                            |> User.hasEnoughBalance
                                (pool.pair |> Pair.toAsset)
                                assetIn
                        )
                            && (pool.pair
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
                    then
                        ( transaction |> Transaction
                        , Cmd.none
                        , OpenCaution
                            ({ pool = pool
                             , assetIn = assetIn
                             , bondOut = bondOut
                             , minInsurance = answer.minInsurance
                             }
                                |> WriteLend.GivenBond
                            )
                            answer.apr
                            answer.cdp
                            poolInfo
                            True
                            |> Just
                        )
                            |> Just

                    else
                        Nothing

                _ ->
                    Nothing
            )
                |> Maybe.withDefault (transaction |> noCmdAndEffect)

        ( ClickLend, Insurance insurance ) ->
            (case
                ( insurance.claims
                , blockchain |> Blockchain.toUser
                , ( transaction.assetIn
                        |> Uint.fromAmount
                            (pool.pair |> Pair.toAsset)
                  , insurance.insuranceOut
                        |> Uint.fromAmount
                            (pool.pair |> Pair.toCollateral)
                  )
                )
             of
                ( Success answer, Just user, ( Just assetIn, Just insuranceOut ) ) ->
                    if
                        (user
                            |> User.hasEnoughBalance
                                (pool.pair |> Pair.toAsset)
                                assetIn
                        )
                            && (pool.pair
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
                    then
                        ( transaction |> Transaction
                        , Cmd.none
                        , OpenCaution
                            ({ pool = pool
                             , assetIn = assetIn
                             , insuranceOut = insuranceOut
                             , minBond = answer.minBond
                             }
                                |> WriteLend.GivenInsurance
                            )
                            answer.apr
                            answer.cdp
                            poolInfo
                            True
                            |> Just
                        )
                            |> Just

                    else
                        Nothing

                _ ->
                    Nothing
            )
                |> Maybe.withDefault (transaction |> noCmdAndEffect)

        ( ReceiveAnswer value, Default _ ) ->
            (case value |> Decode.decodeValue Query.decoder of
                Ok (Query.GivenPercent answer) ->
                    if
                        (answer.chain == (blockchain |> Blockchain.toChain))
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
                                    |> toRemote
                                    |> Default
                        }
                            |> Just

                    else
                        Nothing

                _ ->
                    Nothing
            )
                |> Maybe.map noCmdAndEffect
                |> Maybe.withDefault (transaction |> noCmdAndEffect)

        ( ReceiveAnswer value, Slider slider ) ->
            (case value |> Decode.decodeValue Query.decoder of
                Ok (Query.GivenPercent answer) ->
                    if
                        (answer.chain == (blockchain |> Blockchain.toChain))
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
                                { slider | claims = answer.result |> toRemote }
                                    |> Slider
                        }
                            |> Just

                    else
                        Nothing

                _ ->
                    Nothing
            )
                |> Maybe.map noCmdAndEffect
                |> Maybe.withDefault (transaction |> noCmdAndEffect)

        ( ReceiveAnswer value, Bond bond ) ->
            (case value |> Decode.decodeValue Query.decoder of
                Ok (Query.GivenBond answer) ->
                    if
                        (answer.chain == (blockchain |> Blockchain.toChain))
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

                _ ->
                    Nothing
            )
                |> Maybe.map noCmdAndEffect
                |> Maybe.withDefault (transaction |> noCmdAndEffect)

        ( ReceiveAnswer value, Insurance insurance ) ->
            (case value |> Decode.decodeValue Query.decoder of
                Ok (Query.GivenInsurance answer) ->
                    if
                        (answer.chain == (blockchain |> Blockchain.toChain))
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

        ( Tick posix, Default claims ) ->
            { transaction
                | claimsOut =
                    claims
                        |> Remote.update posix
                        |> Default
            }
                |> noCmdAndEffect

        ( Tick posix, Slider slider ) ->
            { transaction
                | claimsOut =
                    { slider
                        | claims =
                            slider.claims
                                |> Remote.update posix
                    }
                        |> Slider
            }
                |> noCmdAndEffect

        ( Tick posix, Bond bond ) ->
            { transaction
                | claimsOut =
                    { bond
                        | claims =
                            bond.claims
                                |> Remote.update posix
                    }
                        |> Bond
            }
                |> noCmdAndEffect

        ( Tick posix, Insurance insurance ) ->
            { transaction
                | claimsOut =
                    { insurance
                        | claims =
                            insurance.claims
                                |> Remote.update posix
                    }
                        |> Insurance
            }
                |> noCmdAndEffect

        ( OnMouseEnter tooltip, _ ) ->
            ( { transaction | tooltip = Just tooltip }
                |> Transaction
            , Cmd.none
            , Nothing
            )

        ( OnMouseLeave, _ ) ->
            ( { transaction | tooltip = Nothing }
                |> Transaction
            , Cmd.none
            , Nothing
            )

        _ ->
            transaction |> noCmdAndEffect


updateDefault : String -> ClaimsOut
updateDefault assetIn =
    (if assetIn |> Input.isZero then
        initGivenPercent
            |> Success

     else
        Remote.loading
    )
        |> Default


updateSlider : Percent -> String -> ClaimsOut
updateSlider percent assetIn =
    { percent = percent
    , claims =
        if assetIn |> Input.isZero then
            initGivenPercent
                |> Success

        else
            Remote.loading
    }
        |> Slider


updateBond : Percent -> String -> String -> ClaimsOut
updateBond percent bondOut assetIn =
    { percent = percent
    , bondOut = bondOut
    , claims =
        if
            (assetIn |> Input.isZero)
                || (bondOut |> Input.isZero)
        then
            initGivenBond
                |> Success

        else
            Remote.loading
    }
        |> Bond


updateInsurance : Percent -> String -> String -> ClaimsOut
updateInsurance percent insuranceOut assetIn =
    { percent = percent
    , insuranceOut = insuranceOut
    , claims =
        if
            (assetIn |> Input.isZero)
                || (insuranceOut |> Input.isZero)
        then
            initGivenInsurance
                |> Success

        else
            Remote.loading
    }
        |> Insurance


toRemote :
    Result Error answer
    -> Remote Error answer
toRemote result =
    case result of
        Ok claims ->
            Success claims

        Err error ->
            Failure error


toClaimsGivenBond :
    Result Error Query.ResultBond
    -> Remote Error ClaimsGivenBond
toClaimsGivenBond result =
    case result of
        Ok { insuranceOut, minInsurance, apr, cdp, txnFee, futureApr, futureCdp } ->
            { insuranceOut = insuranceOut
            , minInsurance = minInsurance
            , apr = apr
            , cdp = cdp
            , futureApr = futureApr
            , futureCdp = futureCdp
            , txnFee = txnFee
            }
                |> Success

        Err error ->
            Failure error


toClaimsGivenInsurance :
    Result Error Query.ResultInsurance
    -> Remote Error ClaimsGivenInsurance
toClaimsGivenInsurance result =
    case result of
        Ok { bondOut, minBond, apr, cdp, txnFee, futureApr, futureCdp } ->
            { bondOut = bondOut
            , minBond = minBond
            , apr = apr
            , cdp = cdp
            , futureApr = futureApr
            , futureCdp = futureCdp
            , txnFee = txnFee
            }
                |> Success

        Err error ->
            Failure error


type Txn
    = HasTxn
    | NoTxn


fromTxnToResult :
    { transaction | assetIn : String, claimsOut : ClaimsOut }
    -> Result Error Txn
fromTxnToResult { assetIn, claimsOut } =
    if
        assetIn
            |> Input.isZero
    then
        Ok NoTxn

    else
        case claimsOut of
            Default (Success _) ->
                Ok HasTxn

            Default (Loading _) ->
                Ok NoTxn

            Default (Failure error) ->
                Err error

            Slider { claims } ->
                case claims of
                    Success _ ->
                        Ok HasTxn

                    Loading _ ->
                        Ok NoTxn

                    Failure error ->
                        Err error

            Bond { claims } ->
                case claims of
                    Success _ ->
                        Ok HasTxn

                    Loading _ ->
                        Ok NoTxn

                    Failure error ->
                        Err error

            Insurance { claims } ->
                case claims of
                    Success _ ->
                        Ok HasTxn

                    Loading _ ->
                        Ok NoTxn

                    Failure error ->
                        Err error


noCmd :
    { assetIn : String
    , claimsOut : ClaimsOut
    , tooltip : Maybe Tooltip
    }
    -> ( Transaction, Cmd Msg )
noCmd transaction =
    ( transaction |> Transaction
    , Cmd.none
    )


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


initQuery :
    { model | slippage : Slippage }
    -> Blockchain
    -> Pool
    -> PoolInfo
    ->
        { assetIn : String
        , claimsOut : ClaimsOut
        , tooltip : Maybe Tooltip
        }
    -> ( Transaction, Cmd Msg )
initQuery =
    constructQuery queryLend


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
query model blockchain pool poolInfo transaction =
    transaction
        |> constructQuery queryLend
            model
            blockchain
            pool
            poolInfo
        |> (\( updated, cmd ) ->
                ( updated
                , cmd
                , Nothing
                )
           )


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
queryPerSecond model blockchain pool poolInfo transaction =
    transaction
        |> constructQuery queryLendPerSecond
            model
            blockchain
            pool
            poolInfo
        |> (\( updated, cmd ) ->
                ( updated
                , cmd
                , Nothing
                )
           )


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
    -> ( Transaction, Cmd Msg )
constructQuery givenCmd { slippage } blockchain pool poolInfo transaction =
    (case
        ( transaction.assetIn |> Input.isZero
        , transaction.assetIn
            |> Uint.fromAmount
                (pool.pair |> Pair.toAsset)
        )
     of
        ( False, Just assetIn ) ->
            case transaction.claimsOut of
                Default _ ->
                    { chain = blockchain |> Blockchain.toChain
                    , pool = pool
                    , poolInfo = poolInfo
                    , assetIn = assetIn
                    , percent = Percent.init
                    , slippage = slippage
                    }
                        |> Query.givenPercent
                        |> Just

                Slider { percent } ->
                    { chain = blockchain |> Blockchain.toChain
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
                                    { chain = blockchain |> Blockchain.toChain
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
                                    { chain = blockchain |> Blockchain.toChain
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
                )
           )


port queryLend : Value -> Cmd msg


port queryLendPerSecond : Value -> Cmd msg


port receiveLendAnswer : (Value -> msg) -> Sub msg


subscriptions : Transaction -> Sub Msg
subscriptions (Transaction { assetIn, claimsOut }) =
    [ if (assetIn |> Input.isZero) && (claimsOut |> hasInputZero) then
        Sub.none

      else
        [ Time.every 1000 QueryAgain
        , receiveLendAnswer ReceiveAnswer
        ]
            |> Sub.batch
    , case claimsOut of
        Default claims ->
            claims |> Remote.subscriptions Tick

        Slider { claims } ->
            claims |> Remote.subscriptions Tick

        Bond { claims } ->
            claims |> Remote.subscriptions Tick

        Insurance { claims } ->
            claims |> Remote.subscriptions Tick
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
    { model | priceFeed : PriceFeed, images : Images, theme : Theme }
    -> Blockchain
    -> Pool
    -> PoolInfo
    -> Transaction
    ->
        { first : Element Msg
        , second : Element Msg
        , buttons : Element Msg
        }
view model blockchain pool poolInfo (Transaction transaction) =
    { first =
        transaction
            |> assetInSection model
                blockchain
                pool
                poolInfo
    , second =
        transaction
            |> claimsOutSection model pool poolInfo
    , buttons =
        transaction
            |> buttons model
                blockchain
                (pool.pair |> Pair.toAsset)
    }


assetInSection :
    { model | priceFeed : PriceFeed, images : Images, theme : Theme }
    -> Blockchain
    -> Pool
    -> PoolInfo
    -> { transaction | assetIn : String, tooltip : Maybe Tooltip, claimsOut : ClaimsOut }
    -> Element Msg
assetInSection model blockchain pool poolInfo { assetIn, tooltip, claimsOut } =
    column
        [ Region.description "lend asset"
        , width fill
        , height shrink
        , padding 16
        , spacing 10
        , model.theme |> ThemeColor.sectionBackground |> Background.color
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
                , paddingXY 0 3
                , model.theme |> ThemeColor.actionElemLabel |> Font.color
                ]
                (text "Amount to Lend")
            , blockchain
                |> Blockchain.toUser
                |> Maybe.andThen (User.getBalance (pool.pair |> Pair.toAsset))
                |> Maybe.map
                    (\balance ->
                        MaxButton.view
                            { onPress = InputMax
                            , onMouseEnter = OnMouseEnter
                            , onMouseLeave = OnMouseLeave
                            , tooltip = Tooltip.Balance
                            , opened = tooltip
                            , token = pool.pair |> Pair.toAsset
                            , balance = balance
                            , theme = model.theme
                            }
                    )
                |> Maybe.withDefault none
            ]
        , Textbox.view model
            { onMouseEnter = OnMouseEnter
            , onMouseLeave = OnMouseLeave
            , tooltip = Tooltip.AssetInSymbol
            , opened = tooltip
            , token = pool.pair |> Pair.toAsset
            , onClick = Nothing
            , onChange = InputAssetIn
            , text = Left assetIn
            , description = "asset in textbox"
            }
        , if assetIn |> Input.isZero then
            none

          else
            column
                [ width fill
                , height fill
                , spacing 10
                ]
                [ (case claimsOut of
                    Default (Success claimsGivenPercent) ->
                        claimsGivenPercent.txnFee |> Just

                    Slider { claims } ->
                        case claims of
                            Success claimsGivenPercent ->
                                claimsGivenPercent.txnFee |> Just

                            _ ->
                                Nothing

                    Bond { claims } ->
                        case claims of
                            Success claimsGivenBond ->
                                claimsGivenBond.txnFee |> Just

                            _ ->
                                Nothing

                    Insurance { claims } ->
                        case claims of
                            Success claimsGivenInsurance ->
                                claimsGivenInsurance.txnFee |> Just

                            _ ->
                                Nothing

                    _ ->
                        Nothing
                  )
                    |> (\maybeTxnFee ->
                            case maybeTxnFee of
                                Just txnFee ->
                                    row
                                        [ width fill
                                        , height shrink
                                        , spacing 8
                                        ]
                                        [ el
                                            [ Font.size 14
                                            , model.theme |> ThemeColor.textLight |> Font.color
                                            ]
                                            (text "Transaction Fee")
                                        , el [ alignRight ]
                                            (Truncate.viewAmount
                                                { onMouseEnter = OnMouseEnter
                                                , onMouseLeave = OnMouseLeave
                                                , tooltip = Tooltip.TxnFee
                                                , opened = tooltip
                                                , token = pool.pair |> Pair.toAsset
                                                , amount = txnFee
                                                , theme = model.theme
                                                , customStyles = [ Font.size 14 ]
                                                }
                                            )
                                        ]

                                _ ->
                                    none
                       )
                , (case claimsOut of
                    Default (Success claimsGivenPercent) ->
                        claimsGivenPercent.futureApr |> Just

                    Slider { claims } ->
                        case claims of
                            Success claimsGivenPercent ->
                                claimsGivenPercent.futureApr |> Just

                            _ ->
                                Nothing

                    Bond { claims } ->
                        case claims of
                            Success claimsGivenBond ->
                                claimsGivenBond.futureApr |> Just

                            _ ->
                                Nothing

                    Insurance { claims } ->
                        case claims of
                            Success claimsGivenInsurance ->
                                claimsGivenInsurance.futureApr |> Just

                            _ ->
                                Nothing

                    _ ->
                        Nothing
                  )
                    |> (\maybeFutureApr ->
                            case maybeFutureApr of
                                Just futureApr ->
                                    row
                                        [ width fill
                                        , height shrink
                                        ]
                                        [ row
                                            [ Font.size 14
                                            , model.theme |> ThemeColor.textLight |> Font.color
                                            , centerY
                                            ]
                                            [ text "Change in Pool APR"
                                            , model.images
                                                |> (case model.theme of
                                                        Theme.Dark ->
                                                            Image.questionMark

                                                        Theme.Light ->
                                                            Image.questionMarkDark
                                                   )
                                                    [ width <| px 12
                                                    , height <| px 12
                                                    , paddingXY 8 0
                                                    , Events.onMouseEnter (OnMouseEnter Tooltip.ApproxFutureAPR)
                                                    , Events.onMouseLeave OnMouseLeave
                                                    , (if tooltip == Just Tooltip.ApproxFutureAPR then
                                                        el
                                                            [ Font.size 14
                                                            , model.theme |> ThemeColor.textLight |> Font.color
                                                            ]
                                                            ("Approximate change in the pool's APR after this txn" |> text)
                                                            |> Tooltip.belowAlignLeft model.theme

                                                       else
                                                        none
                                                      )
                                                        |> below
                                                    ]
                                            ]
                                        , row
                                            [ alignRight
                                            , Font.size 14
                                            , model.theme |> ThemeColor.text |> Font.color
                                            ]
                                            [ if futureApr > poolInfo.apr then
                                                text "+"

                                              else
                                                none
                                            , (futureApr - poolInfo.apr) |> Calculate.apr
                                            ]
                                        ]

                                _ ->
                                    none
                       )
                , (case claimsOut of
                    Default (Success claimsGivenPercent) ->
                        claimsGivenPercent.futureCdp |> Just

                    Slider { claims } ->
                        case claims of
                            Success claimsGivenPercent ->
                                claimsGivenPercent.futureCdp |> Just

                            _ ->
                                Nothing

                    Bond { claims } ->
                        case claims of
                            Success claimsGivenBond ->
                                claimsGivenBond.futureCdp |> Just

                            _ ->
                                Nothing

                    Insurance { claims } ->
                        case claims of
                            Success claimsGivenInsurance ->
                                claimsGivenInsurance.futureCdp |> Just

                            _ ->
                                Nothing

                    _ ->
                        Nothing
                  )
                    |> (\maybeFutureCdp ->
                            case maybeFutureCdp of
                                Just futureCdp ->
                                    case ( futureCdp.percent, poolInfo.cdp.percent ) of
                                        ( Just futureCdpPerc, Just poolCdpPerc ) ->
                                            row
                                                [ width fill
                                                , height shrink
                                                , spacing 8
                                                , paddingXY 0 2
                                                ]
                                                [ row
                                                    [ Font.size 14
                                                    , model.theme |> ThemeColor.textLight |> Font.color
                                                    ]
                                                    [ text "Change in Pool CDP"
                                                    , model.images
                                                        |> (case model.theme of
                                                                Theme.Dark ->
                                                                    Image.questionMark

                                                                Theme.Light ->
                                                                    Image.questionMarkDark
                                                           )
                                                            [ width <| px 12
                                                            , height <| px 12
                                                            , paddingXY 8 0
                                                            , Events.onMouseEnter (OnMouseEnter Tooltip.ApproxFutureCDP)
                                                            , Events.onMouseLeave OnMouseLeave
                                                            , (if tooltip == Just Tooltip.ApproxFutureCDP then
                                                                el
                                                                    [ Font.size 14
                                                                    , model.theme |> ThemeColor.textLight |> Font.color
                                                                    ]
                                                                    ("Approximate change in the pool's CDP after this txn" |> text)
                                                                    |> Tooltip.belowAlignLeft model.theme

                                                               else
                                                                none
                                                              )
                                                                |> below
                                                            ]
                                                    ]
                                                , row
                                                    [ alignRight
                                                    , Font.size 14
                                                    , (if futureCdpPerc < 1 then
                                                        Color.negative500

                                                       else
                                                        model.theme |> ThemeColor.text
                                                      )
                                                        |> Font.color
                                                    ]
                                                    [ if futureCdpPerc > poolCdpPerc then
                                                        text "+"

                                                      else
                                                        none
                                                    , [ (futureCdpPerc - poolCdpPerc)
                                                            |> (*) 10000
                                                            |> truncate
                                                            |> toFloat
                                                            |> (\number -> number / 100)
                                                            |> String.fromFloat
                                                      , "%"
                                                      ]
                                                        |> String.concat
                                                        |> text
                                                    ]
                                                ]

                                        _ ->
                                            none

                                _ ->
                                    none
                       )
                ]
        ]


claimsOutSection :
    { model | priceFeed : PriceFeed, images : Images, theme : Theme }
    -> Pool
    -> PoolInfo
    -> { transaction | assetIn : String, claimsOut : ClaimsOut, tooltip : Maybe Tooltip }
    -> Element Msg
claimsOutSection model pool poolInfo ({ assetIn, claimsOut, tooltip } as transaction) =
    column
        [ Region.description "claims"
        , width fill
        , height shrink
        , padding 16
        , spacing 16
        , model.theme |> ThemeColor.sectionBackground |> Background.color
        , Border.rounded 8
        ]
        [ (case claimsOut of
            Default _ ->
                Mode.Recommended

            _ ->
                Mode.Advanced
          )
            |> (\mode ->
                    Switch.view
                        { onChange = SwitchMode
                        , mode = mode
                        , theme = model.theme
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
            |> Maybe.map
                (\percent ->
                    Slider.view
                        { onChange = Slide
                        , click = ClickSlider
                        , percent = percent
                        , min = 0
                        , max = 128
                        , theme = model.theme
                        , learnMore =
                            Builder.crossOrigin
                                "https://timeswap.gitbook.io"
                                [ "timeswap"
                                , "deep-dive"
                                , "lending"
                                ]
                                []
                        }
                )
            |> Maybe.withDefault none
        , row
            [ width fill
            , height shrink
            , spacing 16
            ]
            ((case claimsOut of
                Default default ->
                    ( default |> Remote.map .apr
                    , default |> Remote.map .cdp
                    )

                Slider { claims } ->
                    ( claims |> Remote.map .apr
                    , claims |> Remote.map .cdp
                    )

                Bond { claims } ->
                    ( claims |> Remote.map .apr
                    , claims |> Remote.map .cdp
                    )

                Insurance { claims } ->
                    ( claims |> Remote.map .apr
                    , claims |> Remote.map .cdp
                    )
             )
                |> (\( apr, cdp ) ->
                        [ Info.lendAPR apr assetIn (poolInfo |> Just) model.theme
                        , Info.lendCDP model
                            { onMouseEnter = OnMouseEnter
                            , onMouseLeave = OnMouseLeave
                            , cdpTooltip = Tooltip.CDP
                            , symbolTooltip = Tooltip.CDPSymbol
                            , opened = tooltip
                            , pair = pool.pair
                            , cdp = cdp
                            , poolInfo = poolInfo |> Just
                            , assetIn = assetIn
                            }
                        ]
                   )
            )
        , case claimsOut of
            Default default ->
                column
                    [ width fill
                    , height shrink
                    , spacing 16
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


bondOutSection :
    { model | images : Images, theme : Theme }
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
        [ row
            [ width shrink
            , height shrink
            , spacing 10
            ]
            [ el
                [ width shrink
                , height shrink
                , Font.size 14
                , paddingXY 0 3
                , model.theme |> ThemeColor.textLight |> Font.color
                ]
                (text "Amount to Receive")
            , case output of
                Loading timeline ->
                    el
                        [ width shrink
                        , height shrink
                        , centerY
                        ]
                        (Loading.view timeline model.theme)

                _ ->
                    none
            ]
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
    { model | images : Images, theme : Theme }
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
        [ row
            [ width shrink
            , height shrink
            , spacing 10
            ]
            [ el
                [ width shrink
                , height shrink
                , Font.size 14
                , paddingXY 0 3
                , model.theme |> ThemeColor.textLight |> Font.color
                ]
                (text "Amount Protecting")
            , case output of
                Loading timeline ->
                    el
                        [ width shrink
                        , height shrink
                        , centerY
                        ]
                        (Loading.view timeline model.theme)

                _ ->
                    none
            ]
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
    { model | images : Images, theme : Theme }
    -> Token
    -> { transaction | tooltip : Maybe Tooltip }
    -> Or String (Remote Error Uint)
    -> Element Msg
advancedBondOutSection ({ images, theme } as model) asset { tooltip } or =
    column
        [ width fill
        , height shrink
        , spacing 10
        ]
        [ row
            [ width shrink
            , height shrink
            , spacing 10
            ]
            [ el
                [ width shrink
                , height shrink
                , Font.size 14
                , paddingXY 0 3
                , model.theme |> ThemeColor.actionElemLabel |> Font.color
                ]
                (text "Amount to Receive")
            , images
                |> (case theme of
                        Theme.Dark ->
                            Image.info

                        Theme.Light ->
                            Image.infoDark
                   )
                    [ width <| px 16
                    , height <| px 16
                    , Events.onMouseEnter (OnMouseEnter Tooltip.AmountToReceive)
                    , Events.onMouseLeave OnMouseLeave
                    , (if tooltip == Just Tooltip.AmountToReceive then
                        el
                            [ Font.size 14
                            , theme |> ThemeColor.textLight |> Font.color
                            ]
                            ("The amount you are expected to receive at maturity based on the APR selected. This also includes tx fees" |> text)
                            |> Tooltip.belowAlignRight theme

                       else
                        none
                      )
                        |> below
                    ]
            , case or of
                Right (Loading timeline) ->
                    el
                        [ width shrink
                        , height shrink
                        , centerY
                        ]
                        (Loading.view timeline model.theme)

                _ ->
                    none
            ]
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
    { model | images : Images, theme : Theme }
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
        [ row
            [ width shrink
            , height shrink
            , spacing 10
            ]
            [ el
                [ width shrink
                , height shrink
                , Font.size 14
                , paddingXY 0 3
                , model.theme |> ThemeColor.actionElemLabel |> Font.color
                ]
                (text "Amount Protecting")
            , case or of
                Right (Loading timeline) ->
                    el
                        [ width shrink
                        , height shrink
                        , centerY
                        ]
                        (Loading.view timeline model.theme)

                _ ->
                    none
            ]
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


buttons :
    { model | theme : Theme }
    -> Blockchain
    -> Token
    -> { transaction | assetIn : String, claimsOut : ClaimsOut }
    -> Element Msg
buttons { theme } blockchain asset transaction =
    column
        [ width fill
        , height shrink
        , spacing 12
        ]
        (blockchain
            |> Blockchain.toUser
            |> Maybe.map
                (\user ->
                    case
                        ( transaction.assetIn
                            |> Uint.fromAmount asset
                        , asset |> Token.toERC20
                        , transaction |> fromTxnToResult
                        )
                    of
                        ( Just assetIn, Just erc20, Ok HasTxn ) ->
                            case
                                ( user
                                    |> User.getBalance asset
                                    |> (Maybe.map << Remote.map)
                                        (Uint.hasEnough assetIn)
                                , user
                                    |> User.getAllowance erc20
                                    |> (Maybe.map << Remote.map)
                                        (Uint.hasEnough assetIn)
                                )
                            of
                                ( Just (Success True), Just (Success True) ) ->
                                    [ lendButton theme ]

                                ( Just (Success False), Just (Success True) ) ->
                                    [ Button.notEnoughBalance ]

                                ( Just (Loading _), Just (Success True) ) ->
                                    [ theme |> Button.checkingBalance |> map never ]

                                ( Just (Success True), Just (Success False) ) ->
                                    [ approveAndLendButton erc20 theme

                                    -- , theme |> disabledLend
                                    ]

                                ( Just (Success False), Just (Success False) ) ->
                                    [ disabledApprove theme erc20
                                    , Button.notEnoughBalance
                                    ]

                                ( Just (Loading _), Just (Success False) ) ->
                                    [ disabledApprove theme erc20
                                    , theme |> Button.checkingBalance |> map never
                                    ]

                                ( Just (Success True), Just (Loading _) ) ->
                                    [ theme |> Button.checkingAllowance |> map never
                                    , theme |> disabledLend
                                    ]

                                ( Just (Success False), Just (Loading _) ) ->
                                    [ theme |> Button.checkingAllowance |> map never
                                    , Button.notEnoughBalance |> map never
                                    ]

                                ( Just (Loading _), Just (Loading _) ) ->
                                    [ theme |> Button.checkingAllowance |> map never
                                    , theme |> Button.checkingBalance |> map never
                                    ]

                                ( Just (Failure error), _ ) ->
                                    [ Button.error error |> map never ]

                                ( _, Just (Failure error) ) ->
                                    [ Button.error error |> map never ]

                                _ ->
                                    []

                        ( Just _, _, Err err ) ->
                            [ Button.customError (err |> Error.toString) |> map never ]

                        ( Just assetIn, Just erc20, Ok NoTxn ) ->
                            case
                                ( user
                                    |> User.getBalance asset
                                    |> (Maybe.map << Remote.map)
                                        (Uint.hasEnough assetIn)
                                , user
                                    |> User.getAllowance erc20
                                    |> (Maybe.map << Remote.map)
                                        (Uint.hasEnough assetIn)
                                )
                            of
                                ( Just (Failure error), _ ) ->
                                    [ Button.error error |> map never ]

                                ( _, Just (Failure error) ) ->
                                    [ Button.error error |> map never ]

                                ( _, Just (Success True) ) ->
                                    [ theme |> disabledLend ]

                                ( _, Just (Success False) ) ->
                                    [ disabledApprove theme erc20
                                    , theme |> disabledLend
                                    ]

                                ( Just (Loading _), Just (Loading _) ) ->
                                    [ theme |> Button.checkingAllowance |> map never
                                    , theme |> Button.checkingBalance |> map never
                                    ]

                                ( _, Just (Loading _) ) ->
                                    [ theme |> Button.checkingAllowance |> map never
                                    , theme |> disabledLend
                                    ]

                                _ ->
                                    []

                        ( Just assetIn, Nothing, Ok HasTxn ) ->
                            case
                                user
                                    |> User.getBalance asset
                                    |> (Maybe.map << Remote.map)
                                        (Uint.hasEnough assetIn)
                            of
                                Just (Success True) ->
                                    [ lendButton theme ]

                                Just (Success False) ->
                                    [ Button.notEnoughBalance ]

                                Just (Loading _) ->
                                    [ theme |> Button.checkingBalance |> map never ]

                                Just (Failure error) ->
                                    [ Button.error error |> map never ]

                                Nothing ->
                                    []

                        ( Just assetIn, Nothing, Ok NoTxn ) ->
                            case
                                user
                                    |> User.getBalance asset
                                    |> (Maybe.map << Remote.map)
                                        (Uint.hasEnough assetIn)
                            of
                                Just (Success True) ->
                                    [ theme |> disabledLend ]

                                Just (Success False) ->
                                    [ Button.notEnoughBalance ]

                                Just (Loading _) ->
                                    [ theme |> Button.checkingBalance |> map never ]

                                Just (Failure error) ->
                                    [ Button.error error |> map never ]

                                Nothing ->
                                    []

                        _ ->
                            []
                )
            |> Maybe.withDefault
                [ Button.connect theme ClickConnect ]
        )


lendButton : Theme -> Element Msg
lendButton theme =
    Button.view
        { onPress = ClickLend
        , text = "Lend"
        , theme = theme
        }


disabledLend : Theme -> Element msg
disabledLend theme =
    Button.disabled theme "Lend"
        |> map never


approveButton : ERC20 -> Theme -> Element Msg
approveButton erc20 theme =
    Button.approve
        { onPress = ClickApprove
        , erc20 = erc20
        , theme = theme
        }


approveAndLendButton : ERC20 -> Theme -> Element Msg
approveAndLendButton erc20 theme =
    Button.approveAndLend
        { onPress = ClickApproveAndLend
        , erc20 = erc20
        , theme = theme
        }


disabledApprove : Theme -> ERC20 -> Element msg
disabledApprove theme erc20 =
    Button.disabledApprove theme erc20
        |> map never
