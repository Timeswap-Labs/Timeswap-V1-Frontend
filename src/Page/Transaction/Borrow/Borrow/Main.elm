port module Page.Transaction.Borrow.Borrow.Main exposing
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
import Data.CDP as CDP exposing (CDP)
import Data.Deadline exposing (Deadline)
import Data.Images exposing (Images)
import Data.Mode as Mode exposing (Mode)
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
import Page.Transaction.Borrow.Borrow.Answer as Answer
import Page.Transaction.Borrow.Borrow.Disabled as Disabled
import Page.Transaction.Borrow.Borrow.Error exposing (Error)
import Page.Transaction.Borrow.Borrow.Query as Query
import Page.Transaction.Borrow.Borrow.Tooltip as Tooltip exposing (Tooltip)
import Page.Transaction.Info as Info
import Page.Transaction.MaxButton as MaxButton
import Page.Transaction.Output as Output
import Page.Transaction.PoolInfo exposing (PoolInfo)
import Page.Transaction.Slider as Slider
import Page.Transaction.Switch as Switch
import Page.Transaction.Textbox as Textbox
import Time exposing (Posix)
import Url.Builder as Builder
import Utility.Color as Color
import Utility.Input as Input


type Transaction
    = Transaction
        { state : State
        , tooltip : Maybe Tooltip
        }


type State
    = Default DefaultInput
    | DefaultMax DefaultMaxInput
    | Slider SliderInput
    | Debt DebtInput
    | Collateral CollateralInput
    | AdvancedMax AdvancedMaxInput


type alias DefaultInput =
    { assetOut : String
    , dues : Remote Error DuesGivenPercent
    }


type alias DefaultMaxInput =
    { collateralOut : String
    , out : Remote Error OutGivenMax
    }


type alias SliderInput =
    { assetOut : String
    , percent : Percent
    , dues : Remote Error DuesGivenPercent
    }


type alias DebtInput =
    { assetOut : String
    , percent : Percent
    , debtOut : String
    , dues : Remote Error DuesGivenDebt
    }


type alias CollateralInput =
    { assetOut : String
    , percent : Percent
    , collateralOut : String
    , dues : Remote Error DuesGivenCollateral
    }


type alias AdvancedMaxInput =
    { collateralOut : String
    , percent : Percent
    , out : Remote Error OutGivenMax
    }


type alias DuesGivenPercent =
    { debtOut : Uint
    , collateralOut : Uint
    , maxDebt : Uint
    , maxCollateral : Uint
    , apr : Float
    , cdp : CDP
    }


type alias OutGivenMax =
    { assetOut : Uint
    , debtOut : Uint
    , maxDebt : Uint
    , apr : Float
    , cdp : CDP
    }


type alias DuesGivenDebt =
    { collateralOut : Uint
    , maxCollateral : Uint
    , apr : Float
    , cdp : CDP
    }


type alias DuesGivenCollateral =
    { debtOut : Uint
    , maxDebt : Uint
    , apr : Float
    , cdp : CDP
    }


type Msg
    = ClickAssetOut
    | InputAssetOut String
    | SwitchMode Mode
    | ClickSlider
    | Slide Float
    | ClickDebtOut
    | InputDebtOut String
    | ClickCollateralOut
    | InputCollateralOut String
    | InputMax
    | QueryAgain Posix
    | ClickConnect
    | ClickApprove
      -- | ClickBorrow
    | ReceiveAnswer Value
    | OnMouseEnter Tooltip
    | OnMouseLeave


type Effect
    = OpenConnect
    | OpenConfirm


init : Transaction
init =
    { state =
        { assetOut = ""
        , dues =
            initGivenPercent
                |> Success
        }
            |> Default
    , tooltip = Nothing
    }
        |> Transaction


initGivenPercent : DuesGivenPercent
initGivenPercent =
    { debtOut = Uint.zero
    , collateralOut = Uint.zero
    , maxDebt = Uint.zero
    , maxCollateral = Uint.zero
    , apr = 0
    , cdp = CDP.init
    }


initGivenMax : OutGivenMax
initGivenMax =
    { assetOut = Uint.zero
    , debtOut = Uint.zero
    , maxDebt = Uint.zero
    , apr = 0
    , cdp = CDP.init
    }


initGivenDebt : DuesGivenDebt
initGivenDebt =
    { collateralOut = Uint.zero
    , maxCollateral = Uint.zero
    , apr = 0
    , cdp = CDP.init
    }


initGivenCollateral : DuesGivenCollateral
initGivenCollateral =
    { debtOut = Uint.zero
    , maxDebt = Uint.zero
    , apr = 0
    , cdp = CDP.init
    }


fromDisabled :
    { model | slippage : Slippage }
    -> Blockchain
    -> Pool
    -> PoolInfo
    -> Disabled.Transaction
    -> ( Transaction, Cmd Msg )
fromDisabled model blockchain pool poolInfo transaction =
    (case transaction of
        Disabled.Default assetOut ->
            if assetOut |> Input.isZero then
                { assetOut = assetOut
                , dues = initGivenPercent |> Success
                }
                    |> Default
                    |> Left

            else
                { assetOut = assetOut
                , dues = Loading
                }
                    |> Default
                    |> Right

        Disabled.DefaultMax collateralOut ->
            if collateralOut |> Input.isZero then
                { collateralOut = collateralOut
                , out = initGivenMax |> Success
                }
                    |> DefaultMax
                    |> Left

            else
                { collateralOut = collateralOut
                , out = Loading
                }
                    |> DefaultMax
                    |> Right

        Disabled.Slider { assetOut, percent } ->
            if assetOut |> Input.isZero then
                { assetOut = assetOut
                , percent = percent
                , dues = initGivenPercent |> Success
                }
                    |> Slider
                    |> Left

            else
                { assetOut = assetOut
                , percent = percent
                , dues = Loading
                }
                    |> Slider
                    |> Right

        Disabled.Debt { assetOut, percent, debtOut } ->
            if
                (assetOut |> Input.isZero)
                    || (debtOut |> Input.isZero)
            then
                { assetOut = assetOut
                , percent = percent
                , debtOut = debtOut
                , dues = initGivenDebt |> Success
                }
                    |> Debt
                    |> Left

            else
                { assetOut = assetOut
                , percent = percent
                , debtOut = debtOut
                , dues = Loading
                }
                    |> Debt
                    |> Right

        Disabled.Collateral { assetOut, percent, collateralOut } ->
            if
                (assetOut |> Input.isZero)
                    || (collateralOut |> Input.isZero)
            then
                { assetOut = assetOut
                , percent = percent
                , collateralOut = collateralOut
                , dues = initGivenCollateral |> Success
                }
                    |> Collateral
                    |> Left

            else
                { assetOut = assetOut
                , percent = percent
                , collateralOut = collateralOut
                , dues = Loading
                }
                    |> Collateral
                    |> Right

        Disabled.AdvancedMax { collateralOut, percent } ->
            if collateralOut |> Input.isZero then
                { collateralOut = collateralOut
                , percent = percent
                , out = initGivenMax |> Success
                }
                    |> AdvancedMax
                    |> Left

            else
                { collateralOut = collateralOut
                , percent = percent
                , out = Loading
                }
                    |> AdvancedMax
                    |> Right
    )
        |> (\or ->
                case or of
                    Left state ->
                        { state = state
                        , tooltip = Nothing
                        }
                            |> noCmd

                    Right state ->
                        { state = state
                        , tooltip = Nothing
                        }
                            |> initQuery model blockchain pool poolInfo
           )


toDisabled : Transaction -> Disabled.Transaction
toDisabled (Transaction { state }) =
    case state of
        Default { assetOut } ->
            Disabled.Default assetOut

        DefaultMax { collateralOut } ->
            Disabled.DefaultMax collateralOut

        Slider { assetOut, percent } ->
            { assetOut = assetOut
            , percent = percent
            }
                |> Disabled.Slider

        Debt { assetOut, percent, debtOut } ->
            { assetOut = assetOut
            , percent = percent
            , debtOut = debtOut
            }
                |> Disabled.Debt

        Collateral { assetOut, percent, collateralOut } ->
            { assetOut = assetOut
            , percent = percent
            , collateralOut = collateralOut
            }
                |> Disabled.Collateral

        AdvancedMax { collateralOut, percent } ->
            { collateralOut = collateralOut
            , percent = percent
            }
                |> Disabled.AdvancedMax


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
    case ( msg, transaction.state ) of
        ( ClickAssetOut, DefaultMax { out } ) ->
            (case out of
                Success { assetOut } ->
                    assetOut
                        |> Uint.toAmount
                            (pool.pair |> Pair.toAsset)

                _ ->
                    ""
            )
                |> (\assetOut ->
                        { transaction
                            | state =
                                assetOut |> updateDefault
                        }
                            |> query model blockchain pool poolInfo
                   )

        ( ClickAssetOut, AdvancedMax { percent, out } ) ->
            (case out of
                Success { assetOut } ->
                    assetOut
                        |> Uint.toAmount
                            (pool.pair |> Pair.toAsset)

                _ ->
                    ""
            )
                |> (\assetOut ->
                        { transaction
                            | state =
                                assetOut |> updateSlider percent
                        }
                            |> query model blockchain pool poolInfo
                   )

        ( InputAssetOut assetOut, Default _ ) ->
            if assetOut |> Uint.isAmount (pool.pair |> Pair.toAsset) then
                { transaction
                    | state =
                        assetOut |> updateDefault
                }
                    |> query model blockchain pool poolInfo

            else
                transaction |> noCmdAndEffect

        ( InputAssetOut assetOut, DefaultMax _ ) ->
            if assetOut |> Uint.isAmount (pool.pair |> Pair.toAsset) then
                { transaction
                    | state =
                        assetOut |> updateDefault
                }
                    |> query model blockchain pool poolInfo

            else
                transaction |> noCmdAndEffect

        ( InputAssetOut assetOut, Slider { percent } ) ->
            if assetOut |> Uint.isAmount (pool.pair |> Pair.toAsset) then
                { transaction
                    | state =
                        assetOut |> updateSlider percent
                }
                    |> query model blockchain pool poolInfo

            else
                transaction |> noCmdAndEffect

        ( InputAssetOut assetOut, Debt { percent, debtOut } ) ->
            if assetOut |> Uint.isAmount (pool.pair |> Pair.toAsset) then
                { transaction
                    | state =
                        assetOut |> updateDebt percent debtOut
                }
                    |> query model blockchain pool poolInfo

            else
                transaction |> noCmdAndEffect

        ( InputAssetOut assetOut, Collateral { percent, collateralOut } ) ->
            if assetOut |> Uint.isAmount (pool.pair |> Pair.toAsset) then
                { transaction
                    | state =
                        assetOut |> updateCollateral percent collateralOut
                }
                    |> query model blockchain pool poolInfo

            else
                transaction |> noCmdAndEffect

        ( InputAssetOut assetOut, AdvancedMax { percent } ) ->
            if assetOut |> Uint.isAmount (pool.pair |> Pair.toAsset) then
                { transaction
                    | state =
                        assetOut |> updateSlider percent
                }
                    |> query model blockchain pool poolInfo

            else
                transaction |> noCmdAndEffect

        ( SwitchMode Mode.Advanced, Default { assetOut } ) ->
            { transaction
                | state =
                    assetOut |> updateSlider Percent.init
            }
                |> noCmdAndEffect

        ( SwitchMode Mode.Advanced, DefaultMax { collateralOut } ) ->
            { transaction
                | state =
                    collateralOut |> updateAdvancedMax Percent.init
            }
                |> noCmdAndEffect

        ( SwitchMode Mode.Recommended, Slider { assetOut, percent } ) ->
            { transaction
                | state =
                    assetOut |> updateDefault
            }
                |> (\updated ->
                        if (percent |> Percent.toFloat) == 64 then
                            updated |> noCmdAndEffect

                        else
                            updated |> query model blockchain pool poolInfo
                   )

        ( SwitchMode Mode.Recommended, Debt { assetOut } ) ->
            { transaction
                | state =
                    assetOut |> updateDefault
            }
                |> query model blockchain pool poolInfo

        ( SwitchMode Mode.Recommended, Collateral { assetOut } ) ->
            { transaction
                | state =
                    assetOut |> updateDefault
            }
                |> query model blockchain pool poolInfo

        ( SwitchMode Mode.Recommended, AdvancedMax { collateralOut, percent } ) ->
            { transaction
                | state =
                    collateralOut |> updateDefaultMax
            }
                |> (\updated ->
                        if (percent |> Percent.toFloat) == 64 then
                            updated |> noCmdAndEffect

                        else
                            updated |> query model blockchain pool poolInfo
                   )

        ( ClickSlider, Debt { assetOut, percent } ) ->
            { transaction
                | state =
                    assetOut |> updateSlider percent
            }
                |> query model blockchain pool poolInfo

        ( ClickSlider, Collateral { assetOut, percent } ) ->
            { transaction
                | state =
                    assetOut |> updateSlider percent
            }
                |> query model blockchain pool poolInfo

        ( Slide float, Slider { assetOut } ) ->
            { transaction
                | state =
                    assetOut
                        |> updateSlider
                            (float |> Percent.fromFloat)
            }
                |> query model blockchain pool poolInfo

        ( Slide float, Debt { assetOut } ) ->
            { transaction
                | state =
                    assetOut
                        |> updateSlider
                            (float |> Percent.fromFloat)
            }
                |> query model blockchain pool poolInfo

        ( Slide float, Collateral { assetOut } ) ->
            { transaction
                | state =
                    assetOut
                        |> updateSlider
                            (float |> Percent.fromFloat)
            }
                |> query model blockchain pool poolInfo

        ( Slide float, AdvancedMax { collateralOut } ) ->
            { transaction
                | state =
                    collateralOut
                        |> updateAdvancedMax
                            (float |> Percent.fromFloat)
            }
                |> query model blockchain pool poolInfo

        ( ClickDebtOut, Slider { assetOut, percent, dues } ) ->
            (case dues of
                Success { debtOut } ->
                    debtOut
                        |> Uint.toAmount
                            (pool.pair |> Pair.toAsset)

                _ ->
                    ""
            )
                |> (\debtOut ->
                        { transaction
                            | state =
                                assetOut |> updateDebt percent debtOut
                        }
                            |> query model blockchain pool poolInfo
                   )

        ( ClickDebtOut, Collateral { assetOut, percent, dues } ) ->
            (case dues of
                Success { debtOut } ->
                    debtOut
                        |> Uint.toAmount
                            (pool.pair |> Pair.toAsset)

                _ ->
                    ""
            )
                |> (\debtOut ->
                        { transaction
                            | state =
                                assetOut |> updateDebt percent debtOut
                        }
                            |> query model blockchain pool poolInfo
                   )

        ( ClickDebtOut, AdvancedMax { percent, out } ) ->
            (case out of
                Success { assetOut, debtOut } ->
                    ( assetOut
                        |> Uint.toAmount
                            (pool.pair |> Pair.toAsset)
                    , debtOut
                        |> Uint.toAmount
                            (pool.pair |> Pair.toAsset)
                    )

                _ ->
                    ( "", "" )
            )
                |> (\( assetOut, debtOut ) ->
                        { transaction
                            | state =
                                assetOut |> updateDebt percent debtOut
                        }
                            |> query model blockchain pool poolInfo
                   )

        ( InputDebtOut debtOut, Slider { assetOut, percent } ) ->
            if debtOut |> Uint.isAmount (pool.pair |> Pair.toAsset) then
                { transaction
                    | state =
                        assetOut |> updateDebt percent debtOut
                }
                    |> query model blockchain pool poolInfo

            else
                transaction |> noCmdAndEffect

        ( InputDebtOut debtOut, Debt { assetOut, percent } ) ->
            if debtOut |> Uint.isAmount (pool.pair |> Pair.toAsset) then
                { transaction
                    | state =
                        assetOut |> updateDebt percent debtOut
                }
                    |> query model blockchain pool poolInfo

            else
                transaction |> noCmdAndEffect

        ( InputDebtOut debtOut, Collateral { assetOut, percent } ) ->
            if debtOut |> Uint.isAmount (pool.pair |> Pair.toAsset) then
                { transaction
                    | state =
                        assetOut |> updateDebt percent debtOut
                }
                    |> query model blockchain pool poolInfo

            else
                transaction |> noCmdAndEffect

        ( InputDebtOut debtOut, AdvancedMax { percent, out } ) ->
            if debtOut |> Uint.isAmount (pool.pair |> Pair.toAsset) then
                (case out of
                    Success { assetOut } ->
                        assetOut
                            |> Uint.toAmount
                                (pool.pair |> Pair.toAsset)

                    _ ->
                        ""
                )
                    |> (\assetOut ->
                            { transaction
                                | state =
                                    assetOut |> updateDebt percent debtOut
                            }
                                |> query model blockchain pool poolInfo
                       )

            else
                transaction |> noCmdAndEffect

        ( ClickCollateralOut, Slider { assetOut, percent, dues } ) ->
            (case dues of
                Success { collateralOut } ->
                    collateralOut
                        |> Uint.toAmount
                            (pool.pair |> Pair.toCollateral)

                _ ->
                    ""
            )
                |> (\collateralOut ->
                        { transaction
                            | state =
                                assetOut |> updateCollateral percent collateralOut
                        }
                            |> query model blockchain pool poolInfo
                   )

        ( ClickCollateralOut, Debt { assetOut, percent, dues } ) ->
            (case dues of
                Success { collateralOut } ->
                    collateralOut
                        |> Uint.toAmount
                            (pool.pair |> Pair.toCollateral)

                _ ->
                    ""
            )
                |> (\collateralOut ->
                        { transaction
                            | state =
                                assetOut |> updateCollateral percent collateralOut
                        }
                            |> query model blockchain pool poolInfo
                   )

        ( ClickCollateralOut, AdvancedMax { collateralOut, percent, out } ) ->
            (case out of
                Success { assetOut } ->
                    assetOut
                        |> Uint.toAmount
                            (pool.pair |> Pair.toAsset)

                _ ->
                    ""
            )
                |> (\assetOut ->
                        { transaction
                            | state =
                                assetOut |> updateCollateral percent collateralOut
                        }
                            |> query model blockchain pool poolInfo
                   )

        ( InputCollateralOut collateralOut, Slider { assetOut, percent } ) ->
            if collateralOut |> Uint.isAmount (pool.pair |> Pair.toCollateral) then
                { transaction
                    | state =
                        assetOut |> updateCollateral percent collateralOut
                }
                    |> query model blockchain pool poolInfo

            else
                transaction |> noCmdAndEffect

        ( InputCollateralOut collateralOut, Debt { assetOut, percent } ) ->
            if collateralOut |> Uint.isAmount (pool.pair |> Pair.toCollateral) then
                { transaction
                    | state =
                        assetOut |> updateCollateral percent collateralOut
                }
                    |> query model blockchain pool poolInfo

            else
                transaction |> noCmdAndEffect

        ( InputCollateralOut collateralOut, Collateral { assetOut, percent } ) ->
            if collateralOut |> Uint.isAmount (pool.pair |> Pair.toCollateral) then
                { transaction
                    | state =
                        assetOut |> updateCollateral percent collateralOut
                }
                    |> query model blockchain pool poolInfo

            else
                transaction |> noCmdAndEffect

        ( InputCollateralOut collateralOut, AdvancedMax { percent, out } ) ->
            if collateralOut |> Uint.isAmount (pool.pair |> Pair.toCollateral) then
                (case out of
                    Success { assetOut } ->
                        assetOut
                            |> Uint.toAmount
                                (pool.pair |> Pair.toAsset)

                    _ ->
                        ""
                )
                    |> (\assetOut ->
                            { transaction
                                | state =
                                    assetOut |> updateCollateral percent collateralOut
                            }
                                |> query model blockchain pool poolInfo
                       )

            else
                transaction |> noCmdAndEffect

        ( InputMax, Default _ ) ->
            blockchain
                |> Blockchain.toUser
                |> Maybe.andThen
                    (\user ->
                        user
                            |> User.getBalance
                                (pool.pair |> Pair.toCollateral)
                            |> (Maybe.map << Remote.map)
                                (Uint.toAmount
                                    (pool.pair |> Pair.toCollateral)
                                )
                            |> (Maybe.map << Remote.withDefault) ""
                    )
                |> Maybe.map
                    (\collateralOut ->
                        { transaction
                            | state =
                                collateralOut |> updateDefaultMax
                        }
                            |> query model blockchain pool poolInfo
                    )
                |> Maybe.withDefault (transaction |> noCmdAndEffect)

        ( InputMax, DefaultMax _ ) ->
            blockchain
                |> Blockchain.toUser
                |> Maybe.andThen
                    (\user ->
                        user
                            |> User.getBalance
                                (pool.pair |> Pair.toCollateral)
                            |> (Maybe.map << Remote.map)
                                (Uint.toAmount
                                    (pool.pair |> Pair.toCollateral)
                                )
                            |> (Maybe.map << Remote.withDefault) ""
                    )
                |> Maybe.map
                    (\collateralOut ->
                        { transaction
                            | state =
                                collateralOut |> updateDefaultMax
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
                                (pool.pair |> Pair.toCollateral)
                            |> (Maybe.map << Remote.map)
                                (Uint.toAmount
                                    (pool.pair |> Pair.toCollateral)
                                )
                            |> (Maybe.map << Remote.withDefault) ""
                    )
                |> Maybe.map
                    (\collateralOut ->
                        { transaction
                            | state =
                                collateralOut |> updateAdvancedMax percent
                        }
                            |> query model blockchain pool poolInfo
                    )
                |> Maybe.withDefault (transaction |> noCmdAndEffect)

        ( InputMax, Debt { percent } ) ->
            blockchain
                |> Blockchain.toUser
                |> Maybe.andThen
                    (\user ->
                        user
                            |> User.getBalance
                                (pool.pair |> Pair.toCollateral)
                            |> (Maybe.map << Remote.map)
                                (Uint.toAmount
                                    (pool.pair |> Pair.toCollateral)
                                )
                            |> (Maybe.map << Remote.withDefault) ""
                    )
                |> Maybe.map
                    (\collateralOut ->
                        { transaction
                            | state =
                                collateralOut |> updateAdvancedMax percent
                        }
                            |> query model blockchain pool poolInfo
                    )
                |> Maybe.withDefault (transaction |> noCmdAndEffect)

        ( InputMax, Collateral { percent } ) ->
            blockchain
                |> Blockchain.toUser
                |> Maybe.andThen
                    (\user ->
                        user
                            |> User.getBalance
                                (pool.pair |> Pair.toCollateral)
                            |> (Maybe.map << Remote.map)
                                (Uint.toAmount
                                    (pool.pair |> Pair.toCollateral)
                                )
                            |> (Maybe.map << Remote.withDefault) ""
                    )
                |> Maybe.map
                    (\collateralOut ->
                        { transaction
                            | state =
                                collateralOut |> updateAdvancedMax percent
                        }
                            |> query model blockchain pool poolInfo
                    )
                |> Maybe.withDefault (transaction |> noCmdAndEffect)

        ( InputMax, AdvancedMax { percent } ) ->
            blockchain
                |> Blockchain.toUser
                |> Maybe.andThen
                    (\user ->
                        user
                            |> User.getBalance
                                (pool.pair |> Pair.toCollateral)
                            |> (Maybe.map << Remote.map)
                                (Uint.toAmount
                                    (pool.pair |> Pair.toCollateral)
                                )
                            |> (Maybe.map << Remote.withDefault) ""
                    )
                |> Maybe.map
                    (\collateralOut ->
                        { transaction
                            | state =
                                collateralOut |> updateAdvancedMax percent
                        }
                            |> query model blockchain pool poolInfo
                    )
                |> Maybe.withDefault (transaction |> noCmdAndEffect)

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
                , case transaction.state of
                    Default { dues } ->
                        case dues of
                            Success { collateralOut } ->
                                collateralOut |> Just

                            _ ->
                                Nothing

                    DefaultMax { collateralOut } ->
                        collateralOut
                            |> Uint.fromAmount
                                (pool.pair |> Pair.toCollateral)

                    Slider { dues } ->
                        case dues of
                            Success { collateralOut } ->
                                collateralOut |> Just

                            _ ->
                                Nothing

                    Debt { dues } ->
                        case dues of
                            Success { collateralOut } ->
                                collateralOut |> Just

                            _ ->
                                Nothing

                    Collateral { collateralOut } ->
                        collateralOut
                            |> Uint.fromAmount
                                (pool.pair |> Pair.toCollateral)

                    AdvancedMax { collateralOut } ->
                        collateralOut
                            |> Uint.fromAmount
                                (pool.pair |> Pair.toCollateral)
                , pool.pair
                    |> Pair.toCollateral
                    |> Token.toERC20
                )
             of
                ( Just user, Just collateralOut, Just erc20 ) ->
                    if
                        (user
                            |> User.hasEnoughBalance
                                (pool.pair |> Pair.toCollateral)
                                collateralOut
                        )
                            && (user
                                    |> User.hasEnoughAllowance
                                        erc20
                                        collateralOut
                                    |> not
                               )
                    then
                        ( transaction |> Transaction
                        , erc20
                            |> Approve.encode blockchain user
                            |> approveBorrow
                        , OpenConfirm |> Just
                        )
                            |> Just

                    else
                        Nothing

                _ ->
                    Nothing
            )
                |> Maybe.withDefault (transaction |> noCmdAndEffect)

        ( ReceiveAnswer value, Default default ) ->
            (case value |> Decode.decodeValue Answer.decoder of
                Ok (Answer.GivenPercent answer) ->
                    if
                        (answer.chainId == (blockchain |> Blockchain.toChain))
                            && (answer.pool == pool)
                            && (answer.poolInfo == poolInfo)
                            && (Just answer.assetOut
                                    == (default.assetOut
                                            |> Uint.fromAmount
                                                (pool.pair |> Pair.toAsset)
                                       )
                               )
                            && (answer.percent == Percent.init)
                            && (answer.slippage == model.slippage)
                    then
                        { transaction
                            | state =
                                { default | dues = answer.result |> toRemote }
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

        ( ReceiveAnswer value, DefaultMax defaultMax ) ->
            (case value |> Decode.decodeValue Answer.decoder of
                Ok (Answer.GivenMax answer) ->
                    if
                        (answer.chainId == (blockchain |> Blockchain.toChain))
                            && (answer.pool == pool)
                            && (answer.poolInfo == poolInfo)
                            && (Just answer.collateralOut
                                    == (defaultMax.collateralOut
                                            |> Uint.fromAmount
                                                (pool.pair |> Pair.toCollateral)
                                       )
                               )
                            && (answer.percent == Percent.init)
                            && (answer.slippage == model.slippage)
                    then
                        { transaction
                            | state =
                                { defaultMax | out = answer.result |> toRemote }
                                    |> DefaultMax
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
            (case value |> Decode.decodeValue Answer.decoder of
                Ok (Answer.GivenPercent answer) ->
                    if
                        (answer.chainId == (blockchain |> Blockchain.toChain))
                            && (answer.pool == pool)
                            && (answer.poolInfo == poolInfo)
                            && (Just answer.assetOut
                                    == (slider.assetOut
                                            |> Uint.fromAmount
                                                (pool.pair |> Pair.toAsset)
                                       )
                               )
                            && (answer.percent == slider.percent)
                            && (answer.slippage == model.slippage)
                    then
                        { transaction
                            | state =
                                { slider | dues = answer.result |> toRemote }
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

        ( ReceiveAnswer value, Debt debt ) ->
            (case value |> Decode.decodeValue Answer.decoder of
                Ok (Answer.GivenDebt answer) ->
                    if
                        (answer.chainId == (blockchain |> Blockchain.toChain))
                            && (answer.pool == pool)
                            && (answer.poolInfo == poolInfo)
                            && (Just answer.assetOut
                                    == (debt.assetOut
                                            |> Uint.fromAmount
                                                (pool.pair |> Pair.toAsset)
                                       )
                               )
                            && (Just answer.debtOut
                                    == (debt.debtOut
                                            |> Uint.fromAmount
                                                (pool.pair |> Pair.toAsset)
                                       )
                               )
                            && (answer.slippage == model.slippage)
                    then
                        { transaction
                            | state =
                                { debt
                                    | percent =
                                        answer.result
                                            |> Result.map .percent
                                            |> Result.withDefault debt.percent
                                    , dues =
                                        answer.result
                                            |> toStateGivenDebt
                                }
                                    |> Debt
                        }
                            |> Just

                    else
                        Nothing

                _ ->
                    Nothing
            )
                |> Maybe.map noCmdAndEffect
                |> Maybe.withDefault (transaction |> noCmdAndEffect)

        ( ReceiveAnswer value, Collateral collateral ) ->
            (case value |> Decode.decodeValue Answer.decoder of
                Ok (Answer.GivenCollateral answer) ->
                    if
                        (answer.chainId == (blockchain |> Blockchain.toChain))
                            && (answer.pool == pool)
                            && (answer.poolInfo == poolInfo)
                            && (Just answer.assetOut
                                    == (collateral.assetOut
                                            |> Uint.fromAmount
                                                (pool.pair |> Pair.toAsset)
                                       )
                               )
                            && (Just answer.collateralOut
                                    == (collateral.collateralOut
                                            |> Uint.fromAmount
                                                (pool.pair |> Pair.toCollateral)
                                       )
                               )
                            && (answer.slippage == model.slippage)
                    then
                        { transaction
                            | state =
                                { collateral
                                    | percent =
                                        answer.result
                                            |> Result.map .percent
                                            |> Result.withDefault collateral.percent
                                    , dues =
                                        answer.result
                                            |> toStateGivenCollateral
                                }
                                    |> Collateral
                        }
                            |> Just

                    else
                        Nothing

                _ ->
                    Nothing
            )
                |> Maybe.map noCmdAndEffect
                |> Maybe.withDefault (transaction |> noCmdAndEffect)

        ( ReceiveAnswer value, AdvancedMax advancedMax ) ->
            (case value |> Decode.decodeValue Answer.decoder of
                Ok (Answer.GivenMax answer) ->
                    if
                        (answer.chainId == (blockchain |> Blockchain.toChain))
                            && (answer.pool == pool)
                            && (answer.poolInfo == poolInfo)
                            && (Just answer.collateralOut
                                    == (advancedMax.collateralOut
                                            |> Uint.fromAmount
                                                (pool.pair |> Pair.toCollateral)
                                       )
                               )
                            && (answer.percent == advancedMax.percent)
                            && (answer.slippage == model.slippage)
                    then
                        { transaction
                            | state =
                                { advancedMax | out = answer.result |> toRemote }
                                    |> AdvancedMax
                        }
                            |> Just

                    else
                        Nothing

                _ ->
                    Nothing
            )
                |> Maybe.map noCmdAndEffect
                |> Maybe.withDefault (transaction |> noCmdAndEffect)

        ( OnMouseEnter tooltip, _ ) ->
            { transaction | tooltip = Just tooltip }
                |> noCmdAndEffect

        ( OnMouseLeave, _ ) ->
            { transaction | tooltip = Nothing }
                |> noCmdAndEffect

        _ ->
            transaction |> noCmdAndEffect


updateDefault : String -> State
updateDefault assetOut =
    { assetOut = assetOut
    , dues =
        if assetOut |> Input.isZero then
            initGivenPercent
                |> Success

        else
            Loading
    }
        |> Default


updateDefaultMax : String -> State
updateDefaultMax collateralOut =
    { collateralOut = collateralOut
    , out =
        if collateralOut |> Input.isZero then
            initGivenMax
                |> Success

        else
            Loading
    }
        |> DefaultMax


updateSlider : Percent -> String -> State
updateSlider percent assetOut =
    { assetOut = assetOut
    , percent = percent
    , dues =
        if assetOut |> Input.isZero then
            initGivenPercent
                |> Success

        else
            Loading
    }
        |> Slider


updateDebt : Percent -> String -> String -> State
updateDebt percent debtOut assetOut =
    { assetOut = assetOut
    , percent = percent
    , debtOut = debtOut
    , dues =
        if
            (assetOut |> Input.isZero)
                || (debtOut |> Input.isZero)
        then
            initGivenDebt
                |> Success

        else
            Loading
    }
        |> Debt


updateCollateral : Percent -> String -> String -> State
updateCollateral percent collateralOut assetOut =
    { assetOut = assetOut
    , percent = percent
    , collateralOut = collateralOut
    , dues =
        if
            (assetOut |> Input.isZero)
                || (collateralOut |> Input.isZero)
        then
            initGivenCollateral
                |> Success

        else
            Loading
    }
        |> Collateral


updateAdvancedMax : Percent -> String -> State
updateAdvancedMax percent collateralOut =
    { collateralOut = collateralOut
    , percent = percent
    , out =
        if collateralOut |> Input.isZero then
            initGivenMax
                |> Success

        else
            Loading
    }
        |> AdvancedMax


toRemote :
    Result Error answer
    -> Remote Error answer
toRemote result =
    case result of
        Ok out ->
            Success out

        Err error ->
            Failure error


toStateGivenDebt :
    Result Error Answer.ResultDebt
    -> Remote Error DuesGivenDebt
toStateGivenDebt result =
    case result of
        Ok { collateralOut, maxCollateral, apr, cdp } ->
            { collateralOut = collateralOut
            , maxCollateral = maxCollateral
            , apr = apr
            , cdp = cdp
            }
                |> Success

        Err error ->
            Failure error


toStateGivenCollateral :
    Result Error Answer.ResultCollateral
    -> Remote Error DuesGivenCollateral
toStateGivenCollateral result =
    case result of
        Ok { debtOut, maxDebt, apr, cdp } ->
            { debtOut = debtOut
            , maxDebt = maxDebt
            , apr = apr
            , cdp = cdp
            }
                |> Success

        Err error ->
            Failure error


noCmd :
    { state : State
    , tooltip : Maybe Tooltip
    }
    -> ( Transaction, Cmd Msg )
noCmd transaction =
    ( transaction |> Transaction
    , Cmd.none
    )


noCmdAndEffect :
    { state : State
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
        { state : State
        , tooltip : Maybe Tooltip
        }
    -> ( Transaction, Cmd Msg )
initQuery =
    constructQuery queryBorrow


query :
    { model | slippage : Slippage }
    -> Blockchain
    -> Pool
    -> PoolInfo
    ->
        { state : State
        , tooltip : Maybe Tooltip
        }
    -> ( Transaction, Cmd Msg, Maybe Effect )
query model blockchain pool poolInfo transaction =
    transaction
        |> constructQuery queryBorrow
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
        { state : State
        , tooltip : Maybe Tooltip
        }
    -> ( Transaction, Cmd Msg, Maybe Effect )
queryPerSecond model blockchain pool poolInfo transaction =
    transaction
        |> constructQuery queryBorrowPerSecond
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
        { state : State
        , tooltip : Maybe Tooltip
        }
    -> ( Transaction, Cmd Msg )
constructQuery givenCmd { slippage } blockchain pool poolInfo transaction =
    (case transaction.state of
        Default default ->
            case
                ( default.assetOut |> Input.isZero
                , default.assetOut
                    |> Uint.fromAmount
                        (pool.pair |> Pair.toAsset)
                )
            of
                ( False, Just assetOut ) ->
                    { chainId = blockchain |> Blockchain.toChain
                    , pool = pool
                    , poolInfo = poolInfo
                    , assetOut = assetOut
                    , percent = Percent.init
                    , slippage = slippage
                    }
                        |> Query.givenPercent
                        |> Just

                _ ->
                    Nothing

        DefaultMax defaultMax ->
            case
                ( defaultMax.collateralOut |> Input.isZero
                , defaultMax.collateralOut
                    |> Uint.fromAmount
                        (pool.pair |> Pair.toCollateral)
                )
            of
                ( False, Just collateralOut ) ->
                    { chainId = blockchain |> Blockchain.toChain
                    , pool = pool
                    , poolInfo = poolInfo
                    , collateralOut = collateralOut
                    , percent = Percent.init
                    , slippage = slippage
                    }
                        |> Query.givenMax
                        |> Just

                _ ->
                    Nothing

        Slider slider ->
            case
                ( slider.assetOut |> Input.isZero
                , slider.assetOut
                    |> Uint.fromAmount
                        (pool.pair |> Pair.toAsset)
                )
            of
                ( False, Just assetOut ) ->
                    { chainId = blockchain |> Blockchain.toChain
                    , pool = pool
                    , poolInfo = poolInfo
                    , assetOut = assetOut
                    , percent = slider.percent
                    , slippage = slippage
                    }
                        |> Query.givenPercent
                        |> Just

                _ ->
                    Nothing

        Debt debt ->
            case
                ( (debt.assetOut |> Input.isZero)
                    || (debt.debtOut |> Input.isZero)
                , debt.assetOut
                    |> Uint.fromAmount
                        (pool.pair |> Pair.toAsset)
                , debt.debtOut
                    |> Uint.fromAmount
                        (pool.pair |> Pair.toAsset)
                )
            of
                ( False, Just assetOut, Just debtOut ) ->
                    { chainId = blockchain |> Blockchain.toChain
                    , pool = pool
                    , poolInfo = poolInfo
                    , assetOut = assetOut
                    , debtOut = debtOut
                    , slippage = slippage
                    }
                        |> Query.givenDebt
                        |> Just

                _ ->
                    Nothing

        Collateral collateral ->
            case
                ( (collateral.assetOut |> Input.isZero)
                    || (collateral.collateralOut |> Input.isZero)
                , collateral.assetOut
                    |> Uint.fromAmount
                        (pool.pair |> Pair.toAsset)
                , collateral.collateralOut
                    |> Uint.fromAmount
                        (pool.pair |> Pair.toCollateral)
                )
            of
                ( False, Just assetOut, Just collateralOut ) ->
                    { chainId = blockchain |> Blockchain.toChain
                    , pool = pool
                    , poolInfo = poolInfo
                    , assetOut = assetOut
                    , collateralOut = collateralOut
                    , slippage = slippage
                    }
                        |> Query.givenCollateral
                        |> Just

                _ ->
                    Nothing

        AdvancedMax advancedMax ->
            case
                ( advancedMax.collateralOut |> Input.isZero
                , advancedMax.collateralOut
                    |> Uint.fromAmount
                        (pool.pair |> Pair.toCollateral)
                )
            of
                ( False, Just collateralOut ) ->
                    { chainId = blockchain |> Blockchain.toChain
                    , pool = pool
                    , poolInfo = poolInfo
                    , collateralOut = collateralOut
                    , percent = advancedMax.percent
                    , slippage = slippage
                    }
                        |> Query.givenMax
                        |> Just

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


port queryBorrow : Value -> Cmd msg


port queryBorrowPerSecond : Value -> Cmd msg


port approveBorrow : Value -> Cmd msg


port borrow : Value -> Cmd msg


port receiveBorrowAnswer : (Value -> msg) -> Sub msg


subscriptions : Transaction -> Sub Msg
subscriptions (Transaction { state }) =
    if state |> hasInputZero then
        Sub.none

    else
        [ Time.every 1000 QueryAgain
        , receiveBorrowAnswer ReceiveAnswer
        ]
            |> Sub.batch


hasInputZero : State -> Bool
hasInputZero state =
    case state of
        Default { assetOut } ->
            assetOut |> Input.isZero

        DefaultMax { collateralOut } ->
            collateralOut |> Input.isZero

        Slider { assetOut } ->
            assetOut |> Input.isZero

        Debt { assetOut, debtOut } ->
            (assetOut |> Input.isZero)
                || (debtOut |> Input.isZero)

        Collateral { assetOut, collateralOut } ->
            (assetOut |> Input.isZero)
                || (collateralOut |> Input.isZero)

        AdvancedMax { collateralOut } ->
            collateralOut |> Input.isZero


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
            |> assetOutSection model
                (pool.pair |> Pair.toAsset)
    , second =
        transaction
            |> duesOutSection model blockchain pool
    , buttons = none
    }


assetOutSection :
    { model | images : Images }
    -> Token
    -> { transaction | state : State, tooltip : Maybe Tooltip }
    -> Element Msg
assetOutSection model asset { state, tooltip } =
    column
        [ Region.description "borrow asset"
        , width <| px 343
        , height shrink
        , padding 16
        , spacing 10
        , Background.color Color.light500
        , Border.rounded 8
        ]
        [ el
            [ width shrink
            , height shrink
            , Font.size 14
            ]
            (text "Amount to Borrow")
        , (case state of
            Default { assetOut } ->
                Left assetOut

            DefaultMax { out } ->
                case out of
                    Success { assetOut } ->
                        Right assetOut

                    _ ->
                        Left ""

            Slider { assetOut } ->
                Left assetOut

            Debt { assetOut } ->
                Left assetOut

            Collateral { assetOut } ->
                Left assetOut

            AdvancedMax { out } ->
                case out of
                    Success { assetOut } ->
                        Right assetOut

                    _ ->
                        Left ""
          )
            |> (\assetOut ->
                    Textbox.view model
                        { onMouseEnter = OnMouseEnter
                        , onMouseLeave = OnMouseLeave
                        , tooltip = Tooltip.AssetOutSymbol
                        , opened = tooltip
                        , token = asset
                        , onClick = Just ClickAssetOut
                        , onChange = InputAssetOut
                        , text = assetOut
                        , description = "asset in textbox"
                        }
               )
        ]


duesOutSection :
    { model | spot : Spot, images : Images }
    -> Blockchain
    -> Pool
    -> { transaction | state : State, tooltip : Maybe Tooltip }
    -> Element Msg
duesOutSection model blockchain pool ({ state, tooltip } as transaction) =
    column
        [ Region.description "claims"
        , width <| px 343
        , height shrink
        , padding 16
        , spacing 12
        , Background.color Color.primary100
        , Border.rounded 8
        ]
        [ (case state of
            Default _ ->
                Mode.Recommended

            DefaultMax _ ->
                Mode.Recommended

            _ ->
                Mode.Advanced
          )
            |> (\mode ->
                    Switch.view
                        { onChange = SwitchMode
                        , mode = mode
                        }
               )
        , (case state of
            Default _ ->
                Nothing

            DefaultMax _ ->
                Nothing

            Slider { percent } ->
                Just percent

            Debt { percent } ->
                Just percent

            Collateral { percent } ->
                Just percent

            AdvancedMax { percent } ->
                Just percent
          )
            |> Maybe.map
                (\percent ->
                    Slider.view
                        { onChange = Slide
                        , click = ClickSlider
                        , percent = percent
                        , learnMore =
                            Builder.crossOrigin
                                "https://timeswap.gitbook.io"
                                [ "timeswap"
                                , "deep-dive"
                                , "borrowing"
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
            ((case state of
                Default { dues } ->
                    case dues of
                        Success { apr, cdp } ->
                            ( apr, cdp ) |> Just

                        _ ->
                            Nothing

                DefaultMax { out } ->
                    case out of
                        Success { apr, cdp } ->
                            ( apr, cdp ) |> Just

                        _ ->
                            Nothing

                Slider { dues } ->
                    case dues of
                        Success { apr, cdp } ->
                            ( apr, cdp ) |> Just

                        _ ->
                            Nothing

                Debt { dues } ->
                    case dues of
                        Success { apr, cdp } ->
                            ( apr, cdp ) |> Just

                        _ ->
                            Nothing

                Collateral { dues } ->
                    case dues of
                        Success { apr, cdp } ->
                            ( apr, cdp ) |> Just

                        _ ->
                            Nothing

                AdvancedMax { out } ->
                    case out of
                        Success { apr, cdp } ->
                            ( apr, cdp ) |> Just

                        _ ->
                            Nothing
             )
                |> Maybe.map
                    (\( apr, cdp ) ->
                        [ Info.borrowAPR apr
                        , Info.borrowCDP model
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
        , case state of
            Default { dues } ->
                column
                    [ width fill
                    , height shrink
                    , spacing 12
                    ]
                    [ dues
                        |> Remote.map .debtOut
                        |> debtOutSection model
                            (pool.pair |> Pair.toAsset)
                            transaction
                    , dues
                        |> Remote.map .collateralOut
                        |> Right
                        |> collateralOutSection model
                            blockchain
                            (pool.pair |> Pair.toCollateral)
                            transaction
                    ]

            DefaultMax { collateralOut, out } ->
                column
                    [ width fill
                    , height shrink
                    , spacing 12
                    ]
                    [ out
                        |> Remote.map .debtOut
                        |> debtOutSection model
                            (pool.pair |> Pair.toAsset)
                            transaction
                    , collateralOut
                        |> Left
                        |> collateralOutSection model
                            blockchain
                            (pool.pair |> Pair.toCollateral)
                            transaction
                    ]

            Slider { dues } ->
                column
                    [ width fill
                    , height shrink
                    , spacing 12
                    ]
                    [ dues
                        |> Remote.map .debtOut
                        |> Right
                        |> advancedDebtOutSection model
                            (pool.pair |> Pair.toAsset)
                            transaction
                    , dues
                        |> Remote.map .collateralOut
                        |> Right
                        |> advancedCollateralOutSection model
                            blockchain
                            (pool.pair |> Pair.toCollateral)
                            transaction
                    ]

            Debt { debtOut, dues } ->
                column
                    [ width fill
                    , height shrink
                    , spacing 12
                    ]
                    [ debtOut
                        |> Left
                        |> advancedDebtOutSection model
                            (pool.pair |> Pair.toAsset)
                            transaction
                    , dues
                        |> Remote.map .collateralOut
                        |> Right
                        |> advancedCollateralOutSection model
                            blockchain
                            (pool.pair |> Pair.toCollateral)
                            transaction
                    ]

            Collateral { collateralOut, dues } ->
                column
                    [ width fill
                    , height shrink
                    , spacing 12
                    ]
                    [ dues
                        |> Remote.map .debtOut
                        |> Right
                        |> advancedDebtOutSection model
                            (pool.pair |> Pair.toAsset)
                            transaction
                    , collateralOut
                        |> Left
                        |> advancedCollateralOutSection model
                            blockchain
                            (pool.pair |> Pair.toCollateral)
                            transaction
                    ]

            AdvancedMax { collateralOut, out } ->
                column
                    [ width fill
                    , height shrink
                    , spacing 12
                    ]
                    [ out
                        |> Remote.map .debtOut
                        |> Right
                        |> advancedDebtOutSection model
                            (pool.pair |> Pair.toAsset)
                            transaction
                    , collateralOut
                        |> Left
                        |> advancedCollateralOutSection model
                            blockchain
                            (pool.pair |> Pair.toCollateral)
                            transaction
                    ]
        ]


debtOutSection :
    { model | images : Images }
    -> Token
    -> { transaction | tooltip : Maybe Tooltip }
    -> Remote Error Uint
    -> Element Msg
debtOutSection model asset { tooltip } output =
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
            (text "Debt to Repay")
        , Output.view model
            { onMouseEnter = OnMouseEnter
            , onMouseLeave = OnMouseLeave
            , tooltip = Tooltip.DebtOutSymbol
            , opened = tooltip
            , token = asset
            , output = output
            , description = "debt output"
            }
        ]


collateralOutSection :
    { model | images : Images }
    -> Blockchain
    -> Token
    -> { transaction | tooltip : Maybe Tooltip }
    -> Or String (Remote Error Uint)
    -> Element Msg
collateralOutSection model blockchain collateral { tooltip } or =
    column
        [ width fill
        , height shrink
        , spacing 10
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
                , Font.color Color.primary400
                ]
                (text "Collateral to Lock")
            , blockchain
                |> Blockchain.toUser
                |> Maybe.andThen (User.getBalance collateral)
                |> Maybe.map
                    (\balance ->
                        MaxButton.view
                            { onPress = InputMax
                            , onMouseEnter = OnMouseEnter
                            , onMouseLeave = OnMouseLeave
                            , tooltip = Tooltip.Balance
                            , opened = tooltip
                            , token = collateral
                            , balance = balance
                            }
                    )
                |> Maybe.withDefault none
            ]
        , case or of
            Left string ->
                Output.viewCollateral model
                    { onMouseEnter = OnMouseEnter
                    , onMouseLeave = OnMouseLeave
                    , tooltip = Tooltip.CollateralOutSymbol
                    , opened = tooltip
                    , token = collateral
                    , input = string
                    , description = "collateral output"
                    }

            Right output ->
                Output.view model
                    { onMouseEnter = OnMouseEnter
                    , onMouseLeave = OnMouseLeave
                    , tooltip = Tooltip.CollateralOutSymbol
                    , opened = tooltip
                    , token = collateral
                    , output = output
                    , description = "insurance output"
                    }
        ]


advancedDebtOutSection :
    { model | images : Images }
    -> Token
    -> { transaction | tooltip : Maybe Tooltip }
    -> Or String (Remote Error Uint)
    -> Element Msg
advancedDebtOutSection model asset { tooltip } or =
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
            (text "Debt to Repay")
        , Textbox.view model
            { onMouseEnter = OnMouseEnter
            , onMouseLeave = OnMouseLeave
            , tooltip = Tooltip.DebtOutSymbol
            , opened = tooltip
            , token = asset
            , onClick = Just ClickDebtOut
            , onChange = InputDebtOut
            , text =
                case or of
                    Left string ->
                        Left string

                    Right (Success uint) ->
                        Right uint

                    Right _ ->
                        Left ""
            , description = "debt output"
            }
        ]


advancedCollateralOutSection :
    { model | images : Images }
    -> Blockchain
    -> Token
    -> { transaction | tooltip : Maybe Tooltip }
    -> Or String (Remote Error Uint)
    -> Element Msg
advancedCollateralOutSection model blockchain collateral { tooltip } or =
    column
        [ width fill
        , height shrink
        , spacing 10
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
                , Font.color Color.primary400
                ]
                (text "Collateral to Lock")
            , blockchain
                |> Blockchain.toUser
                |> Maybe.andThen (User.getBalance collateral)
                |> Maybe.map
                    (\balance ->
                        MaxButton.view
                            { onPress = InputMax
                            , onMouseEnter = OnMouseEnter
                            , onMouseLeave = OnMouseLeave
                            , tooltip = Tooltip.Balance
                            , opened = tooltip
                            , token = collateral
                            , balance = balance
                            }
                    )
                |> Maybe.withDefault none
            ]
        , Textbox.view model
            { onMouseEnter = OnMouseEnter
            , onMouseLeave = OnMouseLeave
            , tooltip = Tooltip.CollateralOutSymbol
            , opened = tooltip
            , token = collateral
            , onClick = Just ClickCollateralOut
            , onChange = InputCollateralOut
            , text =
                case or of
                    Left string ->
                        Left string

                    Right (Success uint) ->
                        Right uint

                    Right _ ->
                        Left ""
            , description = "collateral output"
            }
        ]
