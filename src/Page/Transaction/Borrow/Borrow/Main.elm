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
import Blockchain.User.WriteBorrow as WriteBorrow exposing (WriteBorrow)
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
import Data.Theme exposing (Theme)
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
import Page.PoolInfo exposing (PoolInfo)
import Page.Transaction.Borrow.Borrow.Disabled as Disabled
import Page.Transaction.Borrow.Borrow.Error exposing (Error)
import Page.Transaction.Borrow.Borrow.Query as Query
import Page.Transaction.Borrow.Borrow.Tooltip as Tooltip exposing (Tooltip)
import Page.Transaction.Button as Button
import Page.Transaction.Info as Info
import Page.Transaction.MaxButton as MaxButton
import Page.Transaction.Output as Output
import Page.Transaction.Slider as Slider
import Page.Transaction.Switch as Switch
import Page.Transaction.Textbox as Textbox
import Time exposing (Posix)
import Url.Builder as Builder
import Utility.Color as Color
import Utility.Input as Input
import Utility.Loading as Loading


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
    { collateralIn : String
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
    , debtIn : String
    , dues : Remote Error DuesGivenDebt
    }


type alias CollateralInput =
    { assetOut : String
    , percent : Percent
    , collateralIn : String
    , dues : Remote Error DuesGivenCollateral
    }


type alias AdvancedMaxInput =
    { collateralIn : String
    , percent : Percent
    , out : Remote Error OutGivenMax
    }


type alias DuesGivenPercent =
    { debtIn : Uint
    , collateralIn : Uint
    , maxDebt : Uint
    , maxCollateral : Uint
    , apr : Float
    , cdp : CDP
    }


type alias OutGivenMax =
    { assetOut : Uint
    , debtIn : Uint
    , maxDebt : Uint
    , apr : Float
    , cdp : CDP
    }


type alias DuesGivenDebt =
    { collateralIn : Uint
    , maxCollateral : Uint
    , apr : Float
    , cdp : CDP
    }


type alias DuesGivenCollateral =
    { debtIn : Uint
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
    | ClickDebtIn
    | InputDebtIn String
    | ClickCollateralIn
    | InputCollateralIn String
    | InputMax
    | QueryAgain Posix
    | ClickConnect
    | ClickApprove
    | ClickBorrow
    | ReceiveAnswer Value
    | Tick Posix
    | OnMouseEnter Tooltip
    | OnMouseLeave


type Effect
    = OpenConnect
    | Approve ERC20
    | Borrow WriteBorrow


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
    { debtIn = Uint.zero
    , collateralIn = Uint.zero
    , maxDebt = Uint.zero
    , maxCollateral = Uint.zero
    , apr = 0
    , cdp = CDP.init
    }


initGivenMax : OutGivenMax
initGivenMax =
    { assetOut = Uint.zero
    , debtIn = Uint.zero
    , maxDebt = Uint.zero
    , apr = 0
    , cdp = CDP.init
    }


initGivenDebt : DuesGivenDebt
initGivenDebt =
    { collateralIn = Uint.zero
    , maxCollateral = Uint.zero
    , apr = 0
    , cdp = CDP.init
    }


initGivenCollateral : DuesGivenCollateral
initGivenCollateral =
    { debtIn = Uint.zero
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
                , dues = Remote.loading
                }
                    |> Default
                    |> Right

        Disabled.DefaultMax collateralIn ->
            if collateralIn |> Input.isZero then
                { collateralIn = collateralIn
                , out = initGivenMax |> Success
                }
                    |> DefaultMax
                    |> Left

            else
                { collateralIn = collateralIn
                , out = Remote.loading
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
                , dues = Remote.loading
                }
                    |> Slider
                    |> Right

        Disabled.Debt { assetOut, percent, debtIn } ->
            if
                (assetOut |> Input.isZero)
                    || (debtIn |> Input.isZero)
            then
                { assetOut = assetOut
                , percent = percent
                , debtIn = debtIn
                , dues = initGivenDebt |> Success
                }
                    |> Debt
                    |> Left

            else
                { assetOut = assetOut
                , percent = percent
                , debtIn = debtIn
                , dues = Remote.loading
                }
                    |> Debt
                    |> Right

        Disabled.Collateral { assetOut, percent, collateralIn } ->
            if
                (assetOut |> Input.isZero)
                    || (collateralIn |> Input.isZero)
            then
                { assetOut = assetOut
                , percent = percent
                , collateralIn = collateralIn
                , dues = initGivenCollateral |> Success
                }
                    |> Collateral
                    |> Left

            else
                { assetOut = assetOut
                , percent = percent
                , collateralIn = collateralIn
                , dues = Remote.loading
                }
                    |> Collateral
                    |> Right

        Disabled.AdvancedMax { collateralIn, percent } ->
            if collateralIn |> Input.isZero then
                { collateralIn = collateralIn
                , percent = percent
                , out = initGivenMax |> Success
                }
                    |> AdvancedMax
                    |> Left

            else
                { collateralIn = collateralIn
                , percent = percent
                , out = Remote.loading
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

        DefaultMax { collateralIn } ->
            Disabled.DefaultMax collateralIn

        Slider { assetOut, percent } ->
            { assetOut = assetOut
            , percent = percent
            }
                |> Disabled.Slider

        Debt { assetOut, percent, debtIn } ->
            { assetOut = assetOut
            , percent = percent
            , debtIn = debtIn
            }
                |> Disabled.Debt

        Collateral { assetOut, percent, collateralIn } ->
            { assetOut = assetOut
            , percent = percent
            , collateralIn = collateralIn
            }
                |> Disabled.Collateral

        AdvancedMax { collateralIn, percent } ->
            { collateralIn = collateralIn
            , percent = percent
            }
                |> Disabled.AdvancedMax


update :
    { model | slippage : Slippage }
    -> Blockchain
    -> Pool
    -> PoolInfo
    -> Msg
    -> Transaction
    -> ( Transaction, Cmd Msg, Maybe Effect )
update model blockchain pool poolInfo msg (Transaction transaction) =
    case ( msg, transaction.state ) of
        ( ClickAssetOut, DefaultMax { collateralIn, out } ) ->
            (case out of
                Success { assetOut } ->
                    if collateralIn |> Input.isZero then
                        ""

                    else
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

        ( ClickAssetOut, AdvancedMax { collateralIn, percent, out } ) ->
            (case out of
                Success { assetOut } ->
                    if collateralIn |> Input.isZero then
                        ""

                    else
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

        ( InputAssetOut assetOut, Debt { percent, debtIn } ) ->
            if assetOut |> Uint.isAmount (pool.pair |> Pair.toAsset) then
                { transaction
                    | state =
                        assetOut |> updateDebt percent debtIn
                }
                    |> query model blockchain pool poolInfo

            else
                transaction |> noCmdAndEffect

        ( InputAssetOut assetOut, Collateral { percent, collateralIn } ) ->
            if assetOut |> Uint.isAmount (pool.pair |> Pair.toAsset) then
                { transaction
                    | state =
                        assetOut |> updateCollateral percent collateralIn
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

        ( SwitchMode Mode.Advanced, DefaultMax { collateralIn } ) ->
            { transaction
                | state =
                    collateralIn |> updateAdvancedMax Percent.init
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

        ( SwitchMode Mode.Recommended, AdvancedMax { collateralIn, percent } ) ->
            { transaction
                | state =
                    collateralIn |> updateDefaultMax
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

        ( Slide float, AdvancedMax { collateralIn } ) ->
            { transaction
                | state =
                    collateralIn
                        |> updateAdvancedMax
                            (float |> Percent.fromFloat)
            }
                |> query model blockchain pool poolInfo

        ( ClickDebtIn, Slider { assetOut, percent, dues } ) ->
            (case dues of
                Success { debtIn } ->
                    if assetOut |> Input.isZero then
                        ""

                    else
                        debtIn
                            |> Uint.toAmount
                                (pool.pair |> Pair.toAsset)

                _ ->
                    ""
            )
                |> (\debtIn ->
                        { transaction
                            | state =
                                assetOut |> updateDebt percent debtIn
                        }
                            |> query model blockchain pool poolInfo
                   )

        ( ClickDebtIn, Collateral { assetOut, percent, dues } ) ->
            (case dues of
                Success { debtIn } ->
                    if assetOut |> Input.isZero then
                        ""

                    else
                        debtIn
                            |> Uint.toAmount
                                (pool.pair |> Pair.toAsset)

                _ ->
                    ""
            )
                |> (\debtIn ->
                        { transaction
                            | state =
                                assetOut |> updateDebt percent debtIn
                        }
                            |> query model blockchain pool poolInfo
                   )

        ( ClickDebtIn, AdvancedMax { collateralIn, percent, out } ) ->
            (case out of
                Success { assetOut, debtIn } ->
                    ( assetOut
                        |> Uint.toAmount
                            (pool.pair |> Pair.toAsset)
                    , debtIn
                        |> Uint.toAmount
                            (pool.pair |> Pair.toAsset)
                    )

                _ ->
                    ( "", "" )
            )
                |> (\( assetOut, debtIn ) ->
                        { transaction
                            | state =
                                (if collateralIn |> Input.isZero then
                                    ""

                                 else
                                    assetOut
                                )
                                    |> updateDebt percent debtIn
                        }
                            |> query model blockchain pool poolInfo
                   )

        ( InputDebtIn debtIn, Slider { assetOut, percent } ) ->
            if debtIn |> Uint.isAmount (pool.pair |> Pair.toAsset) then
                { transaction
                    | state =
                        assetOut |> updateDebt percent debtIn
                }
                    |> query model blockchain pool poolInfo

            else
                transaction |> noCmdAndEffect

        ( InputDebtIn debtIn, Debt { assetOut, percent } ) ->
            if debtIn |> Uint.isAmount (pool.pair |> Pair.toAsset) then
                { transaction
                    | state =
                        assetOut |> updateDebt percent debtIn
                }
                    |> query model blockchain pool poolInfo

            else
                transaction |> noCmdAndEffect

        ( InputDebtIn debtIn, Collateral { assetOut, percent } ) ->
            if debtIn |> Uint.isAmount (pool.pair |> Pair.toAsset) then
                { transaction
                    | state =
                        assetOut |> updateDebt percent debtIn
                }
                    |> query model blockchain pool poolInfo

            else
                transaction |> noCmdAndEffect

        ( InputDebtIn debtIn, AdvancedMax { percent, out } ) ->
            if debtIn |> Uint.isAmount (pool.pair |> Pair.toAsset) then
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
                                    assetOut |> updateDebt percent debtIn
                            }
                                |> query model blockchain pool poolInfo
                       )

            else
                transaction |> noCmdAndEffect

        ( ClickCollateralIn, Slider { assetOut, percent, dues } ) ->
            (case dues of
                Success { collateralIn } ->
                    if assetOut |> Input.isZero then
                        ""

                    else
                        collateralIn
                            |> Uint.toAmount
                                (pool.pair |> Pair.toCollateral)

                _ ->
                    ""
            )
                |> (\collateralIn ->
                        { transaction
                            | state =
                                assetOut |> updateCollateral percent collateralIn
                        }
                            |> query model blockchain pool poolInfo
                   )

        ( ClickCollateralIn, Debt { assetOut, percent, dues } ) ->
            (case dues of
                Success { collateralIn } ->
                    if assetOut |> Input.isZero then
                        ""

                    else
                        collateralIn
                            |> Uint.toAmount
                                (pool.pair |> Pair.toCollateral)

                _ ->
                    ""
            )
                |> (\collateralIn ->
                        { transaction
                            | state =
                                assetOut |> updateCollateral percent collateralIn
                        }
                            |> query model blockchain pool poolInfo
                   )

        ( ClickCollateralIn, AdvancedMax { collateralIn, percent, out } ) ->
            (case out of
                Success { assetOut } ->
                    if collateralIn |> Input.isZero then
                        ""

                    else
                        assetOut
                            |> Uint.toAmount
                                (pool.pair |> Pair.toAsset)

                _ ->
                    ""
            )
                |> (\assetOut ->
                        { transaction
                            | state =
                                assetOut |> updateCollateral percent collateralIn
                        }
                            |> query model blockchain pool poolInfo
                   )

        ( InputCollateralIn collateralIn, Slider { assetOut, percent } ) ->
            if collateralIn |> Uint.isAmount (pool.pair |> Pair.toCollateral) then
                { transaction
                    | state =
                        assetOut |> updateCollateral percent collateralIn
                }
                    |> query model blockchain pool poolInfo

            else
                transaction |> noCmdAndEffect

        ( InputCollateralIn collateralIn, Debt { assetOut, percent } ) ->
            if collateralIn |> Uint.isAmount (pool.pair |> Pair.toCollateral) then
                { transaction
                    | state =
                        assetOut |> updateCollateral percent collateralIn
                }
                    |> query model blockchain pool poolInfo

            else
                transaction |> noCmdAndEffect

        ( InputCollateralIn collateralIn, Collateral { assetOut, percent } ) ->
            if collateralIn |> Uint.isAmount (pool.pair |> Pair.toCollateral) then
                { transaction
                    | state =
                        assetOut |> updateCollateral percent collateralIn
                }
                    |> query model blockchain pool poolInfo

            else
                transaction |> noCmdAndEffect

        ( InputCollateralIn collateralIn, AdvancedMax { percent, out } ) ->
            if collateralIn |> Uint.isAmount (pool.pair |> Pair.toCollateral) then
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
                                    assetOut |> updateCollateral percent collateralIn
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
                    (\collateralIn ->
                        { transaction
                            | state =
                                collateralIn |> updateDefaultMax
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
                    (\collateralIn ->
                        { transaction
                            | state =
                                collateralIn |> updateDefaultMax
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
                    (\collateralIn ->
                        { transaction
                            | state =
                                collateralIn |> updateAdvancedMax percent
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
                    (\collateralIn ->
                        { transaction
                            | state =
                                collateralIn |> updateAdvancedMax percent
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
                    (\collateralIn ->
                        { transaction
                            | state =
                                collateralIn |> updateAdvancedMax percent
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
                    (\collateralIn ->
                        { transaction
                            | state =
                                collateralIn |> updateAdvancedMax percent
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
                            Success { collateralIn } ->
                                collateralIn |> Just

                            _ ->
                                Nothing

                    DefaultMax { collateralIn } ->
                        collateralIn
                            |> Uint.fromAmount
                                (pool.pair |> Pair.toCollateral)

                    Slider { dues } ->
                        case dues of
                            Success { collateralIn } ->
                                collateralIn |> Just

                            _ ->
                                Nothing

                    Debt { dues } ->
                        case dues of
                            Success { collateralIn } ->
                                collateralIn |> Just

                            _ ->
                                Nothing

                    Collateral { collateralIn } ->
                        collateralIn
                            |> Uint.fromAmount
                                (pool.pair |> Pair.toCollateral)

                    AdvancedMax { collateralIn } ->
                        collateralIn
                            |> Uint.fromAmount
                                (pool.pair |> Pair.toCollateral)
                , pool.pair
                    |> Pair.toCollateral
                    |> Token.toERC20
                )
             of
                ( Just user, Just collateralIn, Just erc20 ) ->
                    if
                        (user
                            |> User.hasEnoughBalance
                                (pool.pair |> Pair.toCollateral)
                                collateralIn
                        )
                            && (user
                                    |> User.hasEnoughAllowance
                                        erc20
                                        collateralIn
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

        ( ClickBorrow, Default default ) ->
            (case default.dues of
                Success answer ->
                    case
                        ( blockchain |> Blockchain.toUser
                        , default.assetOut
                            |> Uint.fromAmount
                                (pool.pair |> Pair.toAsset)
                        )
                    of
                        ( Just user, Just assetOut ) ->
                            if
                                (user
                                    |> User.hasEnoughBalance
                                        (pool.pair |> Pair.toCollateral)
                                        answer.collateralIn
                                )
                                    && (pool.pair
                                            |> Pair.toCollateral
                                            |> Token.toERC20
                                            |> Maybe.map
                                                (\erc20 ->
                                                    user
                                                        |> User.hasEnoughAllowance
                                                            erc20
                                                            answer.collateralIn
                                                )
                                            |> Maybe.withDefault True
                                       )
                            then
                                ( transaction |> Transaction
                                , Cmd.none
                                , { pool = pool
                                  , assetOut = assetOut
                                  , percent = Percent.init
                                  , maxDebt = answer.maxDebt
                                  , maxCollateral = answer.maxCollateral
                                  }
                                    |> WriteBorrow.GivenPercent
                                    |> Borrow
                                    |> Just
                                )
                                    |> Just

                            else
                                Nothing

                        _ ->
                            Nothing

                _ ->
                    Nothing
            )
                |> Maybe.withDefault (transaction |> noCmdAndEffect)

        ( ClickBorrow, DefaultMax defaultMax ) ->
            (case defaultMax.out of
                Success answer ->
                    case
                        ( blockchain |> Blockchain.toUser
                        , defaultMax.collateralIn
                            |> Uint.fromAmount
                                (pool.pair |> Pair.toCollateral)
                        )
                    of
                        ( Just user, Just collateralIn ) ->
                            if
                                (user
                                    |> User.hasEnoughBalance
                                        (pool.pair |> Pair.toCollateral)
                                        collateralIn
                                )
                                    && (pool.pair
                                            |> Pair.toCollateral
                                            |> Token.toERC20
                                            |> Maybe.map
                                                (\erc20 ->
                                                    user
                                                        |> User.hasEnoughAllowance
                                                            erc20
                                                            collateralIn
                                                )
                                            |> Maybe.withDefault True
                                       )
                            then
                                ( transaction |> Transaction
                                , Cmd.none
                                , { pool = pool
                                  , assetOut = answer.assetOut
                                  , collateralIn = collateralIn
                                  , maxDebt = answer.maxDebt
                                  }
                                    |> WriteBorrow.GivenCollateral
                                    |> Borrow
                                    |> Just
                                )
                                    |> Just

                            else
                                Nothing

                        _ ->
                            Nothing

                _ ->
                    Nothing
            )
                |> Maybe.withDefault (transaction |> noCmdAndEffect)

        ( ClickBorrow, Slider slider ) ->
            (case slider.dues of
                Success answer ->
                    case
                        ( blockchain |> Blockchain.toUser
                        , slider.assetOut
                            |> Uint.fromAmount
                                (pool.pair |> Pair.toAsset)
                        )
                    of
                        ( Just user, Just assetOut ) ->
                            if
                                (user
                                    |> User.hasEnoughBalance
                                        (pool.pair |> Pair.toCollateral)
                                        answer.collateralIn
                                )
                                    && (pool.pair
                                            |> Pair.toCollateral
                                            |> Token.toERC20
                                            |> Maybe.map
                                                (\erc20 ->
                                                    user
                                                        |> User.hasEnoughAllowance
                                                            erc20
                                                            answer.collateralIn
                                                )
                                            |> Maybe.withDefault True
                                       )
                            then
                                ( transaction |> Transaction
                                , Cmd.none
                                , { pool = pool
                                  , assetOut = assetOut
                                  , percent = slider.percent
                                  , maxDebt = answer.maxDebt
                                  , maxCollateral = answer.maxCollateral
                                  }
                                    |> WriteBorrow.GivenPercent
                                    |> Borrow
                                    |> Just
                                )
                                    |> Just

                            else
                                Nothing

                        _ ->
                            Nothing

                _ ->
                    Nothing
            )
                |> Maybe.withDefault (transaction |> noCmdAndEffect)

        ( ClickBorrow, Debt debt ) ->
            (case debt.dues of
                Success answer ->
                    case
                        ( blockchain |> Blockchain.toUser
                        , debt.assetOut
                            |> Uint.fromAmount
                                (pool.pair |> Pair.toAsset)
                        , debt.debtIn
                            |> Uint.fromAmount
                                (pool.pair |> Pair.toAsset)
                        )
                    of
                        ( Just user, Just assetOut, Just debtIn ) ->
                            if
                                (user
                                    |> User.hasEnoughBalance
                                        (pool.pair |> Pair.toCollateral)
                                        answer.collateralIn
                                )
                                    && (pool.pair
                                            |> Pair.toCollateral
                                            |> Token.toERC20
                                            |> Maybe.map
                                                (\erc20 ->
                                                    user
                                                        |> User.hasEnoughAllowance
                                                            erc20
                                                            answer.collateralIn
                                                )
                                            |> Maybe.withDefault True
                                       )
                            then
                                ( transaction |> Transaction
                                , Cmd.none
                                , { pool = pool
                                  , assetOut = assetOut
                                  , debtIn = debtIn
                                  , maxCollateral = answer.maxCollateral
                                  }
                                    |> WriteBorrow.GivenDebt
                                    |> Borrow
                                    |> Just
                                )
                                    |> Just

                            else
                                Nothing

                        _ ->
                            Nothing

                _ ->
                    Nothing
            )
                |> Maybe.withDefault (transaction |> noCmdAndEffect)

        ( ClickBorrow, Collateral collateral ) ->
            (case collateral.dues of
                Success answer ->
                    case
                        ( blockchain |> Blockchain.toUser
                        , collateral.assetOut
                            |> Uint.fromAmount
                                (pool.pair |> Pair.toAsset)
                        , collateral.collateralIn
                            |> Uint.fromAmount
                                (pool.pair |> Pair.toCollateral)
                        )
                    of
                        ( Just user, Just assetOut, Just collateralIn ) ->
                            if
                                (user
                                    |> User.hasEnoughBalance
                                        (pool.pair |> Pair.toCollateral)
                                        collateralIn
                                )
                                    && (pool.pair
                                            |> Pair.toCollateral
                                            |> Token.toERC20
                                            |> Maybe.map
                                                (\erc20 ->
                                                    user
                                                        |> User.hasEnoughAllowance
                                                            erc20
                                                            collateralIn
                                                )
                                            |> Maybe.withDefault True
                                       )
                            then
                                ( transaction |> Transaction
                                , Cmd.none
                                , { pool = pool
                                  , assetOut = assetOut
                                  , collateralIn = collateralIn
                                  , maxDebt = answer.maxDebt
                                  }
                                    |> WriteBorrow.GivenCollateral
                                    |> Borrow
                                    |> Just
                                )
                                    |> Just

                            else
                                Nothing

                        _ ->
                            Nothing

                _ ->
                    Nothing
            )
                |> Maybe.withDefault (transaction |> noCmdAndEffect)

        ( ClickBorrow, AdvancedMax advancedMax ) ->
            (case advancedMax.out of
                Success answer ->
                    case
                        ( blockchain |> Blockchain.toUser
                        , advancedMax.collateralIn
                            |> Uint.fromAmount
                                (pool.pair |> Pair.toCollateral)
                        )
                    of
                        ( Just user, Just collateralIn ) ->
                            if
                                (user
                                    |> User.hasEnoughBalance
                                        (pool.pair |> Pair.toCollateral)
                                        collateralIn
                                )
                                    && (pool.pair
                                            |> Pair.toCollateral
                                            |> Token.toERC20
                                            |> Maybe.map
                                                (\erc20 ->
                                                    user
                                                        |> User.hasEnoughAllowance
                                                            erc20
                                                            collateralIn
                                                )
                                            |> Maybe.withDefault True
                                       )
                            then
                                ( transaction |> Transaction
                                , Cmd.none
                                , { pool = pool
                                  , assetOut = answer.assetOut
                                  , collateralIn = collateralIn
                                  , maxDebt = answer.maxDebt
                                  }
                                    |> WriteBorrow.GivenCollateral
                                    |> Borrow
                                    |> Just
                                )
                                    |> Just

                            else
                                Nothing

                        _ ->
                            Nothing

                _ ->
                    Nothing
            )
                |> Maybe.withDefault (transaction |> noCmdAndEffect)

        ( ReceiveAnswer value, Default default ) ->
            (case value |> Decode.decodeValue Query.decoder of
                Ok (Query.GivenPercent answer) ->
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
            (case value |> Decode.decodeValue Query.decoder of
                Ok (Query.GivenMax answer) ->
                    if
                        (answer.chainId == (blockchain |> Blockchain.toChain))
                            && (answer.pool == pool)
                            && (answer.poolInfo == poolInfo)
                            && (Just answer.collateralIn
                                    == (defaultMax.collateralIn
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
            (case value |> Decode.decodeValue Query.decoder of
                Ok (Query.GivenPercent answer) ->
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
            (case value |> Decode.decodeValue Query.decoder of
                Ok (Query.GivenDebt answer) ->
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
                            && (Just answer.debtIn
                                    == (debt.debtIn
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
            (case value |> Decode.decodeValue Query.decoder of
                Ok (Query.GivenCollateral answer) ->
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
                            && (Just answer.collateralIn
                                    == (collateral.collateralIn
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
            (case value |> Decode.decodeValue Query.decoder of
                Ok (Query.GivenMax answer) ->
                    if
                        (answer.chainId == (blockchain |> Blockchain.toChain))
                            && (answer.pool == pool)
                            && (answer.poolInfo == poolInfo)
                            && (Just answer.collateralIn
                                    == (advancedMax.collateralIn
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

        ( Tick posix, Default default ) ->
            { transaction
                | state =
                    { default
                        | dues =
                            default.dues
                                |> Remote.update posix
                    }
                        |> Default
            }
                |> noCmdAndEffect

        ( Tick posix, DefaultMax defaultMax ) ->
            { transaction
                | state =
                    { defaultMax
                        | out =
                            defaultMax.out
                                |> Remote.update posix
                    }
                        |> DefaultMax
            }
                |> noCmdAndEffect

        ( Tick posix, Slider slider ) ->
            { transaction
                | state =
                    { slider
                        | dues =
                            slider.dues
                                |> Remote.update posix
                    }
                        |> Slider
            }
                |> noCmdAndEffect

        ( Tick posix, Debt debt ) ->
            { transaction
                | state =
                    { debt
                        | dues =
                            debt.dues
                                |> Remote.update posix
                    }
                        |> Debt
            }
                |> noCmdAndEffect

        ( Tick posix, Collateral collateral ) ->
            { transaction
                | state =
                    { collateral
                        | dues =
                            collateral.dues
                                |> Remote.update posix
                    }
                        |> Collateral
            }
                |> noCmdAndEffect

        ( Tick posix, AdvancedMax advancedMax ) ->
            { transaction
                | state =
                    { advancedMax
                        | out =
                            advancedMax.out
                                |> Remote.update posix
                    }
                        |> AdvancedMax
            }
                |> noCmdAndEffect

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
            Remote.loading
    }
        |> Default


updateDefaultMax : String -> State
updateDefaultMax collateralIn =
    { collateralIn = collateralIn
    , out =
        if collateralIn |> Input.isZero then
            initGivenMax
                |> Success

        else
            Remote.loading
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
            Remote.loading
    }
        |> Slider


updateDebt : Percent -> String -> String -> State
updateDebt percent debtIn assetOut =
    { assetOut = assetOut
    , percent = percent
    , debtIn = debtIn
    , dues =
        if
            (assetOut |> Input.isZero)
                || (debtIn |> Input.isZero)
        then
            initGivenDebt
                |> Success

        else
            Remote.loading
    }
        |> Debt


updateCollateral : Percent -> String -> String -> State
updateCollateral percent collateralIn assetOut =
    { assetOut = assetOut
    , percent = percent
    , collateralIn = collateralIn
    , dues =
        if
            (assetOut |> Input.isZero)
                || (collateralIn |> Input.isZero)
        then
            initGivenCollateral
                |> Success

        else
            Remote.loading
    }
        |> Collateral


updateAdvancedMax : Percent -> String -> State
updateAdvancedMax percent collateralIn =
    { collateralIn = collateralIn
    , percent = percent
    , out =
        if collateralIn |> Input.isZero then
            initGivenMax
                |> Success

        else
            Remote.loading
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
    Result Error Query.ResultDebt
    -> Remote Error DuesGivenDebt
toStateGivenDebt result =
    case result of
        Ok { collateralIn, maxCollateral, apr, cdp } ->
            { collateralIn = collateralIn
            , maxCollateral = maxCollateral
            , apr = apr
            , cdp = cdp
            }
                |> Success

        Err error ->
            Failure error


toStateGivenCollateral :
    Result Error Query.ResultCollateral
    -> Remote Error DuesGivenCollateral
toStateGivenCollateral result =
    case result of
        Ok { debtIn, maxDebt, apr, cdp } ->
            { debtIn = debtIn
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
                ( defaultMax.collateralIn |> Input.isZero
                , defaultMax.collateralIn
                    |> Uint.fromAmount
                        (pool.pair |> Pair.toCollateral)
                )
            of
                ( False, Just collateralIn ) ->
                    { chainId = blockchain |> Blockchain.toChain
                    , pool = pool
                    , poolInfo = poolInfo
                    , collateralIn = collateralIn
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
                    || (debt.debtIn |> Input.isZero)
                , debt.assetOut
                    |> Uint.fromAmount
                        (pool.pair |> Pair.toAsset)
                , debt.debtIn
                    |> Uint.fromAmount
                        (pool.pair |> Pair.toAsset)
                )
            of
                ( False, Just assetOut, Just debtIn ) ->
                    { chainId = blockchain |> Blockchain.toChain
                    , pool = pool
                    , poolInfo = poolInfo
                    , assetOut = assetOut
                    , debtIn = debtIn
                    , slippage = slippage
                    }
                        |> Query.givenDebt
                        |> Just

                _ ->
                    Nothing

        Collateral collateral ->
            case
                ( (collateral.assetOut |> Input.isZero)
                    || (collateral.collateralIn |> Input.isZero)
                , collateral.assetOut
                    |> Uint.fromAmount
                        (pool.pair |> Pair.toAsset)
                , collateral.collateralIn
                    |> Uint.fromAmount
                        (pool.pair |> Pair.toCollateral)
                )
            of
                ( False, Just assetOut, Just collateralIn ) ->
                    { chainId = blockchain |> Blockchain.toChain
                    , pool = pool
                    , poolInfo = poolInfo
                    , assetOut = assetOut
                    , collateralIn = collateralIn
                    , slippage = slippage
                    }
                        |> Query.givenCollateral
                        |> Just

                _ ->
                    Nothing

        AdvancedMax advancedMax ->
            case
                ( advancedMax.collateralIn |> Input.isZero
                , advancedMax.collateralIn
                    |> Uint.fromAmount
                        (pool.pair |> Pair.toCollateral)
                )
            of
                ( False, Just collateralIn ) ->
                    { chainId = blockchain |> Blockchain.toChain
                    , pool = pool
                    , poolInfo = poolInfo
                    , collateralIn = collateralIn
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


port receiveBorrowAnswer : (Value -> msg) -> Sub msg


subscriptions : Transaction -> Sub Msg
subscriptions (Transaction { state }) =
    [ if state |> hasInputZero then
        Sub.none

      else
        [ Time.every 1000 QueryAgain
        , receiveBorrowAnswer ReceiveAnswer
        ]
            |> Sub.batch
    , case state of
        Default { dues } ->
            dues |> Remote.subscriptions Tick

        DefaultMax { out } ->
            out |> Remote.subscriptions Tick

        Slider { dues } ->
            dues |> Remote.subscriptions Tick

        Debt { dues } ->
            dues |> Remote.subscriptions Tick

        Collateral { dues } ->
            dues |> Remote.subscriptions Tick

        AdvancedMax { out } ->
            out |> Remote.subscriptions Tick
    ]
        |> Sub.batch


hasInputZero : State -> Bool
hasInputZero state =
    case state of
        Default { assetOut } ->
            assetOut |> Input.isZero

        DefaultMax { collateralIn } ->
            collateralIn |> Input.isZero

        Slider { assetOut } ->
            assetOut |> Input.isZero

        Debt { assetOut, debtIn } ->
            (assetOut |> Input.isZero)
                || (debtIn |> Input.isZero)

        Collateral { assetOut, collateralIn } ->
            (assetOut |> Input.isZero)
                || (collateralIn |> Input.isZero)

        AdvancedMax { collateralIn } ->
            collateralIn |> Input.isZero


view :
    { model | priceFeed : PriceFeed, images : Images, theme : Theme }
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
            |> duesInSection model blockchain pool
    , buttons = buttons model.theme blockchain
    }


assetOutSection :
    { model | images : Images, theme : Theme }
    -> Token
    -> { transaction | state : State, tooltip : Maybe Tooltip }
    -> Element Msg
assetOutSection model asset { state, tooltip } =
    column
        [ Region.description "borrow asset"
        , width fill
        , height shrink
        , padding 16
        , spacing 10
        , Background.color Color.primary100
        , Border.rounded 8
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
                , Font.color Color.primary400
                ]
                (text "Amount to Borrow")
            , (case state of
                DefaultMax { out } ->
                    case out of
                        Loading timeline ->
                            Just timeline

                        _ ->
                            Nothing

                AdvancedMax { out } ->
                    case out of
                        Loading timeline ->
                            Just timeline

                        _ ->
                            Nothing

                _ ->
                    Nothing
              )
                |> Maybe.map
                    (\timeline ->
                        el
                            [ width shrink
                            , height shrink
                            , centerY
                            ]
                            (Loading.view timeline)
                    )
                |> Maybe.withDefault none
            ]
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
                        , description = "asset out textbox"
                        }
               )
        ]


duesInSection :
    { model | priceFeed : PriceFeed, images : Images, theme : Theme }
    -> Blockchain
    -> Pool
    -> { transaction | state : State, tooltip : Maybe Tooltip }
    -> Element Msg
duesInSection model blockchain pool ({ state, tooltip } as transaction) =
    column
        [ Region.description "claims"
        , width fill
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
                        , theme = model.theme
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
                        , min = 8
                        , max = 120
                        , theme = model.theme
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
                    ( dues |> Remote.map .apr
                    , dues |> Remote.map .cdp
                    )

                DefaultMax { out } ->
                    ( out |> Remote.map .apr
                    , out |> Remote.map .cdp
                    )

                Slider { dues } ->
                    ( dues |> Remote.map .apr
                    , dues |> Remote.map .cdp
                    )

                Debt { dues } ->
                    ( dues |> Remote.map .apr
                    , dues |> Remote.map .cdp
                    )

                Collateral { dues } ->
                    ( dues |> Remote.map .apr
                    , dues |> Remote.map .cdp
                    )

                AdvancedMax { out } ->
                    ( out |> Remote.map .apr
                    , out |> Remote.map .cdp
                    )
             )
                |> (\( apr, cdp ) ->
                        [ Info.borrowAPR apr model.theme
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
            )
        , case state of
            Default { dues } ->
                column
                    [ width fill
                    , height shrink
                    , spacing 12
                    ]
                    [ dues
                        |> Remote.map .debtIn
                        |> debtInSection model
                            (pool.pair |> Pair.toAsset)
                            transaction
                    , dues
                        |> Remote.map .collateralIn
                        |> Right
                        |> collateralInSection model
                            blockchain
                            (pool.pair |> Pair.toCollateral)
                            transaction
                    ]

            DefaultMax { collateralIn, out } ->
                column
                    [ width fill
                    , height shrink
                    , spacing 12
                    ]
                    [ out
                        |> Remote.map .debtIn
                        |> debtInSection model
                            (pool.pair |> Pair.toAsset)
                            transaction
                    , collateralIn
                        |> Left
                        |> collateralInSection model
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
                        |> Remote.map .debtIn
                        |> Right
                        |> advancedDebtInSection model
                            (pool.pair |> Pair.toAsset)
                            transaction
                    , dues
                        |> Remote.map .collateralIn
                        |> Right
                        |> advancedCollateralInSection model
                            blockchain
                            (pool.pair |> Pair.toCollateral)
                            transaction
                    ]

            Debt { debtIn, dues } ->
                column
                    [ width fill
                    , height shrink
                    , spacing 12
                    ]
                    [ debtIn
                        |> Left
                        |> advancedDebtInSection model
                            (pool.pair |> Pair.toAsset)
                            transaction
                    , dues
                        |> Remote.map .collateralIn
                        |> Right
                        |> advancedCollateralInSection model
                            blockchain
                            (pool.pair |> Pair.toCollateral)
                            transaction
                    ]

            Collateral { collateralIn, dues } ->
                column
                    [ width fill
                    , height shrink
                    , spacing 12
                    ]
                    [ dues
                        |> Remote.map .debtIn
                        |> Right
                        |> advancedDebtInSection model
                            (pool.pair |> Pair.toAsset)
                            transaction
                    , collateralIn
                        |> Left
                        |> advancedCollateralInSection model
                            blockchain
                            (pool.pair |> Pair.toCollateral)
                            transaction
                    ]

            AdvancedMax { collateralIn, out } ->
                column
                    [ width fill
                    , height shrink
                    , spacing 12
                    ]
                    [ out
                        |> Remote.map .debtIn
                        |> Right
                        |> advancedDebtInSection model
                            (pool.pair |> Pair.toAsset)
                            transaction
                    , collateralIn
                        |> Left
                        |> advancedCollateralInSection model
                            blockchain
                            (pool.pair |> Pair.toCollateral)
                            transaction
                    ]
        ]


debtInSection :
    { model | images : Images, theme : Theme }
    -> Token
    -> { transaction | tooltip : Maybe Tooltip }
    -> Remote Error Uint
    -> Element Msg
debtInSection model asset { tooltip } output =
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
                , Font.color Color.primary400
                ]
                (text "Debt to Repay")
            , case output of
                Loading timeline ->
                    el
                        [ width shrink
                        , height shrink
                        , centerY
                        ]
                        (Loading.view timeline)

                _ ->
                    none
            ]
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


collateralInSection :
    { model | images : Images, theme : Theme }
    -> Blockchain
    -> Token
    -> { transaction | tooltip : Maybe Tooltip }
    -> Or String (Remote Error Uint)
    -> Element Msg
collateralInSection model blockchain collateral { tooltip } or =
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
                    , Font.color Color.primary400
                    ]
                    (text "Collateral to Lock")
                , case or of
                    Right (Loading timeline) ->
                        el
                            [ width shrink
                            , height shrink
                            , centerY
                            ]
                            (Loading.view timeline)

                    _ ->
                        none
                ]
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
                            , theme = model.theme
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


advancedDebtInSection :
    { model | images : Images, theme : Theme }
    -> Token
    -> { transaction | tooltip : Maybe Tooltip }
    -> Or String (Remote Error Uint)
    -> Element Msg
advancedDebtInSection model asset { tooltip } or =
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
                , Font.color Color.primary400
                ]
                (text "Debt to Repay")
            , case or of
                Right (Loading timeline) ->
                    el
                        [ width shrink
                        , height shrink
                        , centerY
                        ]
                        (Loading.view timeline)

                _ ->
                    none
            ]
        , Textbox.view model
            { onMouseEnter = OnMouseEnter
            , onMouseLeave = OnMouseLeave
            , tooltip = Tooltip.DebtOutSymbol
            , opened = tooltip
            , token = asset
            , onClick = Just ClickDebtIn
            , onChange = InputDebtIn
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


advancedCollateralInSection :
    { model | images : Images, theme : Theme }
    -> Blockchain
    -> Token
    -> { transaction | tooltip : Maybe Tooltip }
    -> Or String (Remote Error Uint)
    -> Element Msg
advancedCollateralInSection model blockchain collateral { tooltip } or =
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
                    , Font.color Color.primary400
                    ]
                    (text "Collateral to Lock")
                , case or of
                    Right (Loading timeline) ->
                        el
                            [ width shrink
                            , height shrink
                            , centerY
                            ]
                            (Loading.view timeline)

                    _ ->
                        none
                ]
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
                            , theme = model.theme
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
            , onClick = Just ClickCollateralIn
            , onChange = InputCollateralIn
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


buttons : Theme -> Blockchain -> Element Msg
buttons theme blockchain =
    column
        [ width fill
        , height shrink
        , spacing 12
        ]
        (blockchain
            |> Blockchain.toUser
            |> Maybe.map
                (\_ -> [])
            |> Maybe.withDefault
                [ Button.connect theme ClickConnect ]
        )
