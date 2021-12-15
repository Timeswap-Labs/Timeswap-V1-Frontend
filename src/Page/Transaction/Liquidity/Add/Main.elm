port module Page.Transaction.Liquidity.Add.Main exposing
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
import Data.Or exposing (Or(..))
import Data.Pair as Pair
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
import Page.Transaction.Info as Info
import Page.Transaction.Liquidity.Add.Answer as Answer
import Page.Transaction.Liquidity.Add.Disabled as Disabled
import Page.Transaction.Liquidity.Add.Error exposing (Error)
import Page.Transaction.Liquidity.Add.Query as Query
import Page.Transaction.Liquidity.Add.Tooltip as Tooltip exposing (Tooltip)
import Page.Transaction.MaxButton as MaxButton
import Page.Transaction.Output as Output
import Page.Transaction.PoolInfo exposing (PoolInfo)
import Page.Transaction.Textbox as Textbox
import Time exposing (Posix)
import Utility.Color as Color
import Utility.Input as Input


type Transaction
    = Transaction
        { state : State
        , tooltip : Maybe Tooltip
        }


type State
    = Asset AssetInput
    | Debt DebtInput
    | Collateral CollateralInput


type alias AssetInput =
    { assetIn : String
    , out : Remote Error OutGivenAsset
    }


type alias DebtInput =
    { debtOut : String
    , out : Remote Error OutGivenDebt
    }


type alias CollateralInput =
    { collateralOut : String
    , out : Remote Error OutGivenCollateral
    }


type alias OutGivenAsset =
    { debtOut : Uint
    , collateralOut : Uint
    , liquidityOut : Uint
    , maxDebt : Uint
    , maxCollateral : Uint
    , minLiquidity : Uint
    , apr : Float
    , cdp : CDP
    }


type alias OutGivenDebt =
    { assetIn : Uint
    , collateralOut : Uint
    , liquidityOut : Uint
    , maxDebt : Uint
    , maxCollateral : Uint
    , minLiquidity : Uint
    , apr : Float
    , cdp : CDP
    }


type alias OutGivenCollateral =
    { assetIn : Uint
    , debtOut : Uint
    , liquidityOut : Uint
    , maxAsset : Uint
    , maxDebt : Uint
    , minLiquidity : Uint
    , apr : Float
    , cdp : CDP
    }


type Msg
    = ClickAssetIn
    | InputAssetIn String
    | InputMaxAsset
    | ClickDebtOut
    | InputDebtOut String
    | ClickCollateralOut
    | InputCollateralOut String
    | InputMaxCollateral
    | QueryAgain Posix
    | ClickConnect
    | ClickApproveAsset
    | ClickApproveCollateral
    | ReceiveAnswer Value
    | OnMouseEnter Tooltip
    | OnMouseLeave


type Effect
    = OpenConnect
    | OpenConfirm


init : Transaction
init =
    { state =
        { assetIn = ""
        , out =
            initGivenAsset
                |> Success
        }
            |> Asset
    , tooltip = Nothing
    }
        |> Transaction


initGivenAsset : OutGivenAsset
initGivenAsset =
    { debtOut = Uint.zero
    , collateralOut = Uint.zero
    , liquidityOut = Uint.zero
    , maxDebt = Uint.zero
    , maxCollateral = Uint.zero
    , minLiquidity = Uint.zero
    , apr = 0
    , cdp = CDP.init
    }


initGivenDebt : OutGivenDebt
initGivenDebt =
    { assetIn = Uint.zero
    , collateralOut = Uint.zero
    , liquidityOut = Uint.zero
    , maxDebt = Uint.zero
    , maxCollateral = Uint.zero
    , minLiquidity = Uint.zero
    , apr = 0
    , cdp = CDP.init
    }


initGivenCollateral : OutGivenCollateral
initGivenCollateral =
    { assetIn = Uint.zero
    , debtOut = Uint.zero
    , liquidityOut = Uint.zero
    , maxAsset = Uint.zero
    , maxDebt = Uint.zero
    , minLiquidity = Uint.zero
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
        Disabled.Asset assetIn ->
            if assetIn |> Input.isZero then
                { assetIn = assetIn
                , out = initGivenAsset |> Success
                }
                    |> Asset
                    |> Left

            else
                { assetIn = assetIn
                , out = Loading
                }
                    |> Asset
                    |> Right

        Disabled.Debt debtOut ->
            if debtOut |> Input.isZero then
                { debtOut = debtOut
                , out = initGivenDebt |> Success
                }
                    |> Debt
                    |> Left

            else
                { debtOut = debtOut
                , out = Loading
                }
                    |> Debt
                    |> Right

        Disabled.Collateral collateralOut ->
            if collateralOut |> Input.isZero then
                { collateralOut = collateralOut
                , out = initGivenCollateral |> Success
                }
                    |> Collateral
                    |> Left

            else
                { collateralOut = collateralOut
                , out = Loading
                }
                    |> Collateral
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
        Asset { assetIn } ->
            Disabled.Asset assetIn

        Debt { debtOut } ->
            Disabled.Debt debtOut

        Collateral { collateralOut } ->
            Disabled.Collateral collateralOut


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
        ( ClickAssetIn, Debt { out } ) ->
            (case out of
                Success { assetIn } ->
                    assetIn
                        |> Uint.toAmount (pool.pair |> Pair.toAsset)

                _ ->
                    ""
            )
                |> (\assetIn ->
                        { transaction | state = assetIn |> updateGivenAssetIn }
                            |> query model blockchain pool poolInfo
                   )

        ( ClickAssetIn, Collateral { out } ) ->
            (case out of
                Success { assetIn } ->
                    assetIn
                        |> Uint.toAmount (pool.pair |> Pair.toAsset)

                _ ->
                    ""
            )
                |> (\assetIn ->
                        { transaction | state = assetIn |> updateGivenAssetIn }
                            |> query model blockchain pool poolInfo
                   )

        ( InputAssetIn assetIn, _ ) ->
            if assetIn |> Uint.isAmount (pool.pair |> Pair.toAsset) then
                { transaction | state = assetIn |> updateGivenAssetIn }
                    |> query model blockchain pool poolInfo

            else
                transaction |> noCmdAndEffect

        ( InputMaxAsset, _ ) ->
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
                        { transaction | state = assetIn |> updateGivenAssetIn }
                            |> query model blockchain pool poolInfo
                    )
                |> Maybe.withDefault (transaction |> noCmdAndEffect)

        ( ClickDebtOut, Asset { out } ) ->
            (case out of
                Success { debtOut } ->
                    debtOut
                        |> Uint.toAmount (pool.pair |> Pair.toAsset)

                _ ->
                    ""
            )
                |> (\debtOut ->
                        { transaction | state = debtOut |> updateGivenDebtOut }
                            |> query model blockchain pool poolInfo
                   )

        ( ClickDebtOut, Collateral { out } ) ->
            (case out of
                Success { debtOut } ->
                    debtOut
                        |> Uint.toAmount (pool.pair |> Pair.toAsset)

                _ ->
                    ""
            )
                |> (\debtOut ->
                        { transaction | state = debtOut |> updateGivenDebtOut }
                            |> query model blockchain pool poolInfo
                   )

        ( InputDebtOut debtOut, _ ) ->
            if debtOut |> Uint.isAmount (pool.pair |> Pair.toAsset) then
                { transaction | state = debtOut |> updateGivenDebtOut }
                    |> query model blockchain pool poolInfo

            else
                transaction |> noCmdAndEffect

        ( ClickCollateralOut, Asset { out } ) ->
            (case out of
                Success { collateralOut } ->
                    collateralOut
                        |> Uint.toAmount (pool.pair |> Pair.toCollateral)

                _ ->
                    ""
            )
                |> (\collateralOut ->
                        { transaction | state = collateralOut |> updateGivenCollateralOut }
                            |> query model blockchain pool poolInfo
                   )

        ( ClickCollateralOut, Debt { out } ) ->
            (case out of
                Success { collateralOut } ->
                    collateralOut
                        |> Uint.toAmount (pool.pair |> Pair.toCollateral)

                _ ->
                    ""
            )
                |> (\collateralOut ->
                        { transaction | state = collateralOut |> updateGivenCollateralOut }
                            |> query model blockchain pool poolInfo
                   )

        ( InputCollateralOut collateralOut, _ ) ->
            if collateralOut |> Uint.isAmount (pool.pair |> Pair.toCollateral) then
                { transaction | state = collateralOut |> updateGivenCollateralOut }
                    |> query model blockchain pool poolInfo

            else
                transaction |> noCmdAndEffect

        ( InputMaxCollateral, _ ) ->
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
                        { transaction | state = collateralOut |> updateGivenCollateralOut }
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

        ( ClickApproveAsset, _ ) ->
            (case
                ( blockchain |> Blockchain.toUser
                , case transaction.state of
                    Asset { assetIn } ->
                        assetIn
                            |> Uint.fromAmount
                                (pool.pair |> Pair.toAsset)

                    Debt { out } ->
                        case out of
                            Success { assetIn } ->
                                assetIn |> Just

                            _ ->
                                Nothing

                    Collateral { out } ->
                        case out of
                            Success { assetIn } ->
                                assetIn |> Just

                            _ ->
                                Nothing
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
                            |> approveLiquidity
                        , OpenConfirm |> Just
                        )
                            |> Just

                    else
                        Nothing

                _ ->
                    Nothing
            )
                |> Maybe.withDefault (transaction |> noCmdAndEffect)

        ( ClickApproveCollateral, _ ) ->
            (case
                ( blockchain |> Blockchain.toUser
                , case transaction.state of
                    Asset { out } ->
                        case out of
                            Success { collateralOut } ->
                                collateralOut |> Just

                            _ ->
                                Nothing

                    Debt { out } ->
                        case out of
                            Success { collateralOut } ->
                                collateralOut |> Just

                            _ ->
                                Nothing

                    Collateral { collateralOut } ->
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
                            |> approveLiquidity
                        , OpenConfirm |> Just
                        )
                            |> Just

                    else
                        Nothing

                _ ->
                    Nothing
            )
                |> Maybe.withDefault (transaction |> noCmdAndEffect)

        ( ReceiveAnswer value, Asset asset ) ->
            (case value |> Decode.decodeValue Answer.decoder of
                Ok (Answer.GivenAsset answer) ->
                    if
                        (answer.chainId == (blockchain |> Blockchain.toChain))
                            && (answer.pool == pool)
                            && (answer.poolInfo == poolInfo)
                            && (Just answer.assetIn
                                    == (asset.assetIn
                                            |> Uint.fromAmount
                                                (pool.pair |> Pair.toAsset)
                                       )
                               )
                            && (answer.slippage == model.slippage)
                    then
                        { transaction
                            | state =
                                { asset | out = answer.result |> toRemote }
                                    |> Asset
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
                                { debt | out = answer.result |> toRemote }
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
                                { collateral | out = answer.result |> toRemote }
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

        ( OnMouseEnter tooltip, _ ) ->
            { transaction | tooltip = Just tooltip }
                |> noCmdAndEffect

        ( OnMouseLeave, _ ) ->
            { transaction | tooltip = Nothing }
                |> noCmdAndEffect

        _ ->
            transaction |> noCmdAndEffect


updateGivenAssetIn : String -> State
updateGivenAssetIn assetIn =
    { assetIn = assetIn
    , out =
        if assetIn |> Input.isZero then
            initGivenAsset
                |> Success

        else
            Loading
    }
        |> Asset


updateGivenDebtOut : String -> State
updateGivenDebtOut debtOut =
    { debtOut = debtOut
    , out =
        if debtOut |> Input.isZero then
            initGivenDebt
                |> Success

        else
            Loading
    }
        |> Debt


updateGivenCollateralOut : String -> State
updateGivenCollateralOut collateralOut =
    { collateralOut = collateralOut
    , out =
        if collateralOut |> Input.isZero then
            initGivenCollateral
                |> Success

        else
            Loading
    }
        |> Collateral


toRemote :
    Result error answer
    -> Remote error answer
toRemote result =
    case result of
        Ok out ->
            Success out

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
    constructQuery queryLiquidity


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
        |> constructQuery queryLiquidity
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
        |> constructQuery queryLiquidityPerSecond
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
        Asset asset ->
            case
                ( asset.assetIn |> Input.isZero
                , asset.assetIn
                    |> Uint.fromAmount
                        (pool.pair |> Pair.toAsset)
                )
            of
                ( False, Just assetIn ) ->
                    { chainId = blockchain |> Blockchain.toChain
                    , pool = pool
                    , poolInfo = poolInfo
                    , assetIn = assetIn
                    , slippage = slippage
                    }
                        |> Query.givenAsset
                        |> Just

                _ ->
                    Nothing

        Debt debt ->
            case
                ( debt.debtOut |> Input.isZero
                , debt.debtOut
                    |> Uint.fromAmount
                        (pool.pair |> Pair.toAsset)
                )
            of
                ( False, Just debtOut ) ->
                    { chainId = blockchain |> Blockchain.toChain
                    , pool = pool
                    , poolInfo = poolInfo
                    , debtOut = debtOut
                    , slippage = slippage
                    }
                        |> Query.givenDebt
                        |> Just

                _ ->
                    Nothing

        Collateral collateral ->
            case
                ( collateral.collateralOut |> Input.isZero
                , collateral.collateralOut
                    |> Uint.fromAmount
                        (pool.pair |> Pair.toCollateral)
                )
            of
                ( False, Just collateralOut ) ->
                    { chainId = blockchain |> Blockchain.toChain
                    , pool = pool
                    , poolInfo = poolInfo
                    , collateralOut = collateralOut
                    , slippage = slippage
                    }
                        |> Query.givenCollateral
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


port queryLiquidity : Value -> Cmd msg


port queryLiquidityPerSecond : Value -> Cmd msg


port approveLiquidity : Value -> Cmd msg


port receiveAddAnswer : (Value -> msg) -> Sub msg


subscriptions : Transaction -> Sub Msg
subscriptions (Transaction { state }) =
    if state |> hasInputZero then
        Sub.none

    else
        [ Time.every 1000 QueryAgain
        , receiveAddAnswer ReceiveAnswer
        ]
            |> Sub.batch


hasInputZero : State -> Bool
hasInputZero state =
    case state of
        Asset { assetIn } ->
            assetIn |> Input.isZero

        Debt { debtOut } ->
            debtOut |> Input.isZero

        Collateral { collateralOut } ->
            collateralOut |> Input.isZero


view :
    { model | spot : Spot, images : Images }
    -> Blockchain
    -> Pool
    -> Transaction
    ->
        { first : Element Msg
        , second : Element Msg
        , third : Element Msg
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
            |> duesOutSection model
                blockchain
                pool
    , third =
        transaction
            |> liquidityOutSection model
                pool
    , buttons = none |> Debug.log "later"
    }


assetInSection :
    { model | images : Images }
    -> Blockchain
    -> Token
    -> { transaction | state : State, tooltip : Maybe Tooltip }
    -> Element Msg
assetInSection model blockchain asset { state, tooltip } =
    column
        [ Region.description "lend asset"
        , width <| px 343
        , height shrink
        , padding 16
        , spacing 10
        , Background.color Color.primary100
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
                , Font.color Color.primary400
                ]
                (text "Amount to Lend")
            , blockchain
                |> Blockchain.toUser
                |> Maybe.andThen (User.getBalance asset)
                |> Maybe.map
                    (\balance ->
                        MaxButton.view
                            { onPress = InputMaxAsset
                            , onMouseEnter = OnMouseEnter
                            , onMouseLeave = OnMouseLeave
                            , tooltip = Tooltip.AssetBalance
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
            , onClick = Just ClickAssetIn
            , onChange = InputAssetIn
            , text =
                case state of
                    Asset { assetIn } ->
                        Left assetIn

                    Debt { out } ->
                        case out |> Remote.map .assetIn of
                            Success uint ->
                                Right uint

                            _ ->
                                Left ""

                    Collateral { out } ->
                        case out |> Remote.map .assetIn of
                            Success uint ->
                                Right uint

                            _ ->
                                Left ""
            , description = "asset in textbox"
            }
        ]


duesOutSection :
    { model | spot : Spot, images : Images }
    -> Blockchain
    -> Pool
    -> { transaction | state : State, tooltip : Maybe Tooltip }
    -> Element Msg
duesOutSection model blockchain pool ({ state, tooltip } as transaction) =
    column
        [ Region.description "dues"
        , width <| px 343
        , height shrink
        , padding 16
        , spacing 12
        , Background.color Color.primary100
        , Border.rounded 8
        ]
        [ row
            [ width fill
            , height shrink
            , spacing 16
            ]
            ((case state of
                Asset { out } ->
                    case out of
                        Success { apr, cdp } ->
                            ( apr, cdp ) |> Just

                        _ ->
                            Nothing

                Debt { out } ->
                    case out of
                        Success { apr, cdp } ->
                            ( apr, cdp ) |> Just

                        _ ->
                            Nothing

                Collateral { out } ->
                    case out of
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
        , column
            [ width fill
            , height shrink
            , spacing 12
            ]
            [ (case state of
                Asset { out } ->
                    case out |> Remote.map .debtOut of
                        Success uint ->
                            Right uint

                        _ ->
                            Left ""

                Debt { debtOut } ->
                    Left debtOut

                Collateral { out } ->
                    case out |> Remote.map .debtOut of
                        Success uint ->
                            Right uint

                        _ ->
                            Left ""
              )
                |> debtOutSection model
                    (pool.pair |> Pair.toAsset)
                    transaction
            , (case state of
                Asset { out } ->
                    case out |> Remote.map .collateralOut of
                        Success uint ->
                            Right uint

                        _ ->
                            Left ""

                Debt { out } ->
                    case out |> Remote.map .collateralOut of
                        Success uint ->
                            Right uint

                        _ ->
                            Left ""

                Collateral { collateralOut } ->
                    Left collateralOut
              )
                |> collateralOutSection model
                    blockchain
                    (pool.pair |> Pair.toCollateral)
                    transaction
            ]
        ]


debtOutSection :
    { model | images : Images }
    -> Token
    -> { transaction | tooltip : Maybe Tooltip }
    -> Or String Uint
    -> Element Msg
debtOutSection model asset { tooltip } out =
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
            , text = out
            , description = "debt out textbox"
            }
        ]


collateralOutSection :
    { model | images : Images }
    -> Blockchain
    -> Token
    -> { transaction | tooltip : Maybe Tooltip }
    -> Or String Uint
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
                , Font.color Color.primary400
                ]
                (text "Collateral to Lock")
            , blockchain
                |> Blockchain.toUser
                |> Maybe.andThen (User.getBalance collateral)
                |> Maybe.map
                    (\balance ->
                        MaxButton.view
                            { onPress = InputMaxCollateral
                            , onMouseEnter = OnMouseEnter
                            , onMouseLeave = OnMouseLeave
                            , tooltip = Tooltip.CollateralBalance
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
            , text = or
            , description = "collateral out textbox"
            }
        ]


liquidityOutSection :
    { model | images : Images }
    -> Pool
    -> { transaction | state : State }
    -> Element Msg
liquidityOutSection model pool { state } =
    column
        [ Region.description "liquidity output"
        , width <| px 343
        , height shrink
        , padding 16
        , spacing 10
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
            (text "LP Tokens")
        , Output.liquidity model
            { asset = pool.pair |> Pair.toAsset
            , collateral = pool.pair |> Pair.toCollateral
            , output =
                case state of
                    Asset { out } ->
                        out
                            |> Remote.map .liquidityOut

                    Debt { out } ->
                        out
                            |> Remote.map .liquidityOut

                    Collateral { out } ->
                        out
                            |> Remote.map .liquidityOut
            , description = "liquidity out"
            }
        ]
