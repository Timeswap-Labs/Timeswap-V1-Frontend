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
import Blockchain.User.WriteLiquidity as WriteLiquidity exposing (WriteLiquidity)
import Data.CDP as CDP exposing (CDP)
import Data.ERC20 exposing (ERC20)
import Data.Images exposing (Images)
import Data.Or exposing (Or(..))
import Data.Pair as Pair exposing (Pair)
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
        , centerX
        , centerY
        , column
        , el
        , fill
        , height
        , map
        , none
        , padding
        , paddingXY
        , paragraph
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
import Page.Transaction.Liquidity.Add.Disabled as Disabled
import Page.Transaction.Liquidity.Add.Error as Error exposing (Error)
import Page.Transaction.Liquidity.Add.Query as Query
import Page.Transaction.Liquidity.Add.Tooltip as Tooltip exposing (Tooltip)
import Page.Transaction.MaxButton as MaxButton
import Page.Transaction.Output as Output
import Page.Transaction.Textbox as Textbox
import Time exposing (Posix)
import Utility.Color as Color
import Utility.Image as Image
import Utility.Input as Input
import Utility.Loading as Loading
import Utility.ThemeColor as ThemeColor
import Utility.Tooltip as TooltipUtil
import Utility.Truncate as Truncate


type Transaction
    = Transaction
        { state : State
        , tooltip : Maybe Tooltip
        }


type State
    = Asset AssetInput
    | Collateral CollateralInput


type alias AssetInput =
    { assetIn : String
    , out : Remote Error OutGivenAsset
    }


type alias CollateralInput =
    { collateralIn : String
    , out : Remote Error OutGivenCollateral
    }


type alias OutGivenAsset =
    { debtIn : Uint
    , collateralIn : Uint
    , liquidityOut : Uint
    , maxDebt : Uint
    , maxCollateral : Uint
    , minLiquidity : Uint
    , apr : Float
    , cdp : CDP
    , liquidityShare : Float
    }


type alias OutGivenCollateral =
    { assetIn : Uint
    , debtIn : Uint
    , liquidityOut : Uint
    , maxAsset : Uint
    , maxDebt : Uint
    , minLiquidity : Uint
    , apr : Float
    , cdp : CDP
    , liquidityShare : Float
    }


type Msg
    = ClickAssetIn
    | InputAssetIn String
    | InputMaxAsset
    | ClickCollateralIn
    | InputCollateralIn String
    | InputMaxCollateral
    | QueryAgain Posix
    | ClickConnect
    | ClickApproveAsset
    | ClickApproveCollateral
    | ClickLiquidity
    | ReceiveAnswer Value
    | Tick Posix
    | OnMouseEnter Tooltip
    | OnMouseLeave


type Effect
    = OpenConnect
    | OpenCaution WriteLiquidity
    | Approve ERC20
    | Liquidity WriteLiquidity


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
    { debtIn = Uint.zero
    , collateralIn = Uint.zero
    , liquidityOut = Uint.zero
    , maxDebt = Uint.zero
    , maxCollateral = Uint.zero
    , minLiquidity = Uint.zero
    , apr = 0
    , cdp = CDP.init
    , liquidityShare = 0
    }


initGivenCollateral : OutGivenCollateral
initGivenCollateral =
    { assetIn = Uint.zero
    , debtIn = Uint.zero
    , liquidityOut = Uint.zero
    , maxAsset = Uint.zero
    , maxDebt = Uint.zero
    , minLiquidity = Uint.zero
    , apr = 0
    , cdp = CDP.init
    , liquidityShare = 0
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
                , out = Remote.loading
                }
                    |> Asset
                    |> Right

        Disabled.Collateral collateralIn ->
            if collateralIn |> Input.isZero then
                { collateralIn = collateralIn
                , out = initGivenCollateral |> Success
                }
                    |> Collateral
                    |> Left

            else
                { collateralIn = collateralIn
                , out = Remote.loading
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

        Collateral { collateralIn } ->
            Disabled.Collateral collateralIn


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
        ( ClickAssetIn, Collateral { collateralIn, out } ) ->
            (case out of
                Success { assetIn } ->
                    if collateralIn |> Input.isZero then
                        ""

                    else
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

        ( ClickCollateralIn, Asset { assetIn, out } ) ->
            (case out of
                Success { collateralIn } ->
                    if assetIn |> Input.isZero then
                        ""

                    else
                        collateralIn
                            |> Uint.toAmount (pool.pair |> Pair.toCollateral)

                _ ->
                    ""
            )
                |> (\collateralIn ->
                        { transaction | state = collateralIn |> updateGivenCollateralOut }
                            |> query model blockchain pool poolInfo
                   )

        ( InputCollateralIn collateralIn, _ ) ->
            if collateralIn |> Uint.isAmount (pool.pair |> Pair.toCollateral) then
                { transaction | state = collateralIn |> updateGivenCollateralOut }
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
                    (\collateralIn ->
                        { transaction | state = collateralIn |> updateGivenCollateralOut }
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

        ( ClickApproveCollateral, _ ) ->
            (case
                ( blockchain |> Blockchain.toUser
                , case transaction.state of
                    Asset { out } ->
                        case out of
                            Success { collateralIn } ->
                                collateralIn |> Just

                            _ ->
                                Nothing

                    Collateral { collateralIn } ->
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

        ( ClickLiquidity, Asset asset ) ->
            (case asset.out of
                Success answer ->
                    case
                        ( blockchain |> Blockchain.toUser
                        , asset.assetIn
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
                                    && (user
                                            |> User.hasEnoughBalance
                                                (pool.pair |> Pair.toCollateral)
                                                answer.collateralIn
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
                                  , assetIn = assetIn
                                  , minLiquidity = answer.minLiquidity
                                  , maxDebt = answer.maxDebt
                                  , maxCollateral = answer.maxCollateral
                                  }
                                    |> WriteLiquidity.GivenAsset
                                    |> OpenCaution
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

        ( ClickLiquidity, Collateral collateral ) ->
            (case collateral.out of
                Success answer ->
                    case
                        ( blockchain |> Blockchain.toUser
                        , collateral.collateralIn
                            |> Uint.fromAmount
                                (pool.pair |> Pair.toCollateral)
                        )
                    of
                        ( Just user, Just collateralIn ) ->
                            if
                                (user
                                    |> User.hasEnoughBalance
                                        (pool.pair |> Pair.toAsset)
                                        answer.assetIn
                                )
                                    && (user
                                            |> User.hasEnoughBalance
                                                (pool.pair |> Pair.toCollateral)
                                                collateralIn
                                       )
                                    && (pool.pair
                                            |> Pair.toAsset
                                            |> Token.toERC20
                                            |> Maybe.map
                                                (\erc20 ->
                                                    user
                                                        |> User.hasEnoughAllowance
                                                            erc20
                                                            answer.assetIn
                                                )
                                            |> Maybe.withDefault True
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
                                  , collateralIn = collateralIn
                                  , minLiquidity = answer.minLiquidity
                                  , maxAsset = answer.maxAsset
                                  , maxDebt = answer.maxDebt
                                  }
                                    |> WriteLiquidity.GivenCollateral
                                    |> OpenCaution
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

        ( ReceiveAnswer value, Asset asset ) ->
            (case value |> Decode.decodeValue Query.decoder of
                Ok (Query.GivenAsset answer) ->
                    if
                        (answer.chain == (blockchain |> Blockchain.toChain))
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

        ( ReceiveAnswer value, Collateral collateral ) ->
            (case value |> Decode.decodeValue Query.decoder of
                Ok (Query.GivenCollateral answer) ->
                    if
                        (answer.chain == (blockchain |> Blockchain.toChain))
                            && (answer.pool == pool)
                            && (answer.poolInfo == poolInfo)
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

        ( Tick posix, Asset asset ) ->
            { transaction
                | state =
                    { asset
                        | out = asset.out |> Remote.update posix
                    }
                        |> Asset
            }
                |> noCmdAndEffect

        ( Tick posix, Collateral collateral ) ->
            { transaction
                | state =
                    { collateral
                        | out = collateral.out |> Remote.update posix
                    }
                        |> Collateral
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


updateGivenAssetIn : String -> State
updateGivenAssetIn assetIn =
    { assetIn = assetIn
    , out =
        if assetIn |> Input.isZero then
            initGivenAsset
                |> Success

        else
            Remote.loading
    }
        |> Asset


updateGivenCollateralOut : String -> State
updateGivenCollateralOut collateralIn =
    { collateralIn = collateralIn
    , out =
        if collateralIn |> Input.isZero then
            initGivenCollateral
                |> Success

        else
            Remote.loading
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
                    { chain = blockchain |> Blockchain.toChain
                    , pool = pool
                    , poolInfo = poolInfo
                    , assetIn = assetIn
                    , slippage = slippage
                    }
                        |> Query.givenAsset
                        |> Just

                _ ->
                    Nothing

        Collateral collateral ->
            case
                ( collateral.collateralIn |> Input.isZero
                , collateral.collateralIn
                    |> Uint.fromAmount
                        (pool.pair |> Pair.toCollateral)
                )
            of
                ( False, Just collateralIn ) ->
                    { chain = blockchain |> Blockchain.toChain
                    , pool = pool
                    , poolInfo = poolInfo
                    , collateralIn = collateralIn
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


port liquidity : Value -> Cmd msg


port receiveAddLiqAnswer : (Value -> msg) -> Sub msg


subscriptions : Transaction -> Sub Msg
subscriptions (Transaction { state }) =
    [ if state |> hasInputZero then
        Sub.none

      else
        [ Time.every 1000 QueryAgain
        , receiveAddLiqAnswer ReceiveAnswer
        ]
            |> Sub.batch
    , case state of
        Asset { out } ->
            out |> Remote.subscriptions Tick

        Collateral { out } ->
            out |> Remote.subscriptions Tick
    ]
        |> Sub.batch


hasInputZero : State -> Bool
hasInputZero state =
    case state of
        Asset { assetIn } ->
            assetIn |> Input.isZero

        Collateral { collateralIn } ->
            collateralIn |> Input.isZero


view :
    { model | priceFeed : PriceFeed, images : Images, theme : Theme }
    -> Blockchain
    -> Pool
    -> PoolInfo
    -> Transaction
    ->
        { first : Element Msg
        , second : Element Msg
        , third : Element Msg
        , buttons : Element Msg
        }
view model blockchain pool poolInfo (Transaction transaction) =
    { first =
        transaction
            |> assetInSection model
                blockchain
                pool
    , second =
        transaction
            |> duesInSection model
                blockchain
                pool
                poolInfo
    , third =
        transaction
            |> warningSection model
                pool
    , buttons =
        transaction
            |> buttons model.theme blockchain pool.pair
    }


assetInSection :
    { model | images : Images, theme : Theme }
    -> Blockchain
    -> Pool
    -> { transaction | state : State, tooltip : Maybe Tooltip }
    -> Element Msg
assetInSection model blockchain pool ({ state, tooltip } as transaction) =
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
                    (text "Add Asset")
                , (case state of
                    Collateral { out } ->
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
                                (Loading.view timeline model.theme)
                        )
                    |> Maybe.withDefault none
                ]
            , blockchain
                |> Blockchain.toUser
                |> Maybe.andThen (User.getBalance (pool.pair |> Pair.toAsset))
                |> Maybe.map
                    (\balance ->
                        MaxButton.view
                            { onPress = InputMaxAsset
                            , onMouseEnter = OnMouseEnter
                            , onMouseLeave = OnMouseLeave
                            , tooltip = Tooltip.AssetBalance
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
            , onClick = Just ClickAssetIn
            , onChange = InputAssetIn
            , text =
                case state of
                    Asset { assetIn } ->
                        Left assetIn

                    Collateral { out } ->
                        case out |> Remote.map .assetIn of
                            Success uint ->
                                Right uint

                            _ ->
                                Left ""
            , description = "asset-in textbox"
            }
        , (case state of
            Asset { out } ->
                out
                    |> Remote.map .collateralIn
                    |> Right

            Collateral { collateralIn } ->
                Left collateralIn
          )
            |> collateralInSection model
                blockchain
                (pool.pair |> Pair.toCollateral)
                transaction
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
                    , model.theme |> ThemeColor.actionElemLabel |> Font.color
                    ]
                    (text "Add Collateral")
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
                    Right (Success uint) ->
                        Right uint

                    Left string ->
                        Left string

                    _ ->
                        Left ""
            , description = "collateral-out textbox"
            }
        ]


duesInSection :
    { model | priceFeed : PriceFeed, images : Images, theme : Theme }
    -> Blockchain
    -> Pool
    -> PoolInfo
    -> { transaction | state : State, tooltip : Maybe Tooltip }
    -> Element Msg
duesInSection model blockchain pool poolInfo ({ state, tooltip } as transaction) =
    column [ width fill, spacing 16 ]
        [ column
            [ Region.description "APR and CDP"
            , width fill
            , height shrink
            , padding 16
            , spacing 12
            , model.theme |> ThemeColor.sectionBackground |> Background.color
            , Border.rounded 8
            ]
            [ row
                [ width fill
                , height shrink
                , spacing 16
                ]
                ((case state of
                    Asset { out, assetIn } ->
                        ( out |> Remote.map .apr
                        , out |> Remote.map .cdp
                        , assetIn
                        )

                    Collateral { out } ->
                        ( out |> Remote.map .apr
                        , out |> Remote.map .cdp
                        , "1"
                        )
                 )
                    |> (\( apr, cdp, assetIn ) ->
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
            ]
        , column
            [ Region.description "debt and pool-share"
            , width fill
            , height shrink
            , padding 16
            , spacing 12
            , model.theme |> ThemeColor.sectionBackground |> Background.color
            , Border.rounded 8
            ]
            [ column
                [ width fill
                , height shrink
                , spacing 12
                ]
                [ (case state of
                    Asset { out } ->
                        out
                            |> Remote.map .debtIn

                    Collateral { out } ->
                        out
                            |> Remote.map .debtIn
                  )
                    |> debtInSection model
                        (pool.pair |> Pair.toAsset)
                        transaction
                ]
            ]
        ]


debtInSection :
    { model | images : Images, theme : Theme }
    -> Token
    -> { transaction | state : State, tooltip : Maybe Tooltip }
    -> Remote Error Uint
    -> Element Msg
debtInSection model asset { state, tooltip } remote =
    column
        [ width fill
        , height shrink
        , spacing 14
        ]
        [ row
            [ width fill
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
                (text "Debt to Repay")
            , case remote of
                Loading timeline ->
                    el
                        [ width shrink
                        , height shrink
                        , centerY
                        ]
                        (Loading.view timeline model.theme)

                _ ->
                    none
            , row [ spacing 8, alignRight ]
                [ case remote of
                    Success uint ->
                        Truncate.viewAmount
                            { onMouseEnter = OnMouseEnter
                            , onMouseLeave = OnMouseLeave
                            , tooltip = Tooltip.DebtOutSymbol
                            , opened = tooltip
                            , token = asset
                            , amount = uint
                            , theme = model.theme
                            , customStyles = [ Font.size 18 ]
                            }

                    _ ->
                        none
                , model.images
                    |> Image.viewToken
                        [ width <| px 24
                        , height <| px 24
                        ]
                        asset
                , Truncate.viewSymbol
                    { onMouseEnter = OnMouseEnter
                    , onMouseLeave = OnMouseLeave
                    , tooltip = Tooltip.DebtOutSymbol
                    , opened = tooltip
                    , token = asset
                    , theme = model.theme
                    , customStyles = [ Font.size 18 ]
                    }
                ]
            ]
        , row [ width fill ]
            [ row [ width fill, spacing 8 ]
                [ el
                    [ Font.size 14
                    , model.theme |> ThemeColor.textLight |> Font.color
                    ]
                    (text "Pool Share")
                , model.images
                    |> (case model.theme of
                            Theme.Dark ->
                                Image.info

                            Theme.Light ->
                                Image.infoDark
                       )
                        [ width <| px 12
                        , height <| px 12
                        , Font.alignLeft
                        , Events.onMouseEnter (OnMouseEnter Tooltip.PoolShare)
                        , Events.onMouseLeave OnMouseLeave
                        , (if tooltip == Just Tooltip.PoolShare then
                            el
                                [ Font.size 14
                                , model.theme |> ThemeColor.textLight |> Font.color
                                ]
                                ("This is your share of the total liquidity provided in the pool" |> text)
                                |> TooltipUtil.belowAlignLeft model.theme

                           else
                            none
                          )
                            |> below
                        ]
                , el [ alignRight, Font.color Color.positive500, Font.size 18, Font.bold ]
                    ((case state of
                        Asset { out } ->
                            out
                                |> Remote.map .liquidityShare

                        Collateral { out } ->
                            out
                                |> Remote.map .liquidityShare
                     )
                        |> (\liquidityShare ->
                                case liquidityShare of
                                    Success poolShare ->
                                        text ("%" |> String.append (String.fromFloat poolShare))

                                    Loading timeline ->
                                        el
                                            [ width shrink
                                            , height shrink
                                            , centerX
                                            , centerY
                                            ]
                                            (Loading.view timeline model.theme)

                                    _ ->
                                        none
                           )
                    )
                ]
            ]
        ]


warningSection :
    { model | images : Images, theme : Theme }
    -> Pool
    -> { transaction | state : State }
    -> Element Msg
warningSection { images, theme } pool { state } =
    column
        [ width fill
        , height shrink
        , centerX
        , padding 16
        , spacing 12
        , Font.size 14
        , Border.rounded 8
        , Background.color Color.warning100
        ]
        [ images
            |> Image.warning
                [ width <| px 24, height <| px 24, Font.center, centerX ]
        , paragraph
            [ Font.color Color.warning400
            , Font.center
            ]
            [ text "The above Debt must be repaid before maturity of the pool, else the collateral locked will be forfeited. You can view the debt position under the Borrow tab." ]
        ]


liqOutSection :
    { model | images : Images, theme : Theme }
    -> Pool
    -> { transaction | state : State }
    -> Element Msg
liqOutSection model pool { state } =
    column
        [ Region.description "liquidity output"
        , width fill
        , height shrink
        , padding 16
        , spacing 10
        , model.theme |> ThemeColor.sectionBackground |> Background.color
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
                , model.theme |> ThemeColor.actionElemLabel |> Font.color
                ]
                (text "LP Tokens to Receive")
            , (case state of
                Asset { out } ->
                    case out of
                        Loading timeline ->
                            Just timeline

                        _ ->
                            Nothing

                Collateral { out } ->
                    case out of
                        Loading timeline ->
                            Just timeline

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
                            (Loading.view timeline model.theme)
                    )
                |> Maybe.withDefault none
            ]
        , Output.liquidity model
            { pair = pool.pair
            , output =
                case state of
                    Asset { out } ->
                        out
                            |> Remote.map .liquidityOut

                    Collateral { out } ->
                        out
                            |> Remote.map .liquidityOut
            , description = "liquidity out"
            }
        ]


toAsset : Token -> { transaction | state : State } -> Maybe Uint
toAsset asset transaction =
    case transaction.state of
        Asset { assetIn } ->
            assetIn
                |> Uint.fromAmount asset

        Collateral { out } ->
            out
                |> Remote.map .assetIn
                |> Remote.map Just
                |> Remote.withDefault Nothing


toCollateral : Token -> { transaction | state : State } -> Maybe Uint
toCollateral collateral transaction =
    case transaction.state of
        Asset { out } ->
            out
                |> Remote.map .collateralIn
                |> Remote.map Just
                |> Remote.withDefault Nothing

        Collateral { collateralIn } ->
            collateralIn
                |> Uint.fromAmount collateral


type Txn
    = HasTxn
    | NoTxn


fromTxnToResult :
    { transaction | state : State }
    -> Result Error Txn
fromTxnToResult { state } =
    case state of
        Asset { assetIn, out } ->
            if assetIn |> Input.isZero then
                Ok NoTxn

            else
                case out of
                    Success _ ->
                        Ok HasTxn

                    Loading _ ->
                        Ok NoTxn

                    Failure error ->
                        Err error

        Collateral { collateralIn, out } ->
            if collateralIn |> Input.isZero then
                Ok NoTxn

            else
                case out of
                    Success _ ->
                        Ok HasTxn

                    Loading _ ->
                        Ok NoTxn

                    Failure error ->
                        Err error


buttons :
    Theme
    -> Blockchain
    -> Pair
    -> { transaction | state : State }
    -> Element Msg
buttons theme blockchain pair transaction =
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
                        ( transaction |> toAsset (pair |> Pair.toAsset)
                        , transaction |> toCollateral (pair |> Pair.toCollateral)
                        , transaction |> fromTxnToResult
                        )
                    of
                        ( Just assetIn, Just collateralIn, Ok HasTxn ) ->
                            case
                                ( pair |> Pair.toAsset |> Token.toERC20
                                , pair |> Pair.toCollateral |> Token.toERC20
                                )
                            of
                                ( Just assetErc20, Just collErc20 ) ->
                                    case
                                        ( user
                                            |> User.getAllowance assetErc20
                                            |> (Maybe.map << Remote.map)
                                                (Uint.hasEnough assetIn)
                                        , user
                                            |> User.getAllowance collErc20
                                            |> (Maybe.map << Remote.map)
                                                (Uint.hasEnough collateralIn)
                                        )
                                    of
                                        ( Just (Success True), Just (Success True) ) ->
                                            case
                                                ( user
                                                    |> User.getBalance (pair |> Pair.toAsset)
                                                    |> (Maybe.map << Remote.map)
                                                        (Uint.hasEnough assetIn)
                                                , user
                                                    |> User.getBalance (pair |> Pair.toCollateral)
                                                    |> (Maybe.map << Remote.map)
                                                        (Uint.hasEnough collateralIn)
                                                )
                                            of
                                                ( Just (Success True), Just (Success True) ) ->
                                                    [ liquidityButton theme ]

                                                ( Just (Success False), Just (Success True) ) ->
                                                    [ insufficientTokenBalance (pair |> Pair.toAsset) ]

                                                ( Just (Success True), Just (Success False) ) ->
                                                    [ insufficientTokenBalance (pair |> Pair.toCollateral) ]

                                                ( Just (Success False), Just (Success False) ) ->
                                                    [ insufficientTokenBalance (pair |> Pair.toAsset)
                                                    , insufficientTokenBalance (pair |> Pair.toCollateral)
                                                    ]

                                                _ ->
                                                    []

                                        ( Just (Success False), Just (Success True) ) ->
                                            [ Button.approveAsset ClickApproveAsset theme ]

                                        ( Just (Success True), Just (Success False) ) ->
                                            [ Button.approveCollateral ClickApproveCollateral theme ]

                                        ( Just (Success False), Just (Success False) ) ->
                                            [ Button.approveAsset ClickApproveAsset theme
                                            , Button.approveCollateral ClickApproveCollateral theme
                                            ]

                                        ( Just (Success True), Just (Loading _) ) ->
                                            [ theme |> Button.checkingAllowance |> map never
                                            , theme |> disabledLiquidityButton
                                            ]

                                        ( Just (Loading _), Just (Success True) ) ->
                                            [ theme |> Button.checkingAllowance |> map never
                                            , theme |> disabledLiquidityButton
                                            ]

                                        ( Just (Loading _), Just (Loading _) ) ->
                                            [ theme |> Button.checkingAllowance |> map never
                                            , theme |> disabledLiquidityButton
                                            ]

                                        ( Just (Failure error), _ ) ->
                                            [ Button.error error |> map never ]

                                        ( _, Just (Failure error) ) ->
                                            [ Button.error error |> map never ]

                                        _ ->
                                            []

                                ( Just assetErc20, Nothing ) ->
                                    case
                                        user
                                            |> User.getAllowance assetErc20
                                            |> (Maybe.map << Remote.map)
                                                (Uint.hasEnough assetIn)
                                    of
                                        Just (Success True) ->
                                            case
                                                ( user
                                                    |> User.getBalance (pair |> Pair.toAsset)
                                                    |> (Maybe.map << Remote.map)
                                                        (Uint.hasEnough assetIn)
                                                , user
                                                    |> User.getBalance (pair |> Pair.toCollateral)
                                                    |> (Maybe.map << Remote.map)
                                                        (Uint.hasEnough collateralIn)
                                                )
                                            of
                                                ( Just (Success True), Just (Success True) ) ->
                                                    [ liquidityButton theme ]

                                                ( Just (Success False), _ ) ->
                                                    [ insufficientTokenBalance (pair |> Pair.toAsset) ]

                                                ( _, Just (Success False) ) ->
                                                    [ insufficientTokenBalance (pair |> Pair.toCollateral) ]

                                                ( Just (Loading _), _ ) ->
                                                    [ Button.checkingBalance theme |> map never ]

                                                ( _, Just (Loading _) ) ->
                                                    [ Button.checkingBalance theme |> map never ]

                                                _ ->
                                                    []

                                        Just (Success False) ->
                                            [ Button.approveAsset ClickApproveAsset theme ]

                                        Just (Loading _) ->
                                            [ Button.checkingAllowance theme |> map never ]

                                        _ ->
                                            []

                                ( Nothing, Just collErc20 ) ->
                                    case
                                        user
                                            |> User.getAllowance collErc20
                                            |> (Maybe.map << Remote.map)
                                                (Uint.hasEnough collateralIn)
                                    of
                                        Just (Success True) ->
                                            case
                                                ( user
                                                    |> User.getBalance (pair |> Pair.toAsset)
                                                    |> (Maybe.map << Remote.map)
                                                        (Uint.hasEnough assetIn)
                                                , user
                                                    |> User.getBalance (pair |> Pair.toCollateral)
                                                    |> (Maybe.map << Remote.map)
                                                        (Uint.hasEnough collateralIn)
                                                )
                                            of
                                                ( Just (Success True), Just (Success True) ) ->
                                                    [ liquidityButton theme ]

                                                ( Just (Success False), _ ) ->
                                                    [ insufficientTokenBalance (pair |> Pair.toAsset) ]

                                                ( _, Just (Success False) ) ->
                                                    [ insufficientTokenBalance (pair |> Pair.toCollateral) ]

                                                ( Just (Loading _), _ ) ->
                                                    [ Button.checkingBalance theme |> map never ]

                                                ( _, Just (Loading _) ) ->
                                                    [ Button.checkingBalance theme |> map never ]

                                                _ ->
                                                    []

                                        Just (Success False) ->
                                            [ Button.approveCollateral ClickApproveCollateral theme ]

                                        Just (Loading _) ->
                                            [ Button.checkingAllowance theme |> map never ]

                                        _ ->
                                            []

                                _ ->
                                    []

                        ( _, _, Err err ) ->
                            [ Button.customError (err |> Error.toString) |> map never ]

                        ( _, _, Ok NoTxn ) ->
                            []

                        _ ->
                            []
                )
            |> Maybe.withDefault
                [ Button.connect theme ClickConnect ]
        )


liquidityButton : Theme -> Element Msg
liquidityButton theme =
    Button.view
        { onPress = ClickLiquidity
        , text = "Add Liquidity"
        , theme = theme
        }


disabledLiquidityButton : Theme -> Element msg
disabledLiquidityButton theme =
    Button.disabled theme "Add Liquidity"
        |> map never


insufficientTokenBalance : Token -> Element msg
insufficientTokenBalance token =
    el
        [ width fill
        , height <| px 44
        , Background.color Color.negative500
        , Border.rounded 4
        ]
        (el
            [ centerX
            , centerY
            , paddingXY 0 4
            , Font.size 16
            , Font.bold
            , Font.color Color.light100
            ]
            ([ "Insufficient"
             , token |> Token.toSymbol
             , "Balance"
             ]
                |> String.join " "
                |> text
            )
        )
