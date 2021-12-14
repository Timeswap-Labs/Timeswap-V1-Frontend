port module Page.Transaction.Liquidity.New.Main exposing
    ( Effect(..)
    , Msg
    , Transaction
    , fromDisabled
    , init
    , toDisabled
    , update
    , view
    )

import Blockchain.Main as Blockchain exposing (Blockchain)
import Blockchain.User.Main as User
import Data.CDP as CDP exposing (CDP)
import Data.Images exposing (Images)
import Data.Or exposing (Or(..))
import Data.Pair as Pair
import Data.Pool exposing (Pool)
import Data.Remote as Remote exposing (Remote(..))
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
import Page.Transaction.Liquidity.New.Answer as Answer
import Page.Transaction.Liquidity.New.Disabled as Disabled
import Page.Transaction.Liquidity.New.Error exposing (Error)
import Page.Transaction.Liquidity.New.Query as Query
import Page.Transaction.Liquidity.New.Tooltip as Tooltip exposing (Tooltip)
import Page.Transaction.MaxButton as MaxButton
import Page.Transaction.Output as Output
import Page.Transaction.SpotPrice exposing (SpotPrice)
import Page.Transaction.Textbox as Textbox
import Utility.Color as Color
import Utility.Input as Input


type Transaction
    = Transaction
        { assetIn : String
        , debtOut : String
        , collateralOut : String
        , liquidityOut : Remote Error LiquidityGivenNew
        , tooltip : Maybe Tooltip
        }


type alias LiquidityGivenNew =
    { liquidityOut : Uint
    , apr : Float
    , cdp : CDP
    }


type Msg
    = InputAssetIn String
    | InputMaxAsset
    | InputDebtOut String
    | InputCollateralOut String
    | InputMaxCollateral
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
    { assetIn = ""
    , debtOut = ""
    , collateralOut = ""
    , liquidityOut =
        initGivenNew
            |> Success
    , tooltip = Nothing
    }
        |> Transaction


initGivenNew : LiquidityGivenNew
initGivenNew =
    { liquidityOut = Uint.zero
    , apr = 0
    , cdp = CDP.init
    }


fromDisabled :
    Blockchain
    -> Pool
    -> SpotPrice
    -> Disabled.Transaction
    -> ( Transaction, Cmd Msg )
fromDisabled blockchain pool spot transaction =
    if
        (transaction.assetIn |> Input.isZero)
            || (transaction.debtOut |> Input.isZero)
            || (transaction.collateralOut |> Input.isZero)
    then
        { assetIn = transaction.assetIn
        , debtOut = transaction.debtOut
        , collateralOut = transaction.collateralOut
        , liquidityOut = initGivenNew |> Success
        , tooltip = Nothing
        }
            |> noCmd

    else
        { assetIn = transaction.assetIn
        , debtOut = transaction.debtOut
        , collateralOut = transaction.collateralOut
        , liquidityOut = Loading
        , tooltip = Nothing
        }
            |> initQuery blockchain pool spot


toDisabled : Transaction -> Disabled.Transaction
toDisabled (Transaction { assetIn, debtOut, collateralOut }) =
    { assetIn = assetIn
    , debtOut = debtOut
    , collateralOut = collateralOut
    }


update :
    Blockchain
    -> Pool
    -> SpotPrice
    -> Msg
    -> Transaction
    -> ( Transaction, Cmd Msg, Maybe Effect )
update blockchain pool spot msg (Transaction transaction) =
    case msg of
        InputAssetIn assetIn ->
            if assetIn |> Uint.isAmount (pool.pair |> Pair.toAsset) then
                { transaction
                    | assetIn = assetIn
                    , liquidityOut =
                        if
                            (assetIn |> Input.isZero)
                                || (transaction.debtOut |> Input.isZero)
                                || (transaction.collateralOut |> Input.isZero)
                        then
                            initGivenNew
                                |> Success

                        else
                            Loading
                }
                    |> query blockchain pool spot

            else
                transaction |> noCmdAndEffect

        InputMaxAsset ->
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
                            , liquidityOut =
                                if
                                    (assetIn |> Input.isZero)
                                        || (transaction.debtOut |> Input.isZero)
                                        || (transaction.collateralOut |> Input.isZero)
                                then
                                    initGivenNew
                                        |> Success

                                else
                                    Loading
                        }
                            |> query blockchain pool spot
                    )
                |> Maybe.withDefault (transaction |> noCmdAndEffect)

        InputDebtOut debtOut ->
            if debtOut |> Uint.isAmount (pool.pair |> Pair.toAsset) then
                { transaction
                    | debtOut = debtOut
                    , liquidityOut =
                        if
                            (transaction.assetIn |> Input.isZero)
                                || (debtOut |> Input.isZero)
                                || (transaction.collateralOut |> Input.isZero)
                        then
                            initGivenNew
                                |> Success

                        else
                            Loading
                }
                    |> query blockchain pool spot

            else
                transaction |> noCmdAndEffect

        InputCollateralOut collateralOut ->
            if collateralOut |> Uint.isAmount (pool.pair |> Pair.toCollateral) then
                { transaction
                    | collateralOut = collateralOut
                    , liquidityOut =
                        if
                            (transaction.assetIn |> Input.isZero)
                                || (transaction.debtOut |> Input.isZero)
                                || (collateralOut |> Input.isZero)
                        then
                            initGivenNew
                                |> Success

                        else
                            Loading
                }
                    |> query blockchain pool spot

            else
                transaction |> noCmdAndEffect

        InputMaxCollateral ->
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
                            | collateralOut = collateralOut
                            , liquidityOut =
                                if
                                    (transaction.assetIn |> Input.isZero)
                                        || (transaction.debtOut |> Input.isZero)
                                        || (collateralOut |> Input.isZero)
                                then
                                    initGivenNew
                                        |> Success

                                else
                                    Loading
                        }
                            |> query blockchain pool spot
                    )
                |> Maybe.withDefault (transaction |> noCmdAndEffect)

        ClickConnect ->
            blockchain
                |> Blockchain.toUser
                |> Maybe.map (\_ -> transaction |> noCmdAndEffect)
                |> Maybe.withDefault
                    ( transaction |> Transaction
                    , Cmd.none
                    , OpenConnect |> Just
                    )

        ClickApproveAsset ->
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
                            |> approveCreate
                        , OpenConfirm |> Just
                        )
                            |> Just

                    else
                        Nothing

                _ ->
                    Nothing
            )
                |> Maybe.withDefault (transaction |> noCmdAndEffect)

        ClickApproveCollateral ->
            (case
                ( blockchain |> Blockchain.toUser
                , transaction.collateralOut
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
                            |> approveCreate
                        , OpenConfirm |> Just
                        )
                            |> Just

                    else
                        Nothing

                _ ->
                    Nothing
            )
                |> Maybe.withDefault (transaction |> noCmdAndEffect)

        ReceiveAnswer value ->
            (case value |> Decode.decodeValue Answer.decoder of
                Ok answer ->
                    if
                        (answer.chainId == (blockchain |> Blockchain.toChain))
                            && (answer.pool == pool)
                            && (Just answer.assetIn
                                    == (transaction.assetIn
                                            |> Uint.fromAmount
                                                (pool.pair |> Pair.toAsset)
                                       )
                               )
                            && (Just answer.debtOut
                                    == (transaction.debtOut
                                            |> Uint.fromAmount
                                                (pool.pair |> Pair.toAsset)
                                       )
                               )
                            && (Just answer.collateralOut
                                    == (transaction.collateralOut
                                            |> Uint.fromAmount
                                                (pool.pair |> Pair.toCollateral)
                                       )
                               )
                    then
                        { transaction | liquidityOut = answer.result |> toRemote }
                            |> Just

                    else
                        Nothing

                Err _ ->
                    Nothing
            )
                |> Maybe.map noCmdAndEffect
                |> Maybe.withDefault (transaction |> noCmdAndEffect)

        OnMouseEnter tooltip ->
            { transaction | tooltip = Just tooltip }
                |> noCmdAndEffect

        OnMouseLeave ->
            { transaction | tooltip = Nothing }
                |> noCmdAndEffect


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
    { assetIn : String
    , debtOut : String
    , collateralOut : String
    , liquidityOut : Remote Error LiquidityGivenNew
    , tooltip : Maybe Tooltip
    }
    -> ( Transaction, Cmd Msg )
noCmd transaction =
    ( transaction |> Transaction
    , Cmd.none
    )


noCmdAndEffect :
    { assetIn : String
    , debtOut : String
    , collateralOut : String
    , liquidityOut : Remote Error LiquidityGivenNew
    , tooltip : Maybe Tooltip
    }
    -> ( Transaction, Cmd Msg, Maybe Effect )
noCmdAndEffect transaction =
    ( transaction |> Transaction
    , Cmd.none
    , Nothing
    )


initQuery :
    Blockchain
    -> Pool
    -> SpotPrice
    ->
        { assetIn : String
        , debtOut : String
        , collateralOut : String
        , liquidityOut : Remote Error LiquidityGivenNew
        , tooltip : Maybe Tooltip
        }
    -> ( Transaction, Cmd Msg )
initQuery =
    constructQueryNew queryCreate


query :
    Blockchain
    -> Pool
    -> SpotPrice
    ->
        { assetIn : String
        , debtOut : String
        , collateralOut : String
        , liquidityOut : Remote Error LiquidityGivenNew
        , tooltip : Maybe Tooltip
        }
    -> ( Transaction, Cmd Msg, Maybe Effect )
query blockchain pool spot transaction =
    transaction
        |> constructQueryNew queryCreate
            blockchain
            pool
            spot
        |> (\( updated, cmd ) ->
                ( updated
                , cmd
                , Nothing
                )
           )


constructQueryNew :
    (Value -> Cmd Msg)
    -> Blockchain
    -> Pool
    -> SpotPrice
    ->
        { assetIn : String
        , debtOut : String
        , collateralOut : String
        , liquidityOut : Remote Error LiquidityGivenNew
        , tooltip : Maybe Tooltip
        }
    -> ( Transaction, Cmd Msg )
constructQueryNew givenCmd blockchain pool spot transaction =
    (if
        (transaction.assetIn |> Input.isZero)
            || (transaction.debtOut |> Input.isZero)
            || (transaction.collateralOut |> Input.isZero)
     then
        Nothing

     else
        case
            ( transaction.assetIn
                |> Uint.fromAmount
                    (pool.pair |> Pair.toAsset)
            , transaction.debtOut
                |> Uint.fromAmount
                    (pool.pair |> Pair.toAsset)
            , transaction.collateralOut
                |> Uint.fromAmount
                    (pool.pair |> Pair.toCollateral)
            )
        of
            ( Just assetIn, Just debtIn, Just collateralIn ) ->
                { chainId = blockchain |> Blockchain.toChain
                , pool = pool
                , spot = spot
                , assetIn = assetIn
                , debtOut = debtIn
                , collateralOut = collateralIn
                }
                    |> Query.givenNew
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


port queryCreate : Value -> Cmd msg


port approveCreate : Value -> Cmd msg


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
            |> assetInSection model blockchain (pool.pair |> Pair.toAsset)
    , second =
        transaction
            |> duesOutSection model blockchain pool
    , third =
        transaction
            |> liquidityOutSection model pool
    , buttons = none |> Debug.log "later"
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
            , onClick = Nothing
            , onChange = InputAssetIn
            , text = Left assetIn
            , description = "asset in textbox"
            }
        ]


duesOutSection :
    { model | spot : Spot, images : Images }
    -> Blockchain
    -> Pool
    ->
        { transaction
            | debtOut : String
            , collateralOut : String
            , liquidityOut : Remote Error LiquidityGivenNew
            , tooltip : Maybe Tooltip
        }
    -> Element Msg
duesOutSection model blockchain pool ({ debtOut, collateralOut, liquidityOut, tooltip } as transaction) =
    column
        [ Region.description "dues"
        , width <| px 343
        , height shrink
        , padding 16
        , spacing 12
        ]
        [ row
            [ width fill
            , height shrink
            , spacing 16
            ]
            ((case liquidityOut of
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
            [ debtOut
                |> debtOutSection model
                    (pool.pair |> Pair.toAsset)
                    transaction
            , collateralOut
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
    -> String
    -> Element Msg
debtOutSection model asset { tooltip } input =
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
            , onClick = Nothing
            , onChange = InputDebtOut
            , text = Left input
            , description = "debt out textbox"
            }
        ]


collateralOutSection :
    { model | images : Images }
    -> Blockchain
    -> Token
    -> { transaction | tooltip : Maybe Tooltip }
    -> String
    -> Element Msg
collateralOutSection model blockchain collateral { tooltip } input =
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
            , onClick = Nothing
            , onChange = InputCollateralOut
            , text = Left input
            , description = "collateral out textbox"
            }
        ]


liquidityOutSection :
    { model | images : Images }
    -> Pool
    -> { transaction | liquidityOut : Remote Error LiquidityGivenNew }
    -> Element Msg
liquidityOutSection model pool { liquidityOut } =
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
                liquidityOut
                    |> Remote.map .liquidityOut
            , description = "liquidity out"
            }
        ]
