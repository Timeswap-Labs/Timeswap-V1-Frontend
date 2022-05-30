port module Page.Transaction.Liquidity.New.Main exposing
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
import Blockchain.User.WriteCreate exposing (WriteCreate)
import Data.CDP as CDP exposing (CDP)
import Data.ERC20 exposing (ERC20)
import Data.Images exposing (Images)
import Data.Or exposing (Or(..))
import Data.Pair as Pair exposing (Pair)
import Data.Pool exposing (Pool)
import Data.PriceFeed exposing (PriceFeed)
import Data.Remote as Remote exposing (Remote(..))
import Data.Theme exposing (Theme)
import Data.Token as Token exposing (Token)
import Data.Uint as Uint exposing (Uint)
import Element
    exposing
        ( Element
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
import Page.Transaction.Button as Button
import Page.Transaction.Info as Info
import Page.Transaction.Liquidity.New.Disabled as Disabled
import Page.Transaction.Liquidity.New.Error exposing (Error)
import Page.Transaction.Liquidity.New.Query as Query
import Page.Transaction.Liquidity.New.Tooltip as Tooltip exposing (Tooltip)
import Page.Transaction.MaxButton as MaxButton
import Page.Transaction.Output as Output
import Page.Transaction.Price exposing (Price)
import Page.Transaction.Textbox as Textbox
import Time exposing (Posix)
import Utility.Color as Color
import Utility.Input as Input
import Utility.Loading as Loading
import Utility.ThemeColor as ThemeColor


type Transaction
    = Transaction
        { assetIn : String
        , debtIn : String
        , collateralIn : String
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
    | ClickCreate
    | ReceiveAnswer Value
    | Tick Posix
    | OnMouseEnter Tooltip
    | OnMouseLeave


type Effect
    = OpenConnect
    | Approve ERC20
    | Create WriteCreate


init : Transaction
init =
    { assetIn = ""
    , debtIn = ""
    , collateralIn = ""
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
    -> Price
    -> Disabled.Transaction
    -> ( Transaction, Cmd Msg )
fromDisabled blockchain pool priceFeed transaction =
    if
        (transaction.assetIn |> Input.isZero)
            || (transaction.debtIn |> Input.isZero)
            || (transaction.collateralIn |> Input.isZero)
    then
        { assetIn = transaction.assetIn
        , debtIn = transaction.debtIn
        , collateralIn = transaction.collateralIn
        , liquidityOut = initGivenNew |> Success
        , tooltip = Nothing
        }
            |> noCmd

    else
        { assetIn = transaction.assetIn
        , debtIn = transaction.debtIn
        , collateralIn = transaction.collateralIn
        , liquidityOut = Remote.loading
        , tooltip = Nothing
        }
            |> initQuery blockchain pool priceFeed


toDisabled : Transaction -> Disabled.Transaction
toDisabled (Transaction { assetIn, debtIn, collateralIn }) =
    { assetIn = assetIn
    , debtIn = debtIn
    , collateralIn = collateralIn
    }


update :
    Blockchain
    -> Pool
    -> Price
    -> Msg
    -> Transaction
    -> ( Transaction, Cmd Msg, Maybe Effect )
update blockchain pool priceFeed msg (Transaction transaction) =
    case msg of
        InputAssetIn assetIn ->
            if assetIn |> Uint.isAmount (pool.pair |> Pair.toAsset) then
                { transaction
                    | assetIn = assetIn
                    , liquidityOut =
                        if
                            (assetIn |> Input.isZero)
                                || (transaction.debtIn |> Input.isZero)
                                || (transaction.collateralIn |> Input.isZero)
                        then
                            initGivenNew
                                |> Success

                        else
                            Remote.loading
                }
                    |> query blockchain pool priceFeed

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
                                        || (transaction.debtIn |> Input.isZero)
                                        || (transaction.collateralIn |> Input.isZero)
                                then
                                    initGivenNew
                                        |> Success

                                else
                                    Remote.loading
                        }
                            |> query blockchain pool priceFeed
                    )
                |> Maybe.withDefault (transaction |> noCmdAndEffect)

        InputDebtOut debtIn ->
            if debtIn |> Uint.isAmount (pool.pair |> Pair.toAsset) then
                { transaction
                    | debtIn = debtIn
                    , liquidityOut =
                        if
                            (transaction.assetIn |> Input.isZero)
                                || (debtIn |> Input.isZero)
                                || (transaction.collateralIn |> Input.isZero)
                        then
                            initGivenNew
                                |> Success

                        else
                            Remote.loading
                }
                    |> query blockchain pool priceFeed

            else
                transaction |> noCmdAndEffect

        InputCollateralOut collateralIn ->
            if collateralIn |> Uint.isAmount (pool.pair |> Pair.toCollateral) then
                { transaction
                    | collateralIn = collateralIn
                    , liquidityOut =
                        if
                            (transaction.assetIn |> Input.isZero)
                                || (transaction.debtIn |> Input.isZero)
                                || (collateralIn |> Input.isZero)
                        then
                            initGivenNew
                                |> Success

                        else
                            Remote.loading
                }
                    |> query blockchain pool priceFeed

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
                    (\collateralIn ->
                        { transaction
                            | collateralIn = collateralIn
                            , liquidityOut =
                                if
                                    (transaction.assetIn |> Input.isZero)
                                        || (transaction.debtIn |> Input.isZero)
                                        || (collateralIn |> Input.isZero)
                                then
                                    initGivenNew
                                        |> Success

                                else
                                    Remote.loading
                        }
                            |> query blockchain pool priceFeed
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

        ClickApproveCollateral ->
            (case
                ( blockchain |> Blockchain.toUser
                , transaction.collateralIn
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

        ClickCreate ->
            (case
                ( blockchain |> Blockchain.toUser
                , ( transaction.assetIn
                        |> Uint.fromAmount
                            (pool.pair |> Pair.toAsset)
                  , transaction.debtIn
                        |> Uint.fromAmount
                            (pool.pair |> Pair.toAsset)
                  , transaction.collateralIn
                        |> Uint.fromAmount
                            (pool.pair |> Pair.toCollateral)
                  )
                )
             of
                ( Just user, ( Just assetIn, Just debtIn, Just collateralIn ) ) ->
                    if
                        (user
                            |> User.hasEnoughBalance
                                (pool.pair |> Pair.toAsset)
                                assetIn
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
                                                    collateralIn
                                        )
                                    |> Maybe.withDefault True
                               )
                    then
                        ( transaction |> Transaction
                        , Cmd.none
                        , { pool = pool
                          , assetIn = assetIn
                          , debtIn = debtIn
                          , collateralIn = collateralIn
                          }
                            |> Create
                            |> Just
                        )
                            |> Just

                    else
                        Nothing

                _ ->
                    Nothing
            )
                |> Maybe.withDefault (transaction |> noCmdAndEffect)

        ReceiveAnswer value ->
            (case value |> Decode.decodeValue Query.decoder of
                Ok answer ->
                    if
                        (answer.chain == (blockchain |> Blockchain.toChain))
                            && (answer.pool == pool)
                            && (Just answer.assetIn
                                    == (transaction.assetIn
                                            |> Uint.fromAmount
                                                (pool.pair |> Pair.toAsset)
                                       )
                               )
                            && (Just answer.debtIn
                                    == (transaction.debtIn
                                            |> Uint.fromAmount
                                                (pool.pair |> Pair.toAsset)
                                       )
                               )
                            && (Just answer.collateralIn
                                    == (transaction.collateralIn
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

        Tick posix ->
            { transaction
                | liquidityOut =
                    transaction.liquidityOut
                        |> Remote.update posix
            }
                |> noCmdAndEffect

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
    , debtIn : String
    , collateralIn : String
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
    , debtIn : String
    , collateralIn : String
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
    -> Price
    ->
        { assetIn : String
        , debtIn : String
        , collateralIn : String
        , liquidityOut : Remote Error LiquidityGivenNew
        , tooltip : Maybe Tooltip
        }
    -> ( Transaction, Cmd Msg )
initQuery =
    constructQueryNew queryCreate


query :
    Blockchain
    -> Pool
    -> Price
    ->
        { assetIn : String
        , debtIn : String
        , collateralIn : String
        , liquidityOut : Remote Error LiquidityGivenNew
        , tooltip : Maybe Tooltip
        }
    -> ( Transaction, Cmd Msg, Maybe Effect )
query blockchain pool priceFeed transaction =
    transaction
        |> constructQueryNew queryCreate
            blockchain
            pool
            priceFeed
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
    -> Price
    ->
        { assetIn : String
        , debtIn : String
        , collateralIn : String
        , liquidityOut : Remote Error LiquidityGivenNew
        , tooltip : Maybe Tooltip
        }
    -> ( Transaction, Cmd Msg )
constructQueryNew givenCmd blockchain pool price transaction =
    (if
        (transaction.assetIn |> Input.isZero)
            || (transaction.debtIn |> Input.isZero)
            || (transaction.collateralIn |> Input.isZero)
     then
        Nothing

     else
        case
            ( transaction.assetIn
                |> Uint.fromAmount
                    (pool.pair |> Pair.toAsset)
            , transaction.debtIn
                |> Uint.fromAmount
                    (pool.pair |> Pair.toAsset)
            , transaction.collateralIn
                |> Uint.fromAmount
                    (pool.pair |> Pair.toCollateral)
            )
        of
            ( Just assetIn, Just debtIn, Just collateralIn ) ->
                { chain = blockchain |> Blockchain.toChain
                , pool = pool
                , price = price
                , assetIn = assetIn
                , debtIn = debtIn
                , collateralIn = collateralIn
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


port receiveNewLiqAnswer : (Value -> msg) -> Sub msg


subscriptions : Transaction -> Sub Msg
subscriptions (Transaction transaction) =
    [ transaction.liquidityOut
        |> Remote.subscriptions Tick
    , receiveNewLiqAnswer ReceiveAnswer
    ]
        |> Sub.batch


view :
    { model | priceFeed : PriceFeed, images : Images, theme : Theme }
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
            |> liqOutSection model pool
    , buttons = buttons model.theme blockchain pool.pair transaction
    }


assetInSection :
    { model | images : Images, theme : Theme }
    -> Blockchain
    -> Token
    -> { transaction | assetIn : String, tooltip : Maybe Tooltip }
    -> Element Msg
assetInSection model blockchain asset { assetIn, tooltip } =
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
            , token = asset
            , onClick = Nothing
            , onChange = InputAssetIn
            , text = Left assetIn
            , description = "asset in textbox"
            }
        ]


duesOutSection :
    { model | priceFeed : PriceFeed, images : Images, theme : Theme }
    -> Blockchain
    -> Pool
    ->
        { transaction
            | assetIn : String
            , debtIn : String
            , collateralIn : String
            , liquidityOut : Remote Error LiquidityGivenNew
            , tooltip : Maybe Tooltip
        }
    -> Element Msg
duesOutSection model blockchain pool ({ assetIn, debtIn, collateralIn, liquidityOut, tooltip } as transaction) =
    column
        [ Region.description "dues"
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
            [ model.theme
                |> Info.lendAPR
                    (liquidityOut
                        |> Remote.map .apr
                    )
                    assetIn
                    Nothing
            , liquidityOut
                |> Remote.map .cdp
                |> (\cdp ->
                        Info.lendCDP model
                            { onMouseEnter = OnMouseEnter
                            , onMouseLeave = OnMouseLeave
                            , cdpTooltip = Tooltip.CDP
                            , symbolTooltip = Tooltip.CDPSymbol
                            , opened = tooltip
                            , pair = pool.pair
                            , cdp = cdp
                            , poolInfo = Nothing
                            , assetIn = assetIn
                            }
                   )
            ]
        , column
            [ width fill
            , height shrink
            , spacing 12
            ]
            [ debtIn
                |> debtOutSection model
                    (pool.pair |> Pair.toAsset)
                    transaction
            , collateralIn
                |> collateralOutSection model
                    blockchain
                    (pool.pair |> Pair.toCollateral)
                    transaction
            ]
        ]


debtOutSection :
    { model | images : Images, theme : Theme }
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
            , paddingXY 0 3
            , model.theme |> ThemeColor.actionElemLabel |> Font.color
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
    { model | images : Images, theme : Theme }
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
                , paddingXY 0 3
                , model.theme |> ThemeColor.actionElemLabel |> Font.color
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
            , onClick = Nothing
            , onChange = InputCollateralOut
            , text = Left input
            , description = "collateral out textbox"
            }
        ]


liqOutSection :
    { model | images : Images, theme : Theme }
    -> Pool
    -> { transaction | liquidityOut : Remote Error LiquidityGivenNew }
    -> Element Msg
liqOutSection model pool { liquidityOut } =
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
            , case liquidityOut of
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
        , Output.liquidity model
            { pair = pool.pair
            , output =
                liquidityOut
                    |> Remote.map .liquidityOut
            , description = "liquidity out"
            }
        ]


buttons :
    Theme
    -> Blockchain
    -> Pair
    -> { transaction | assetIn : String, debtIn : String, collateralIn : String, liquidityOut : Remote Error LiquidityGivenNew }
    -> Element Msg
buttons theme blockchain pair { assetIn, debtIn, collateralIn, liquidityOut } =
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
                        ( assetIn |> Uint.fromAmount (pair |> Pair.toAsset)
                        , debtIn |> Uint.fromAmount (pair |> Pair.toAsset)
                        , collateralIn |> Uint.fromAmount (pair |> Pair.toCollateral)
                        )
                    of
                        ( Just assetUint, Just debtUint, Just collateralUint ) ->
                            if
                                (assetUint |> Uint.isZero)
                                    || (debtUint |> Uint.isZero)
                                    || (collateralUint |> Uint.isZero)
                            then
                                [ Button.disabled theme "Create Pool" |> map never ]

                            else if
                                (assetUint |> Uint.isZero |> not)
                                    && (debtUint |> Uint.isZero |> not)
                                    && Uint.hasEnough debtUint assetUint
                            then
                                [ Button.customError "Debt should be higher than Asset" |> map never ]

                            else
                                case
                                    ( pair |> Pair.toAsset |> Token.toERC20
                                    , pair |> Pair.toCollateral |> Token.toERC20
                                    , liquidityOut
                                    )
                                of
                                    ( Just assetErc20, Just collErc20, Success _ ) ->
                                        case
                                            ( user
                                                |> User.getAllowance assetErc20
                                                |> (Maybe.map << Remote.map)
                                                    (Uint.hasEnough assetUint)
                                            , user
                                                |> User.getAllowance collErc20
                                                |> (Maybe.map << Remote.map)
                                                    (Uint.hasEnough collateralUint)
                                            )
                                        of
                                            ( Just (Success True), Just (Success True) ) ->
                                                case
                                                    ( user
                                                        |> User.getBalance (pair |> Pair.toAsset)
                                                        |> (Maybe.map << Remote.map)
                                                            (Uint.hasEnough assetUint)
                                                    , user
                                                        |> User.getBalance (pair |> Pair.toCollateral)
                                                        |> (Maybe.map << Remote.map)
                                                            (Uint.hasEnough collateralUint)
                                                    )
                                                of
                                                    ( Just (Success True), Just (Success True) ) ->
                                                        [ createPoolButton theme ]

                                                    ( Just (Success False), Just (Success True) ) ->
                                                        [ insufficientTokenBalance (pair |> Pair.toAsset) ]

                                                    ( Just (Success True), Just (Success False) ) ->
                                                        [ insufficientTokenBalance (pair |> Pair.toCollateral) ]

                                                    ( Just (Success False), Just (Success False) ) ->
                                                        [ insufficientTokenBalance (pair |> Pair.toAsset)
                                                        , insufficientTokenBalance (pair |> Pair.toCollateral)
                                                        ]

                                                    _ ->
                                                        [ Button.disabled theme "Create Pool" |> map never ]

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
                                                , Button.disabled theme "Create Pool" |> map never
                                                ]

                                            ( Just (Loading _), Just (Success True) ) ->
                                                [ theme |> Button.checkingAllowance |> map never
                                                , Button.disabled theme "Create Pool" |> map never
                                                ]

                                            ( Just (Loading _), Just (Loading _) ) ->
                                                [ theme |> Button.checkingAllowance |> map never
                                                , Button.disabled theme "Create Pool" |> map never
                                                ]

                                            ( Just (Failure error), _ ) ->
                                                [ Button.error error |> map never ]

                                            ( _, Just (Failure error) ) ->
                                                [ Button.error error |> map never ]

                                            _ ->
                                                [ Button.disabled theme "Create Pool" |> map never ]

                                    ( Just assetErc20, Nothing, Success _ ) ->
                                        case
                                            user
                                                |> User.getAllowance assetErc20
                                                |> (Maybe.map << Remote.map)
                                                    (Uint.hasEnough assetUint)
                                        of
                                            Just (Success True) ->
                                                case
                                                    user
                                                        |> User.getBalance (pair |> Pair.toAsset)
                                                        |> (Maybe.map << Remote.map)
                                                            (Uint.hasEnough assetUint)
                                                of
                                                    Just (Success True) ->
                                                        [ createPoolButton theme ]

                                                    Just (Success False) ->
                                                        [ insufficientTokenBalance (pair |> Pair.toAsset) ]

                                                    Just (Loading _) ->
                                                        [ Button.checkingBalance theme |> map never ]

                                                    _ ->
                                                        []

                                            Just (Success False) ->
                                                [ Button.approveAsset ClickApproveAsset theme ]

                                            Just (Loading _) ->
                                                [ Button.checkingAllowance theme |> map never ]

                                            _ ->
                                                []

                                    ( Nothing, Just collErc20, Success _ ) ->
                                        case
                                            user
                                                |> User.getAllowance collErc20
                                                |> (Maybe.map << Remote.map)
                                                    (Uint.hasEnough collateralUint)
                                        of
                                            Just (Success True) ->
                                                case
                                                    user
                                                        |> User.getBalance (pair |> Pair.toCollateral)
                                                        |> (Maybe.map << Remote.map)
                                                            (Uint.hasEnough collateralUint)
                                                of
                                                    Just (Success True) ->
                                                        [ createPoolButton theme ]

                                                    Just (Success False) ->
                                                        [ insufficientTokenBalance (pair |> Pair.toCollateral) ]

                                                    Just (Loading _) ->
                                                        [ Button.checkingBalance theme |> map never ]

                                                    _ ->
                                                        []

                                            Just (Success False) ->
                                                [ Button.approveCollateral ClickApproveCollateral theme ]

                                            Just (Loading _) ->
                                                [ Button.checkingAllowance theme |> map never ]

                                            _ ->
                                                [ Button.disabled theme "Create Pool" |> map never ]

                                    -- ( _, _, Err err ) ->
                                    --     [ Button.customError (err |> Error.toString) |> map never ]
                                    _ ->
                                        [ Button.disabled theme "Create Pool" |> map never ]

                        _ ->
                            [ Button.disabled theme "Create Pool" |> map never ]
                )
            |> Maybe.withDefault
                [ Button.connect theme ClickConnect ]
        )


createPoolButton : Theme -> Element Msg
createPoolButton theme =
    Button.view
        { onPress = ClickCreate
        , text = "Create Pool"
        , theme = theme
        }


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
