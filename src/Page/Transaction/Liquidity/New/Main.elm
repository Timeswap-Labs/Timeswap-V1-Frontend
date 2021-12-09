port module Page.Transaction.Liquidity.New.Main exposing
    ( Effect(..)
    , Msg
    , Transaction
    , fromNewError
    , init
    , toNewError
    , update
    )

import Blockchain.Main as Blockchain exposing (Blockchain)
import Blockchain.User.Main as User exposing (User)
import Data.CDP as CDP exposing (CDP)
import Data.Chains exposing (Chains)
import Data.Deadline exposing (Deadline)
import Data.Images exposing (Images)
import Data.Pair as Pair
import Data.Pool exposing (Pool)
import Data.Remote exposing (Remote(..))
import Data.Slippage exposing (Slippage)
import Data.Token as Token exposing (Token)
import Data.Uint as Uint exposing (Uint)
import Element
    exposing
        ( Element
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
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Json.Decode as Decode
import Json.Encode exposing (Value)
import Page.Approve as Approve
import Page.Transaction.Liquidity.New.Answer as Answer
import Page.Transaction.Liquidity.New.Error exposing (Error)
import Page.Transaction.Liquidity.New.Query as Query
import Page.Transaction.Liquidity.New.Tooltip as Tooltip exposing (Tooltip)
import Page.Transaction.Liquidity.NewError as NewError
import Page.Transaction.Textbox as Textbox
import Time exposing (Posix)
import Utility.Color as Color
import Utility.Input as Input
import Utility.Truncate as Truncate


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


fromNewError : NewError.Transaction -> Transaction
fromNewError transaction =
    { assetIn = transaction.assetIn
    , debtOut = transaction.debtOut
    , collateralOut = transaction.collateralOut
    , liquidityOut = Loading
    , tooltip = Nothing
    }
        |> Transaction


toNewError : Transaction -> NewError.Transaction
toNewError (Transaction { assetIn, debtOut, collateralOut }) =
    { assetIn = assetIn
    , debtOut = debtOut
    , collateralOut = collateralOut
    }


update :
    { model
        | time : Posix
        , chains : Chains
        , slippage : Slippage
        , deadline : Deadline
    }
    -> Blockchain
    -> Pool
    -> Msg
    -> Transaction
    -> ( Transaction, Cmd Msg, Maybe Effect )
update model blockchain pool msg (Transaction transaction) =
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
                    |> query blockchain pool

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
                            |> Maybe.map
                                (Uint.toAmount
                                    (pool.pair |> Pair.toAsset)
                                )
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
                            |> query blockchain pool
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
                    |> query blockchain pool

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
                    |> query blockchain pool

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
                            |> Maybe.map
                                (Uint.toAmount
                                    (pool.pair |> Pair.toCollateral)
                                )
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
                            |> query blockchain pool
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
            (case value |> Decode.decodeValue (Answer.decoder model) of
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


query :
    Blockchain
    -> Pool
    ->
        { assetIn : String
        , debtOut : String
        , collateralOut : String
        , liquidityOut : Remote Error LiquidityGivenNew
        , tooltip : Maybe Tooltip
        }
    -> ( Transaction, Cmd Msg, Maybe Effect )
query =
    constructQueryNew queryCreate


constructQueryNew :
    (Value -> Cmd Msg)
    -> Blockchain
    -> Pool
    ->
        { assetIn : String
        , debtOut : String
        , collateralOut : String
        , liquidityOut : Remote Error LiquidityGivenNew
        , tooltip : Maybe Tooltip
        }
    -> ( Transaction, Cmd Msg, Maybe Effect )
constructQueryNew givenCmd blockchain pool transaction =
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
                , Nothing
                )
           )


port queryCreate : Value -> Cmd msg


port approveCreate : Value -> Cmd msg


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
            |> createAssetIn model blockchain (pool.pair |> Pair.toAsset)
    , second = none |> Debug.log "later"
    , buttons = none |> Debug.log "later"
    }


createAssetIn :
    { model | images : Images }
    -> Blockchain
    -> Token
    -> { transaction | tooltip : Maybe Tooltip }
    -> Element Msg
createAssetIn model blockchain asset ({ tooltip } as transaction) =
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
                                [ userBalance user asset transaction
                                , maxButton
                                ]
                            )
                        |> Maybe.withDefault
                            []
                   )
            )
        , Textbox.view model
            { onMouseEnter = OnMouseEnter
            , onMouseLeave = OnMouseLeave
            , tooltip = Tooltip.AssetInSymbol
            , opened = tooltip
            , token = asset
            , onChange = InputAssetIn
            , text = "" |> Debug.log "later"
            , description = "asset in textbox"
            }
        ]


userBalance :
    User
    -> Token
    -> { transaction | tooltip : Maybe Tooltip }
    -> Element Msg
userBalance user asset { tooltip } =
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
                    , Truncate.viewBalance
                        { onMouseEnter = OnMouseEnter
                        , onMouseLeave = OnMouseLeave
                        , tooltip = Tooltip.AssetBalance
                        , opened = tooltip
                        , token = asset
                        , balance = balance
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
        { onPress = Just InputMaxAsset
        , label = text "MAX"
        }


disabledCreate :
    { model | images : Images }
    -> Blockchain
    -> Pool
    -> Transaction
    ->
        { first : Element Never
        , second : Element Never
        }
disabledCreate model blockchain pool (Transaction transaction) =
    { first =
        transaction
            |> disabledCreateAssetIn
                model
                blockchain
                (pool.pair |> Pair.toAsset)
    , second = none |> Debug.log "later"
    }


disabledCreateAssetIn :
    { model | images : Images }
    -> Blockchain
    -> Token
    -> { transaction | assetIn : String }
    -> Element Never
disabledCreateAssetIn model blockchain asset transaction =
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
                    , Truncate.disabledBalance
                        { token = asset
                        , balance = balance
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
