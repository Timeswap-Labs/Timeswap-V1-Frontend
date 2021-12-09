module Page.Transaction.Lend.Main exposing
    ( Effect(..)
    , Msg
    , Transaction
    , init
    , initGivenPoolInfo
    , notSupported
    , subscriptions
    , toParameter
    , toPoolInfo
    , update
    , view
    )

import Blockchain.Main as Blockchain exposing (Blockchain)
import Data.Backdrop exposing (Backdrop)
import Data.Chain exposing (Chain)
import Data.Chains exposing (Chains)
import Data.ChosenZone exposing (ChosenZone)
import Data.Deadline exposing (Deadline)
import Data.Images exposing (Images)
import Data.Maturity as Maturity
import Data.Offset exposing (Offset)
import Data.Pair as Pair exposing (Pair)
import Data.Parameter as Parameter exposing (Parameter)
import Data.Pool exposing (Pool)
import Data.Remote exposing (Remote(..))
import Data.Slippage exposing (Slippage)
import Data.Token exposing (Token)
import Data.TokenParam as TokenParam exposing (TokenParam)
import Data.WebError as WebError exposing (WebError)
import Element
    exposing
        ( Element
        , alignRight
        , centerY
        , column
        , el
        , fill
        , height
        , map
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
import Http
import Page.Transaction.Answer as Answer exposing (Answer)
import Page.Transaction.Lend.Lend.Main as Lend
import Page.Transaction.Lend.LendError as LendError
import Page.Transaction.MaturityButton as MaturityButton
import Page.Transaction.PoolInfo exposing (PoolInfo)
import Page.Transaction.Query as Query
import Page.Transaction.TokenButton as TokenButton
import Page.Transaction.Tooltip as Tooltip exposing (Tooltip)
import Process
import Task
import Time exposing (Posix)
import Utility.Color as Color
import Utility.Glass as Glass
import Utility.Image as Image


type Transaction
    = Transaction
        { state : State
        , tooltip : Maybe Tooltip
        }


type State
    = None
    | Asset Token
    | Collateral Token
    | Pair Pair
    | Pool Pool Status


type Status
    = Active (Remote Error PoolState)
    | Matured


type alias Error =
    { web : WebError
    , lend : LendError.Transaction
    }


type PoolState
    = Exist PoolInfo Lend.Transaction
    | DoesNotExist


type Msg
    = SelectToken TokenParam
    | SelectMaturity
    | QueryAgain
    | ReceiveAnswer Chain Pool (Result Http.Error Answer)
    | CheckMaturity Posix
    | LendMsg Lend.Msg
    | ClickSettings
    | OnMouseEnter Tooltip
    | OnMouseLeave


type Effect
    = OpenTokenList TokenParam
    | OpenMaturityList Pair
    | OpenConnect
    | OpenSettings
    | OpenConfirm


init :
    { model | time : Posix, chains : Chains }
    -> Blockchain
    -> Maybe Parameter
    -> ( Transaction, Cmd Msg )
init ({ time } as model) blockchain parameter =
    case parameter of
        Nothing ->
            ( { state = None
              , tooltip = Nothing
              }
                |> Transaction
            , Cmd.none
            )

        Just (Parameter.Asset asset) ->
            ( { state = Asset asset
              , tooltip = Nothing
              }
                |> Transaction
            , Cmd.none
            )

        Just (Parameter.Collateral collateral) ->
            ( { state = Collateral collateral
              , tooltip = Nothing
              }
                |> Transaction
            , Cmd.none
            )

        Just (Parameter.Pair pair) ->
            ( { state = Pair pair
              , tooltip = Nothing
              }
                |> Transaction
            , Cmd.none
            )

        Just (Parameter.Pool pool) ->
            if pool.maturity |> Maturity.isActive time then
                ( { state =
                        Loading
                            |> Active
                            |> Pool pool
                  , tooltip = Nothing
                  }
                    |> Transaction
                , get model blockchain pool
                )

            else
                ( { state = Matured |> Pool pool
                  , tooltip = Nothing
                  }
                    |> Transaction
                , Cmd.none
                )


initGivenPoolInfo :
    { model | time : Posix, chains : Chains }
    -> Blockchain
    -> Pool
    -> PoolInfo
    -> ( Transaction, Cmd Msg )
initGivenPoolInfo model blockchain pool poolInfo =
    ( { state =
            Lend.init
                |> Exist poolInfo
                |> Success
                |> Active
                |> Pool pool
      , tooltip = Nothing
      }
        |> Transaction
    , get model blockchain pool
    )


notSupported : Transaction
notSupported =
    { state = None
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
    -> Msg
    -> Transaction
    ->
        ( Transaction
        , Cmd Msg
        , Maybe Effect
        )
update model blockchain msg (Transaction transaction) =
    case ( msg, transaction.state ) of
        ( SelectToken tokenParam, _ ) ->
            ( transaction |> Transaction
            , Cmd.none
            , tokenParam
                |> OpenTokenList
                |> Just
            )

        ( SelectMaturity, Pair pair ) ->
            ( transaction |> Transaction
            , Cmd.none
            , pair
                |> OpenMaturityList
                |> Just
            )

        ( SelectMaturity, Pool pool _ ) ->
            ( transaction |> Transaction
            , Cmd.none
            , pool.pair
                |> OpenMaturityList
                |> Just
            )

        ( QueryAgain, Pool pool (Active (Success _)) ) ->
            ( transaction |> Transaction
            , get model blockchain pool
            , Nothing
            )

        ( QueryAgain, Pool pool (Active (Failure _)) ) ->
            ( transaction |> Transaction
            , get model blockchain pool
            , Nothing
            )

        ( ReceiveAnswer chain pool result, Pool currentPool (Active remote) ) ->
            ( (if
                (chain == (blockchain |> Blockchain.toChain))
                    && (pool == currentPool)
               then
                case
                    ( result |> WebError.toResult
                    , remote
                    )
                of
                    ( Just (Ok (Just poolInfo)), Success (Exist _ lend) ) ->
                        lend
                            |> Exist poolInfo
                            |> Success
                            |> Just

                    ( Just (Ok (Just poolInfo)), Success DoesNotExist ) ->
                        Lend.init
                            |> Exist poolInfo
                            |> Success
                            |> Just

                    ( Just (Ok (Just poolInfo)), Failure error ) ->
                        error.lend
                            |> Lend.fromLendError
                            |> Exist poolInfo
                            |> Success
                            |> Just

                    ( Just (Ok (Just poolInfo)), Loading ) ->
                        Lend.init
                            |> Exist poolInfo
                            |> Success
                            |> Just

                    ( Just (Ok Nothing), _ ) ->
                        DoesNotExist
                            |> Success
                            |> Just

                    ( Just (Err webError), Success (Exist _ lend) ) ->
                        { web = webError
                        , lend = lend |> Lend.toLendError
                        }
                            |> Failure
                            |> Just

                    ( Just (Err webError), Success DoesNotExist ) ->
                        { web = webError
                        , lend = LendError.init
                        }
                            |> Failure
                            |> Just

                    ( Just (Err webError), Failure failure ) ->
                        { failure | web = webError }
                            |> Failure
                            |> Just

                    ( Just (Err webError), Loading ) ->
                        { web = webError
                        , lend = LendError.init
                        }
                            |> Failure
                            |> Just

                    ( Nothing, _ ) ->
                        Nothing

               else
                Nothing
              )
                |> Maybe.map
                    (\state ->
                        { transaction
                            | state =
                                state
                                    |> Active
                                    |> Pool pool
                        }
                    )
                |> Maybe.withDefault transaction
                |> Transaction
            , Process.sleep 5000
                |> Task.perform (\_ -> QueryAgain)
            , Nothing
            )

        ( CheckMaturity posix, Pool pool (Active _) ) ->
            (if pool.maturity |> Maturity.isActive posix then
                transaction

             else
                { transaction
                    | state =
                        Matured
                            |> Pool pool
                }
            )
                |> noCmdAndEffect

        ( LendMsg lendMsg, Pool pool (Active (Success (Exist poolInfo lend))) ) ->
            lend
                |> Lend.update model blockchain pool poolInfo lendMsg
                |> (\( updated, cmd, maybeEffect ) ->
                        ( { transaction
                            | state =
                                updated
                                    |> Exist poolInfo
                                    |> Success
                                    |> Active
                                    |> Pool pool
                          }
                            |> Transaction
                        , cmd |> Cmd.map LendMsg
                        , maybeEffect |> Maybe.map lendEffects
                        )
                   )

        ( ClickSettings, _ ) ->
            ( transaction |> Transaction
            , Cmd.none
            , OpenSettings |> Just
            )

        ( OnMouseEnter tooltip, _ ) ->
            { transaction | tooltip = Just tooltip }
                |> noCmdAndEffect

        ( OnMouseLeave, _ ) ->
            { transaction | tooltip = Nothing }
                |> noCmdAndEffect

        _ ->
            transaction |> noCmdAndEffect


lendEffects : Lend.Effect -> Effect
lendEffects effect =
    case effect of
        Lend.OpenConnect ->
            OpenConnect

        Lend.OpenConfirm ->
            OpenConfirm


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


get :
    { model | chains : Chains }
    -> Blockchain
    -> Pool
    -> Cmd Msg
get model blockchain pool =
    blockchain
        |> Blockchain.toChain
        |> (\chain ->
                Http.get
                    { url =
                        pool
                            |> Query.toUrlString chain
                    , expect =
                        pool
                            |> Answer.decoder model chain
                            |> Http.expectJson
                                (ReceiveAnswer chain pool)
                    }
           )


subscriptions : Transaction -> Sub Msg
subscriptions (Transaction { state }) =
    [ Time.every 1000 CheckMaturity
    , case state of
        Pool _ (Active (Success (Exist _ lend))) ->
            lend
                |> Lend.subscriptions
                |> Sub.map LendMsg

        _ ->
            Sub.none
    ]
        |> Sub.batch


toParameter : Transaction -> Maybe Parameter
toParameter (Transaction { state }) =
    case state of
        None ->
            Nothing

        Asset asset ->
            Parameter.Asset asset
                |> Just

        Collateral collateral ->
            Parameter.Collateral collateral
                |> Just

        Pair pair ->
            Parameter.Pair pair
                |> Just

        Pool pool _ ->
            Parameter.Pool pool
                |> Just


toPoolInfo : Transaction -> Maybe PoolInfo
toPoolInfo (Transaction { state }) =
    case state of
        Pool _ (Active (Success (Exist poolInfo _))) ->
            poolInfo |> Just

        _ ->
            Nothing


view :
    { model
        | time : Posix
        , offset : Offset
        , chosenZone : ChosenZone
        , backdrop : Backdrop
        , images : Images
    }
    -> Transaction
    -> Element Msg
view ({ backdrop } as model) (Transaction transaction) =
    column
        [ Region.description "lend transaction"
        , width shrink
        , height shrink
        , padding 24
        , spacing 24
        , Border.rounded 8
        , Glass.background backdrop
        , Border.width 1
        , Border.color Color.transparent100
        ]
        [ row
            [ width fill
            , height shrink
            ]
            [ el
                [ width shrink
                , height shrink
                , paddingXY 0 4
                , Font.size 24
                , Font.color Color.light100
                , Font.bold
                ]
                (text "Lend")
            , settingsButton model
            ]
        ]


settingsButton :
    { model | images : Images }
    -> Element Msg
settingsButton { images } =
    Input.button
        [ width shrink
        , height shrink
        , alignRight
        , centerY
        ]
        { onPress = Just ClickSettings
        , label =
            images
                |> Image.option
                    [ width <| px 20
                    , height <| px 20
                    ]
        }


parameters :
    { model
        | time : Posix
        , offset : Offset
        , chosenZone : ChosenZone
        , backdrop : Backdrop
        , images : Images
    }
    ->
        { transaction
            | state : State
            , tooltip : Maybe Tooltip
        }
    -> Element Msg
parameters model transaction =
    column
        [ Region.description "pool parameters"
        , width <| px 343
        , height shrink
        , padding 16
        , spacing 12
        , Background.color Color.primary100
        , Border.rounded 8
        ]
        [ pairParameters model transaction
        , maturityParameter model transaction
        ]


pairParameters :
    { model | images : Images }
    ->
        { transaction
            | state : State
            , tooltip : Maybe Tooltip
        }
    -> Element Msg
pairParameters model transaction =
    row
        [ width fill
        , height shrink
        , spacing 16
        ]
        [ tokenParameter model transaction TokenParam.Asset
        , tokenParameter model transaction TokenParam.Collateral
        ]


tokenParameter :
    { model | images : Images }
    ->
        { transaction
            | state : State
            , tooltip : Maybe Tooltip
        }
    -> TokenParam
    -> Element Msg
tokenParameter model transaction tokenParam =
    column
        [ width fill
        , height shrink
        , spacing 8
        ]
        [ el
            [ width shrink
            , height shrink
            , paddingXY 0 3
            , Font.size 14
            , Font.color Color.primary400
            ]
            ((case tokenParam of
                TokenParam.Asset ->
                    "Asset"

                TokenParam.Collateral ->
                    "Collateral"
             )
                |> text
            )
        , tokenButton model transaction tokenParam
        ]


tokenButton :
    { model | images : Images }
    ->
        { transaction
            | state : State
            , tooltip : Maybe Tooltip
        }
    -> TokenParam
    -> Element Msg
tokenButton model { state, tooltip } tokenParam =
    (case ( state, tokenParam ) of
        ( Asset asset, TokenParam.Asset ) ->
            Just asset

        ( Collateral collateral, TokenParam.Collateral ) ->
            Just collateral

        ( Pair pair, TokenParam.Asset ) ->
            pair
                |> Pair.toAsset
                |> Just

        ( Pair pair, TokenParam.Collateral ) ->
            pair
                |> Pair.toCollateral
                |> Just

        ( Pool pool _, TokenParam.Asset ) ->
            pool.pair
                |> Pair.toAsset
                |> Just

        ( Pool pool _, TokenParam.Collateral ) ->
            pool.pair
                |> Pair.toCollateral
                |> Just

        _ ->
            Nothing
    )
        |> (\token ->
                TokenButton.view model
                    { onPress = SelectToken
                    , onMouseEnter = OnMouseEnter
                    , onMouseLeave = OnMouseLeave
                    , tooltip = Tooltip.Token
                    , opened = tooltip
                    , tokenParam = tokenParam
                    , token = token
                    }
           )


maturityParameter :
    { model
        | time : Posix
        , offset : Offset
        , chosenZone : ChosenZone
        , images : Images
    }
    ->
        { transaction
            | state : State
            , tooltip : Maybe Tooltip
        }
    -> Element Msg
maturityParameter model { state, tooltip } =
    column
        [ width fill
        , height shrink
        , spacing 8
        ]
        [ el
            [ width shrink
            , height shrink
            , paddingXY 0 3
            , Font.size 14
            , Font.color Color.primary400
            ]
            (text "Maturity")
        , case state of
            Pool pool _ ->
                MaturityButton.view model
                    { onPress = SelectMaturity
                    , onMouseEnter = OnMouseEnter
                    , onMouseLeave = OnMouseLeave
                    , tooltip = Tooltip.Maturity
                    , opened = tooltip
                    , maturity = Just pool.maturity
                    }

            Pair _ ->
                MaturityButton.view model
                    { onPress = SelectMaturity
                    , onMouseEnter = OnMouseEnter
                    , onMouseLeave = OnMouseLeave
                    , tooltip = Tooltip.Maturity
                    , opened = tooltip
                    , maturity = Nothing
                    }

            _ ->
                MaturityButton.disabled
                    |> map never
        ]
