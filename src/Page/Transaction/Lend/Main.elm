module Page.Transaction.Lend.Main exposing
    ( Effect(..)
    , Msg
    , Transaction
    , init
    , initGivenPoolInfo
    , initGivenSpot
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
import Data.ChosenZone exposing (ChosenZone)
import Data.Deadline exposing (Deadline)
import Data.Images exposing (Images)
import Data.Maturity as Maturity
import Data.Offset exposing (Offset)
import Data.Or exposing (Or(..))
import Data.Pair as Pair exposing (Pair)
import Data.Parameter as Parameter exposing (Parameter)
import Data.Pool exposing (Pool)
import Data.Remote exposing (Remote(..))
import Data.Slippage exposing (Slippage)
import Data.Spot exposing (Spot)
import Data.Token exposing (Token)
import Data.TokenParam as TokenParam exposing (TokenParam)
import Element
    exposing
        ( Element
        , alignRight
        , alignTop
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
import Page.Transaction.Button as Button
import Page.Transaction.Lend.Empty as Empty
import Page.Transaction.Lend.Lend.Disabled as Disabled
import Page.Transaction.Lend.Lend.Main as Lend
import Page.Transaction.MaturityButton as MaturityButton
import Page.Transaction.PoolInfo as PoolInfo exposing (PoolInfo)
import Page.Transaction.Query as Query
import Page.Transaction.SpotPrice exposing (SpotPrice)
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
    { http : Http.Error
    , lend : Disabled.Transaction
    }


type PoolState
    = Exist PoolInfo Lend.Transaction
    | DoesNotExist SpotPrice


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
    { model | time : Posix }
    -> Blockchain
    -> Maybe Parameter
    -> ( Transaction, Cmd Msg )
init { time } blockchain parameter =
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
                        Lend.init
                            |> Exist PoolInfo.dummy
                            |> Success
                            |> Active
                            |> Pool pool

                  -- Loading
                  --     |> Active
                  --     |> Pool pool
                  , tooltip = Nothing
                  }
                    |> Transaction
                  -- , get blockchain pool
                , Cmd.none
                )

            else
                ( { state = Matured |> Pool pool
                  , tooltip = Nothing
                  }
                    |> Transaction
                , Cmd.none
                )


initGivenPoolInfo :
    { model | time : Posix }
    -> Blockchain
    -> Pool
    -> PoolInfo
    -> ( Transaction, Cmd Msg )
initGivenPoolInfo { time } blockchain pool poolInfo =
    if pool.maturity |> Maturity.isActive time then
        ( { state =
                Lend.init
                    |> Exist poolInfo
                    |> Success
                    |> Active
                    |> Pool pool
          , tooltip = Nothing
          }
            |> Transaction
        , get blockchain pool
        )

    else
        ( { state = Matured |> Pool pool
          , tooltip = Nothing
          }
            |> Transaction
        , Cmd.none
        )


initGivenSpot :
    { model | time : Posix }
    -> Blockchain
    -> Pool
    -> SpotPrice
    -> ( Transaction, Cmd Msg )
initGivenSpot { time } blockchain pool spot =
    if pool.maturity |> Maturity.isActive time then
        ( { state =
                DoesNotExist spot
                    |> Success
                    |> Active
                    |> Pool pool
          , tooltip = Nothing
          }
            |> Transaction
        , get blockchain pool
        )

    else
        ( { state = Matured |> Pool pool
          , tooltip = Nothing
          }
            |> Transaction
        , Cmd.none
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
            , get blockchain pool
            , Nothing
            )

        ( QueryAgain, Pool pool (Active (Failure _)) ) ->
            ( transaction |> Transaction
            , get blockchain pool
            , Nothing
            )

        ( ReceiveAnswer chain pool result, Pool currentPool (Active remote) ) ->
            (if
                (chain == (blockchain |> Blockchain.toChain))
                    && (pool == currentPool)
             then
                case
                    ( result
                    , remote
                    )
                of
                    ( Ok (Right poolInfo), Success (Exist _ lend) ) ->
                        lend
                            |> Exist poolInfo
                            |> Success
                            |> Left
                            |> Just

                    ( Ok (Right poolInfo), Success (DoesNotExist _) ) ->
                        Lend.init
                            |> Exist poolInfo
                            |> Success
                            |> Left
                            |> Just

                    ( Ok (Right poolInfo), Failure error ) ->
                        error.lend
                            |> Lend.fromDisabled model blockchain pool poolInfo
                            |> (\( updated, cmd ) ->
                                    ( updated
                                        |> Exist poolInfo
                                        |> Success
                                    , cmd |> Cmd.map LendMsg
                                    )
                                        |> Right
                                        |> Just
                               )

                    ( Ok (Right poolInfo), Loading ) ->
                        Lend.init
                            |> Exist poolInfo
                            |> Success
                            |> Left
                            |> Just

                    ( Ok (Left spot), _ ) ->
                        DoesNotExist spot
                            |> Success
                            |> Left
                            |> Just

                    ( Err error, Success (Exist _ lend) ) ->
                        { http = error
                        , lend = lend |> Lend.toDisabled
                        }
                            |> Failure
                            |> Left
                            |> Just

                    ( Err error, Success (DoesNotExist _) ) ->
                        { http = error
                        , lend = Disabled.init
                        }
                            |> Failure
                            |> Left
                            |> Just

                    ( Err error, Failure failure ) ->
                        { failure | http = error }
                            |> Failure
                            |> Left
                            |> Just

                    ( Err error, Loading ) ->
                        { http = error
                        , lend = Disabled.init
                        }
                            |> Failure
                            |> Left
                            |> Just

             else
                Nothing
            )
                |> (\maybeOr ->
                        case maybeOr of
                            Just (Left state) ->
                                ( { transaction
                                    | state =
                                        state
                                            |> Active
                                            |> Pool pool
                                  }
                                , Cmd.none
                                )

                            Just (Right ( updated, cmd )) ->
                                ( { transaction
                                    | state =
                                        updated
                                            |> Active
                                            |> Pool pool
                                  }
                                , cmd
                                )

                            Nothing ->
                                ( transaction
                                , Cmd.none
                                )
                   )
                |> (\( updated, cmd ) ->
                        ( updated |> Transaction
                        , Cmd.batch
                            [ cmd
                            , Process.sleep 5000
                                |> Task.perform (\_ -> QueryAgain)
                            ]
                        , Nothing
                        )
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
    Blockchain
    -> Pool
    -> Cmd Msg
get blockchain pool =
    blockchain
        |> Blockchain.toChain
        |> (\chain ->
                Http.get
                    { url =
                        pool
                            |> Query.toUrlString chain
                    , expect =
                        Answer.decoder
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


toPoolInfo : Transaction -> Maybe (Or SpotPrice PoolInfo)
toPoolInfo (Transaction { state }) =
    case state of
        Pool _ (Active (Success (Exist poolInfo _))) ->
            poolInfo
                |> Right
                |> Just

        Pool _ (Active (Success (DoesNotExist spot))) ->
            spot
                |> Left
                |> Just

        _ ->
            Nothing


view :
    { model
        | time : Posix
        , offset : Offset
        , chosenZone : ChosenZone
        , spot : Spot
        , backdrop : Backdrop
        , images : Images
    }
    -> Blockchain
    -> Transaction
    -> Element Msg
view ({ backdrop } as model) blockchain (Transaction transaction) =
    (case transaction.state of
        None ->
            { asset = Nothing
            , collateral = Nothing
            }
                |> Empty.view model
                |> (\{ first, second } ->
                        { first = first |> map never
                        , second = second |> map never
                        , buttons = Button.selectTokens |> map never
                        }
                   )

        Asset asset ->
            { asset = Just asset
            , collateral = Nothing
            }
                |> Empty.view model
                |> (\{ first, second } ->
                        { first = first |> map never
                        , second = second |> map never
                        , buttons = Button.selectTokens |> map never
                        }
                   )

        Collateral collateral ->
            { asset = Nothing
            , collateral = Just collateral
            }
                |> Empty.view model
                |> (\{ first, second } ->
                        { first = first |> map never
                        , second = second |> map never
                        , buttons = Button.selectTokens |> map never
                        }
                   )

        Pair pair ->
            { asset =
                pair
                    |> Pair.toAsset
                    |> Just
            , collateral =
                pair
                    |> Pair.toCollateral
                    |> Just
            }
                |> Empty.view model
                |> (\{ first, second } ->
                        { first = first |> map never
                        , second = second |> map never
                        , buttons = Button.selectMaturity |> map never
                        }
                   )

        Pool pool Matured ->
            { asset =
                pool.pair
                    |> Pair.toAsset
                    |> Just
            , collateral =
                pool.pair
                    |> Pair.toCollateral
                    |> Just
            }
                |> Empty.view model
                |> (\{ first, second } ->
                        { first = first |> map never
                        , second = second |> map never
                        , buttons = Button.matured |> map never
                        }
                   )

        Pool pool (Active Loading) ->
            { asset =
                pool.pair
                    |> Pair.toAsset
                    |> Just
            , collateral =
                pool.pair
                    |> Pair.toCollateral
                    |> Just
            }
                |> Empty.view model
                |> (\{ first, second } ->
                        { first = first |> map never
                        , second = second |> map never
                        , buttons = Button.loading |> map never
                        }
                   )

        Pool pool (Active (Failure { http, lend })) ->
            lend
                |> Disabled.view model blockchain pool
                |> (\{ first, second } ->
                        { first = first |> map never
                        , second = second |> map never
                        , buttons = Button.error http |> map never
                        }
                   )

        Pool pool (Active (Success (DoesNotExist _))) ->
            { asset =
                pool.pair
                    |> Pair.toAsset
                    |> Just
            , collateral =
                pool.pair
                    |> Pair.toCollateral
                    |> Just
            }
                |> Empty.view model
                |> (\{ first, second } ->
                        { first = first |> map never
                        , second = second |> map never
                        , buttons = Button.doesNotExist |> map never
                        }
                   )

        Pool pool (Active (Success (Exist _ lend))) ->
            lend
                |> Lend.view model blockchain pool
                |> (\{ first, second, buttons } ->
                        { first = first |> map LendMsg
                        , second = second |> map LendMsg
                        , buttons = buttons |> map LendMsg
                        }
                   )
    )
        |> (\{ first, second, buttons } ->
                column
                    [ Region.description "lend transaction"
                    , width shrink
                    , height shrink
                    , padding 24
                    , spacing 16
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
                    , row
                        [ width shrink
                        , height shrink
                        , spacing 24
                        ]
                        [ column
                            [ width shrink
                            , height shrink
                            , spacing 16
                            , alignTop
                            ]
                            [ parameters model transaction
                            , first
                            ]
                        , column
                            [ width shrink
                            , height shrink
                            , spacing 16
                            , alignTop
                            ]
                            [ second
                            , buttons
                            ]
                        ]
                    ]
           )


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
