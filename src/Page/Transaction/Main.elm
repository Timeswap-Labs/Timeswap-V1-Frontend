module Page.Transaction.Main exposing
    ( Effect(..)
    , Msg
    , Section
    , init
    , initDoesNotExist
    , initGivenPool
    , notSupported
    , subscriptions
    , toParameter
    , toPoolInfo
    , update
    , view
    )

import Blockchain.Main as Blockchain exposing (Blockchain)
import Data.Backdrop exposing (Backdrop)
import Data.Chains exposing (Chains)
import Data.ChosenZone exposing (ChosenZone)
import Data.Deadline exposing (Deadline)
import Data.Images exposing (Images)
import Data.Maturity as Maturity
import Data.Pair as Pair exposing (Pair)
import Data.Parameter as Parameter exposing (Parameter)
import Data.Pool exposing (Pool)
import Data.Remote exposing (Remote(..))
import Data.ShowCreate as ShowCreate exposing (ShowCreate)
import Data.Slippage exposing (Slippage)
import Data.Token exposing (Token)
import Data.TokenParam as TokenParam exposing (TokenParam)
import Element
    exposing
        ( Element
        , alignLeft
        , alignRight
        , alignTop
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
import Http
import Page.Transaction.Answer as Answer exposing (Answer)
import Page.Transaction.Button as Button
import Page.Transaction.Error as Error exposing (Error)
import Page.Transaction.PoolInfo exposing (PoolInfo)
import Page.Transaction.PoolState as PoolState exposing (PoolState)
import Page.Transaction.Query as Query
import Page.Transaction.Tooltip as Tooltip exposing (Tooltip)
import Process
import Task
import Time exposing (Posix, Zone)
import Utility.Color as Color
import Utility.Duration as Duration
import Utility.Glass as Glass
import Utility.Image as Image
import Utility.Truncate as Truncate


type Section transaction create
    = Section
        { state : State transaction create
        , tooltip : Maybe Tooltip
        }


type State transaction create
    = None
    | Asset Token
    | Collateral Token
    | Pair Pair
    | Pool
        { pool : Pool
        , state :
            Remote
                (Error transaction create)
                (PoolState transaction create)
        }


type Msg transactionMsg createMsg
    = SelectToken TokenParam
    | SelectMaturity
    | ClickCreatePool
    | ClickSettings
    | TransactionMsg transactionMsg
    | CreateMsg createMsg
    | QueryAgain
    | ReceiveAnswer (Result Http.Error Answer)
    | CheckMaturity Posix
    | OnMouseEnter Tooltip
    | OnMouseLeave


type Effect transactionEffect createEffect
    = OpenTokenList TokenParam
    | OpenMaturityList Pair
    | OpenChooseMaturity Pair
    | OpenSettings
    | OpenConnect
    | TransactionEffect transactionEffect
    | CreateEffect createEffect


init :
    { model | time : Posix, chains : Chains }
    -> Blockchain
    -> Maybe Parameter
    ->
        ( Section transaction create
        , Cmd (Msg transactionMsg createMsg)
        )
init ({ time } as model) blockchain parameter =
    case parameter of
        Nothing ->
            ( { state = None
              , tooltip = Nothing
              }
                |> Section
            , Cmd.none
            )

        Just (Parameter.Asset asset) ->
            ( { state = Asset asset
              , tooltip = Nothing
              }
                |> Section
            , Cmd.none
            )

        Just (Parameter.Collateral collateral) ->
            ( { state = Collateral collateral
              , tooltip = Nothing
              }
                |> Section
            , Cmd.none
            )

        Just (Parameter.Pair pair) ->
            ( { state = Pair pair
              , tooltip = Nothing
              }
                |> Section
            , Cmd.none
            )

        Just (Parameter.Pool pool) ->
            if pool.maturity |> Maturity.isActive time then
                ( { state =
                        { pool = pool
                        , state = Loading
                        }
                            |> Pool
                  , tooltip = Nothing
                  }
                    |> Section
                , get model blockchain pool
                )

            else
                ( { state =
                        { pool = pool
                        , state = PoolState.Matured |> Success
                        }
                            |> Pool
                  , tooltip = Nothing
                  }
                    |> Section
                , Cmd.none
                )


initGivenPool :
    transaction
    -> { model | time : Posix, chains : Chains }
    -> Blockchain
    -> Pool
    -> PoolInfo
    ->
        ( Section transaction create
        , Cmd (Msg transactionMsg createMsg)
        )
initGivenPool initTransaction ({ time } as model) blockchain pool poolInfo =
    ( { state =
            { pool = pool
            , state =
                (if pool.maturity |> Maturity.isActive time then
                    { poolInfo = poolInfo
                    , transaction = initTransaction
                    }
                        |> PoolState.Active

                 else
                    PoolState.Matured
                )
                    |> Success
            }
                |> Pool
      , tooltip = Nothing
      }
        |> Section
    , get model blockchain pool
    )


initDoesNotExist :
    create
    -> { model | time : Posix }
    -> Pool
    ->
        ( Section transaction create
        , Cmd (Msg transactionMsg createMsg)
        )
initDoesNotExist initCreate { time } pool =
    ( { state =
            { pool = pool
            , state =
                (if pool.maturity |> Maturity.isActive time then
                    initCreate
                        |> PoolState.DoesNotExist

                 else
                    PoolState.Matured
                )
                    |> Success
            }
                |> Pool
      , tooltip = Nothing
      }
        |> Section
    , Process.sleep 5000
        |> Task.perform (\_ -> QueryAgain)
    )


notSupported : Section transaction create
notSupported =
    { state = None
    , tooltip = Nothing
    }
        |> Section


update :
    { initTransaction : transaction
    , refreshTransaction : transaction -> transaction
    , transaction :
        Pool
        -> PoolInfo
        -> transactionMsg
        -> transaction
        -> ( transaction, Cmd transactionMsg, Maybe transactionEffect )
    , initCreate : create
    , refreshCreate : create -> create
    , create :
        Pool
        -> createMsg
        -> create
        -> ( create, Cmd createMsg, Maybe createEffect )
    }
    ->
        { model
            | time : Posix
            , chains : Chains
            , slippage : Slippage
            , deadline : Deadline
        }
    -> Blockchain
    -> Msg transactionMsg createMsg
    -> Section transaction create
    ->
        ( Section transaction create
        , Cmd (Msg transactionMsg createMsg)
        , Maybe (Effect transactionEffect createEffect)
        )
update updates model blockchain msg (Section section) =
    case ( msg, section.state ) of
        ( SelectToken tokenParam, _ ) ->
            ( section |> Section
            , Cmd.none
            , tokenParam
                |> OpenTokenList
                |> Just
            )

        ( SelectMaturity, Pair pair ) ->
            ( section |> Section
            , Cmd.none
            , pair
                |> OpenMaturityList
                |> Just
            )

        ( SelectMaturity, Pool { pool } ) ->
            ( section |> Section
            , Cmd.none
            , pool.pair
                |> OpenMaturityList
                |> Just
            )

        ( ClickCreatePool, Pair pair ) ->
            ( section |> Section
            , Cmd.none
            , pair
                |> OpenChooseMaturity
                |> Just
            )

        ( ClickCreatePool, Pool { pool } ) ->
            ( section |> Section
            , Cmd.none
            , pool.pair
                |> OpenChooseMaturity
                |> Just
            )

        ( ClickSettings, _ ) ->
            ( section |> Section
            , Cmd.none
            , Just OpenSettings
            )

        ( TransactionMsg transactionMsg, Pool { pool, state } ) ->
            case state of
                Success (PoolState.Active { poolInfo, transaction }) ->
                    transaction
                        |> updates.transaction
                            pool
                            poolInfo
                            transactionMsg
                        |> (\( updated, cmd, maybeEffect ) ->
                                ( { section
                                    | state =
                                        { pool = pool
                                        , state =
                                            { poolInfo = poolInfo
                                            , transaction = updated
                                            }
                                                |> PoolState.Active
                                                |> Success
                                        }
                                            |> Pool
                                  }
                                    |> Section
                                , cmd |> Cmd.map TransactionMsg
                                , maybeEffect |> Maybe.map TransactionEffect
                                )
                           )

                _ ->
                    ( section |> Section
                    , Cmd.none
                    , Nothing
                    )

        ( CreateMsg createMsg, Pool { pool, state } ) ->
            case state of
                Success (PoolState.DoesNotExist create) ->
                    create
                        |> updates.create
                            pool
                            createMsg
                        |> (\( updated, cmd, maybeEffect ) ->
                                ( { section
                                    | state =
                                        { pool = pool
                                        , state =
                                            updated
                                                |> PoolState.DoesNotExist
                                                |> Success
                                        }
                                            |> Pool
                                  }
                                    |> Section
                                , cmd |> Cmd.map CreateMsg
                                , maybeEffect |> Maybe.map CreateEffect
                                )
                           )

                _ ->
                    ( section |> Section
                    , Cmd.none
                    , Nothing
                    )

        ( QueryAgain, Pool { pool, state } ) ->
            ( section |> Section
            , case state of
                Success PoolState.Matured ->
                    Cmd.none

                _ ->
                    get model blockchain pool
            , Nothing
            )

        ( ReceiveAnswer (Ok answer), Pool { pool, state } ) ->
            ( (if
                (answer.chainId == (blockchain |> Blockchain.toChain))
                    && (answer.pool == pool)
               then
                case ( answer.result, state ) of
                    ( Just poolInfo, Success (PoolState.Active active) ) ->
                        { active | poolInfo = poolInfo }
                            |> PoolState.Active
                            |> Success

                    ( Just poolInfo, Success (PoolState.DoesNotExist _) ) ->
                        { poolInfo = poolInfo
                        , transaction = updates.initTransaction
                        }
                            |> PoolState.Active
                            |> Success

                    ( Just poolInfo, Failure failure ) ->
                        { poolInfo = poolInfo
                        , transaction =
                            case failure.state of
                                Error.Active transaction ->
                                    transaction

                                Error.DoesNotExist _ ->
                                    updates.initTransaction
                        }
                            |> PoolState.Active
                            |> Success

                    ( Just poolInfo, Loading ) ->
                        { poolInfo = poolInfo
                        , transaction = updates.initTransaction
                        }
                            |> PoolState.Active
                            |> Success

                    ( Nothing, Success (PoolState.Active _) ) ->
                        updates.initCreate
                            |> PoolState.DoesNotExist
                            |> Success

                    ( Nothing, Failure failure ) ->
                        (case failure.state of
                            Error.Active _ ->
                                updates.initCreate

                            Error.DoesNotExist create ->
                                create
                        )
                            |> PoolState.DoesNotExist
                            |> Success

                    ( Nothing, Loading ) ->
                        updates.initCreate
                            |> PoolState.DoesNotExist
                            |> Success

                    _ ->
                        state

               else
                state
              )
                |> (\updated ->
                        { section
                            | state =
                                { pool = pool
                                , state = updated
                                }
                                    |> Pool
                        }
                            |> Section
                   )
            , case state of
                Success PoolState.Matured ->
                    Cmd.none

                _ ->
                    Process.sleep 5000
                        |> Task.perform (\_ -> QueryAgain)
            , Nothing
            )

        ( ReceiveAnswer (Err error), Pool { pool, state } ) ->
            ( (case state of
                Success (PoolState.Active { transaction }) ->
                    { error = error
                    , state =
                        transaction
                            |> updates.refreshTransaction
                            |> Error.Active
                    }
                        |> Failure

                Success (PoolState.DoesNotExist create) ->
                    { error = error
                    , state =
                        create
                            |> updates.refreshCreate
                            |> Error.DoesNotExist
                    }
                        |> Failure

                Failure failure ->
                    { failure | error = error }
                        |> Failure

                Loading ->
                    { error = error
                    , state =
                        updates.initTransaction
                            |> Error.Active
                    }
                        |> Failure

                _ ->
                    state
              )
                |> (\updated ->
                        { section
                            | state =
                                { pool = pool
                                , state = updated
                                }
                                    |> Pool
                        }
                            |> Section
                   )
            , case state of
                Success PoolState.Matured ->
                    Cmd.none

                _ ->
                    Process.sleep 5000
                        |> Task.perform (\_ -> QueryAgain)
            , Nothing
            )

        ( CheckMaturity posix, Pool ({ pool } as state) ) ->
            ( { section
                | state =
                    if pool.maturity |> Maturity.isActive posix then
                        state |> Pool

                    else
                        { state
                            | state =
                                PoolState.Matured
                                    |> Success
                        }
                            |> Pool
              }
                |> Section
            , Cmd.none
            , Nothing
            )

        ( OnMouseEnter tooltip, _ ) ->
            ( { section | tooltip = Just tooltip }
                |> Section
            , Cmd.none
            , Nothing
            )

        ( OnMouseLeave, _ ) ->
            ( { section | tooltip = Nothing }
                |> Section
            , Cmd.none
            , Nothing
            )

        _ ->
            ( section |> Section
            , Cmd.none
            , Nothing
            )


get :
    { model | chains : Chains }
    -> Blockchain
    -> Pool
    -> Cmd (Msg transactionMsg createMsg)
get model blockchain pool =
    Http.get
        { url = pool |> Query.toUrlString blockchain
        , expect =
            Answer.decoder model
                |> Http.expectJson ReceiveAnswer
        }


subscriptions :
    (transaction -> Sub transactionMsg)
    -> Section transaction create
    -> Sub (Msg transactionMsg createMsg)
subscriptions sub (Section section) =
    [ case section.state of
        Pool { state } ->
            case state of
                Success (PoolState.Active _) ->
                    Time.every 1000 CheckMaturity

                Success (PoolState.DoesNotExist _) ->
                    Time.every 1000 CheckMaturity

                _ ->
                    Sub.none

        _ ->
            Sub.none
    , case section.state of
        Pool { state } ->
            case state of
                Success (PoolState.Active { transaction }) ->
                    transaction
                        |> sub
                        |> Sub.map TransactionMsg

                _ ->
                    Sub.none

        _ ->
            Sub.none
    ]
        |> Sub.batch


toParameter : Section transaction create -> Maybe Parameter
toParameter (Section section) =
    case section.state of
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

        Pool { pool } ->
            Parameter.Pool pool
                |> Just


toPoolInfo : Section transaction create -> Maybe PoolInfo
toPoolInfo (Section section) =
    case section.state of
        Pool { state } ->
            case state of
                Success (PoolState.Active { poolInfo }) ->
                    poolInfo |> Just

                _ ->
                    Nothing

        _ ->
            Nothing


view :
    { title : String
    , createTitle : String
    , showCreate : ShowCreate
    , transaction :
        Pool
        -> transaction
        ->
            { first : Element transactionMsg
            , second : Element transactionMsg
            , buttons : Element transactionMsg
            }
    , disabledTransaction :
        Pool
        -> transaction
        ->
            { first : Element Never
            , second : Element Never
            }
    , create :
        Pool
        -> create
        ->
            { first : Element createMsg
            , second : Element createMsg
            , buttons : Element createMsg
            }
    , disabledCreate :
        Pool
        -> create
        ->
            { first : Element Never
            , second : Element Never
            }
    , empty :
        { asset : Maybe Token
        , collateral : Maybe Token
        }
        ->
            { first : Element Never
            , second : Element Never
            }
    }
    ->
        { model
            | time : Posix
            , zone : Zone
            , chosenZone : ChosenZone
            , backdrop : Backdrop
            , images : Images
        }
    -> Section transaction create
    -> Element (Msg transactionMsg createMsg)
view views model (Section section) =
    (case section.state of
        None ->
            views.empty
                { asset = Nothing
                , collateral = Nothing
                }
                |> (\{ first, second } ->
                        { title = views.title
                        , showCreate = ShowCreate.DoNot
                        , first = first |> map never
                        , second = second |> map never
                        , buttons =
                            Button.disabled "Select Pair First"
                                |> map never
                        }
                   )

        Asset asset ->
            views.empty
                { asset = Just asset
                , collateral = Nothing
                }
                |> (\{ first, second } ->
                        { title = views.title
                        , showCreate = ShowCreate.DoNot
                        , first = first |> map never
                        , second = second |> map never
                        , buttons =
                            Button.disabled "Select Pair First"
                                |> map never
                        }
                   )

        Collateral collateral ->
            views.empty
                { asset = Nothing
                , collateral = Just collateral
                }
                |> (\{ first, second } ->
                        { title = views.title
                        , showCreate = ShowCreate.DoNot
                        , first = first |> map never
                        , second = second |> map never
                        , buttons =
                            Button.disabled "Select Pair First"
                                |> map never
                        }
                   )

        Pair pair ->
            views.empty
                { asset = pair |> Pair.toAsset |> Just
                , collateral = pair |> Pair.toCollateral |> Just
                }
                |> (\{ first, second } ->
                        { title = views.title
                        , showCreate = views.showCreate
                        , first = first |> map never
                        , second = second |> map never
                        , buttons =
                            Button.disabled "Select Maturity First"
                                |> map never
                        }
                   )

        Pool { pool, state } ->
            case state of
                Loading ->
                    views.empty
                        { asset =
                            pool.pair
                                |> Pair.toAsset
                                |> Just
                        , collateral =
                            pool.pair
                                |> Pair.toCollateral
                                |> Just
                        }
                        |> (\{ first, second } ->
                                { title = views.title
                                , showCreate = views.showCreate
                                , first = first |> map never
                                , second = second |> map never
                                , buttons =
                                    Button.disabled "Loading"
                                        |> map never
                                }
                           )

                Failure failure ->
                    (case failure.state of
                        Error.Active transaction ->
                            ( transaction |> views.disabledTransaction pool
                            , views.title
                            )

                        Error.DoesNotExist create ->
                            ( create |> views.disabledCreate pool
                            , views.createTitle
                            )
                    )
                        |> (\( { first, second }, title ) ->
                                { title = title
                                , showCreate = views.showCreate
                                , first = first |> map never
                                , second = second |> map never
                                , buttons =
                                    (case failure.error of
                                        Http.Timeout ->
                                            "Network Too Slow"

                                        Http.NetworkError ->
                                            "No Network Connection"

                                        Http.BadStatus status ->
                                            "Error Code: " ++ (status |> String.fromInt)

                                        _ ->
                                            "Error"
                                    )
                                        |> Button.error
                                        |> map never
                                }
                           )

                Success (PoolState.Active { transaction }) ->
                    transaction
                        |> views.transaction pool
                        |> (\{ first, second, buttons } ->
                                { title = views.title
                                , showCreate = views.showCreate
                                , first = first |> map TransactionMsg
                                , second = second |> map TransactionMsg
                                , buttons = buttons |> map TransactionMsg
                                }
                           )

                Success (PoolState.DoesNotExist create) ->
                    create
                        |> views.create pool
                        |> (\{ first, second, buttons } ->
                                { title = views.createTitle
                                , showCreate = views.showCreate
                                , first = first |> map CreateMsg
                                , second = second |> map CreateMsg
                                , buttons = buttons |> map CreateMsg
                                }
                           )

                Success PoolState.Matured ->
                    views.empty
                        { asset =
                            pool.pair
                                |> Pair.toAsset
                                |> Just
                        , collateral =
                            pool.pair
                                |> Pair.toCollateral
                                |> Just
                        }
                        |> (\{ first, second } ->
                                { title = views.title
                                , showCreate = views.showCreate
                                , first = first |> map never
                                , second = second |> map never
                                , buttons = Button.error "Already Matured" |> map never
                                }
                           )
    )
        |> (\{ title, showCreate, first, second, buttons } ->
                column
                    [ Region.description
                        ([ title |> String.toLower
                         , "transaction"
                         ]
                            |> String.join " "
                        )
                    , width shrink
                    , height shrink
                    , padding 16
                    , spacing 16
                    , Border.rounded 8
                    , Glass.background model.backdrop
                    , Border.width 1
                    , Border.color Color.transparent100
                    ]
                    [ row
                        [ width fill
                        , height shrink
                        , spacing 24
                        ]
                        [ el
                            [ width shrink
                            , height shrink
                            , paddingXY 0 4
                            , Font.size 24
                            , Font.color Color.light100
                            , Font.bold
                            ]
                            (text title)
                        , case showCreate of
                            ShowCreate.Do ->
                                createButton model

                            ShowCreate.DoNot ->
                                none
                        , settingsButton model
                        ]
                    , row
                        [ width shrink
                        , height shrink
                        , spacing 20
                        ]
                        [ column
                            [ width shrink
                            , height shrink
                            , spacing 14
                            , alignTop
                            ]
                            [ parameters model section
                            , first
                            ]
                        , column
                            [ width shrink
                            , height shrink
                            , alignTop
                            , spacing 14
                            ]
                            [ second
                            , buttons
                            ]
                        ]
                    ]
           )


createButton :
    { model | images : Images }
    -> Element (Msg transactionMsg createMsg)
createButton { images } =
    Input.button
        [ width shrink
        , height shrink
        , alignRight
        , centerY
        ]
        { onPress = ClickCreatePool |> Just
        , label =
            row
                [ width shrink
                , height shrink
                , spacing 4
                ]
                [ images
                    |> Image.plusPositive
                        [ width <| px 14
                        , height <| px 14
                        , centerY
                        ]
                , el
                    [ width shrink
                    , height shrink
                    , centerY
                    , Font.bold
                    , Font.size 16
                    , paddingXY 0 4
                    , Font.color Color.positive400
                    ]
                    (text "Create Pool")
                ]
        }


settingsButton :
    { model | images : Images }
    -> Element (Msg transactionMsg createMsg)
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
        , zone : Zone
        , chosenZone : ChosenZone
        , backdrop : Backdrop
        , images : Images
    }
    ->
        { section
            | state : State transaction create
            , tooltip : Maybe Tooltip
        }
    -> Element (Msg transactionMsg createMsg)
parameters ({ backdrop } as model) section =
    column
        [ Region.description "pool parameters"
        , width <| px 343
        , height shrink
        , padding 16
        , spacing 12
        , Background.color Color.primary100
        , Border.rounded 8
        ]
        [ pairParameters model section
        , maturityParameter model section
        ]


pairParameters :
    { model | images : Images }
    ->
        { section
            | state : State transaction create
            , tooltip : Maybe Tooltip
        }
    -> Element (Msg transactionMsg createMsg)
pairParameters model section =
    row
        [ width fill
        , height shrink
        , spacing 16
        ]
        [ tokenParameter model section TokenParam.Asset
        , tokenParameter model section TokenParam.Collateral
        ]


tokenParameter :
    { model | images : Images }
    ->
        { section
            | state : State transaction create
            , tooltip : Maybe Tooltip
        }
    -> TokenParam
    -> Element (Msg transactionMsg createMsg)
tokenParameter model section tokenParam =
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
        , tokenButton model section tokenParam
        ]


tokenButton :
    { model | images : Images }
    ->
        { section
            | state : State transaction create
            , tooltip : Maybe Tooltip
        }
    -> TokenParam
    -> Element (Msg transactionMsg createMsg)
tokenButton { images } { state, tooltip } tokenParam =
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

        ( Pool { pool }, TokenParam.Asset ) ->
            pool.pair
                |> Pair.toAsset
                |> Just

        ( Pool { pool }, TokenParam.Collateral ) ->
            pool.pair
                |> Pair.toCollateral
                |> Just

        _ ->
            Nothing
    )
        |> Maybe.map
            (\token ->
                Input.button
                    [ Region.description
                        (case tokenParam of
                            TokenParam.Asset ->
                                "asset button"

                            TokenParam.Collateral ->
                                "collateral button"
                        )
                    , width fill
                    , height <| px 44
                    , Background.color Color.primary100
                    , Border.width 1
                    , Border.color Color.transparent100
                    , Border.rounded 8
                    ]
                    { onPress = SelectToken tokenParam |> Just
                    , label =
                        row
                            [ width fill
                            , height fill
                            , paddingXY 12 0
                            , spacing 6
                            ]
                            [ images
                                |> Image.viewToken
                                    [ width <| px 24
                                    , alignLeft
                                    , centerY
                                    ]
                                    token
                            , Truncate.viewSymbol
                                { onMouseEnter = OnMouseEnter
                                , onMouseLeave = OnMouseLeave
                                , tooltip = Tooltip.Token tokenParam
                                , opened = tooltip
                                , token = token
                                }
                            , images
                                |> Image.discloser
                                    [ width <| px 9
                                    , alignRight
                                    , centerY
                                    ]
                            ]
                    }
            )
        |> Maybe.withDefault
            (Input.button
                [ Region.description
                    (case tokenParam of
                        TokenParam.Asset ->
                            "asset button"

                        TokenParam.Collateral ->
                            "collateral button"
                    )
                , width fill
                , height <| px 44
                , Background.color Color.primary500
                , Border.rounded 8
                ]
                { onPress = SelectToken tokenParam |> Just
                , label =
                    row
                        [ width fill
                        , height fill
                        , paddingXY 12 0
                        ]
                        [ el
                            [ width shrink
                            , height shrink
                            , alignLeft
                            , centerY
                            , Font.size 14
                            , Font.color Color.light100
                            ]
                            (text "Select Token")
                        , images
                            |> Image.discloser
                                [ width <| px 9
                                , alignRight
                                , centerY
                                ]
                        ]
                }
            )


maturityParameter :
    { model
        | time : Posix
        , zone : Zone
        , chosenZone : ChosenZone
        , images : Images
    }
    ->
        { section
            | state : State transaction create
            , tooltip : Maybe Tooltip
        }
    -> Element (Msg transactionMsg createMsg)
maturityParameter { time, zone, chosenZone, images } { state, tooltip } =
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
            Pool { pool } ->
                Input.button
                    [ Region.description "maturity button"
                    , width fill
                    , height <| px 44
                    , Background.color Color.primary100
                    , Border.width 1
                    , Border.color Color.transparent100
                    , Border.rounded 8
                    ]
                    { onPress = SelectMaturity |> Just
                    , label =
                        row
                            [ width fill
                            , height fill
                            , paddingXY 12 0
                            , spacing 6
                            ]
                            [ Duration.viewMaturity
                                { onMouseEnter = OnMouseEnter
                                , onMouseLeave = OnMouseLeave
                                , tooltip = Tooltip.Maturity
                                , opened = tooltip
                                , time = time
                                , zone = zone
                                , chosenZone = chosenZone
                                , maturity = pool.maturity
                                }
                            , images
                                |> Image.discloser
                                    [ width <| px 9
                                    , alignRight
                                    , centerY
                                    ]
                            ]
                    }

            Pair _ ->
                Input.button
                    [ Region.description "maturity button"
                    , width fill
                    , height <| px 44
                    , Background.color Color.primary500
                    , Border.rounded 8
                    ]
                    { onPress = SelectMaturity |> Just
                    , label =
                        row
                            [ width fill
                            , height fill
                            , paddingXY 12 0
                            , spacing 6
                            ]
                            [ el
                                [ width <| px 24
                                , height <| px 24
                                ]
                                (images
                                    |> Image.hourglass
                                        [ height <| px 24
                                        , centerX
                                        ]
                                )
                            , el
                                [ width shrink
                                , height shrink
                                , alignLeft
                                , centerY
                                , Font.size 14
                                , Font.color Color.transparent400
                                ]
                                (text "Select Maturity")
                            , images
                                |> Image.discloser
                                    [ width <| px 9
                                    , alignRight
                                    , centerY
                                    ]
                            ]
                    }

            _ ->
                el
                    [ width fill
                    , height <| px 44
                    , paddingXY 12 0
                    , spacing 6
                    , Background.color Color.primary100
                    , Border.rounded 8
                    ]
                    (el
                        [ width shrink
                        , height shrink
                        , alignLeft
                        , centerY
                        , Font.size 14
                        , Font.color Color.transparent100
                        ]
                        (text "Select Pair First")
                    )
        ]
