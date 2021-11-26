module Page.Transaction.Main exposing (..)

import Blockchain.Main as Blockchain exposing (Blockchain)
import Data.Chains exposing (Chains)
import Data.ChosenZone as ChosenZone exposing (ChosenZone)
import Data.Deadline exposing (Deadline)
import Data.Images exposing (Images)
import Data.Maturity as Maturity
import Data.Pair as Pair exposing (Pair)
import Data.Parameter as Parameter exposing (Parameter)
import Data.Pool exposing (Pool)
import Data.Remote exposing (Remote(..))
import Data.Slippage exposing (Slippage)
import Data.Token as Token exposing (Token)
import Data.TokenParam as TokenParam exposing (TokenParam)
import Element
    exposing
        ( Element
        , alignLeft
        , alignRight
        , alignTop
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
import Page.Transaction.Button as Button
import Page.Transaction.Error exposing (Error)
import Page.Transaction.PoolInfo exposing (PoolInfo)
import Page.Transaction.State as State exposing (State)
import Time exposing (Posix, Zone)
import Utility.Color as Color
import Utility.Image as Image


type Section transaction create
    = None
    | Asset Token
    | Collateral Token
    | Pair Pair
    | Pool
        { pool : Pool
        , state : Remote (Error transaction) (State transaction create)
        }


type Msg transactionMsg createMsg
    = SelectToken TokenParam
    | SelectMaturity
    | ClickConnect
    | TransactionMsg transactionMsg
    | CreateMsg createMsg
    | CheckMaturity Posix


type Effect transactionEffect createEffect
    = OpenTokenList TokenParam
    | OpenMaturityList Pair
    | OpenConnect
    | TransactionEffect transactionEffect
    | CreateEffect createEffect


init :
    { model | time : Posix }
    -> Maybe Parameter
    ->
        ( Section transaction create
        , Cmd (Msg transactionMsg createMsg)
        )
init { time } parameter =
    case parameter of
        Nothing ->
            ( None
            , Cmd.none
            )

        Just (Parameter.Asset asset) ->
            ( Asset asset
            , Cmd.none
            )

        Just (Parameter.Collateral collateral) ->
            ( Collateral collateral
            , Cmd.none
            )

        Just (Parameter.Pair pair) ->
            ( Pair pair
            , Cmd.none
            )

        Just (Parameter.Pool pool) ->
            if pool.maturity |> Maturity.isActive time then
                ( { pool = pool
                  , state = Loading
                  }
                    |> Pool
                , Debug.todo "http call"
                )

            else
                ( { pool = pool
                  , state = State.Matured |> Success
                  }
                    |> Pool
                , Cmd.none
                )


initGivenPool :
    transaction
    -> { model | time : Posix }
    -> Pool
    -> PoolInfo
    -> Section transaction create
initGivenPool initTransaction { time } pool poolInfo =
    { pool = pool
    , state =
        (if pool.maturity |> Maturity.isActive time then
            { poolInfo = poolInfo
            , transaction = initTransaction
            }
                |> State.Active

         else
            State.Matured
        )
            |> Success
    }
        |> Pool


update :
    { transaction :
        { model
            | time : Posix
            , chains : Chains
            , slippage : Slippage
            , deadline : Deadline
        }
        -> Blockchain
        -> Pool
        -> PoolInfo
        -> transactionMsg
        -> transaction
        -> ( transaction, Cmd transactionMsg, Maybe transactionEffect )
    , create :
        { model
            | time : Posix
            , chains : Chains
            , slippage : Slippage
            , deadline : Deadline
        }
        -> Blockchain
        -> Pool
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
update updates model blockchain msg section =
    case ( msg, section ) of
        ( SelectToken tokenParam, _ ) ->
            ( section
            , Cmd.none
            , tokenParam
                |> OpenTokenList
                |> Just
            )

        ( SelectMaturity, Pair pair ) ->
            ( section
            , Cmd.none
            , pair
                |> OpenMaturityList
                |> Just
            )

        ( SelectMaturity, Pool { pool } ) ->
            ( section
            , Cmd.none
            , pool.pair
                |> OpenMaturityList
                |> Just
            )

        ( ClickConnect, _ ) ->
            ( section
            , Cmd.none
            , blockchain
                |> Blockchain.toUser
                |> Maybe.map (\_ -> Nothing)
                |> Maybe.withDefault (Just OpenConnect)
            )

        ( TransactionMsg transactionMsg, Pool { pool, state } ) ->
            case state of
                Success (State.Active { poolInfo, transaction }) ->
                    transaction
                        |> updates.transaction model
                            blockchain
                            pool
                            poolInfo
                            transactionMsg
                        |> (\( updated, cmd, maybeEffect ) ->
                                ( { pool = pool
                                  , state =
                                        { poolInfo = poolInfo
                                        , transaction = updated
                                        }
                                            |> State.Active
                                            |> Success
                                  }
                                    |> Pool
                                , cmd |> Cmd.map TransactionMsg
                                , maybeEffect |> Maybe.map TransactionEffect
                                )
                           )

                _ ->
                    ( section
                    , Cmd.none
                    , Nothing
                    )

        ( CreateMsg createMsg, Pool { pool, state } ) ->
            case state of
                Success (State.DoesNotExist create) ->
                    create
                        |> updates.create model
                            blockchain
                            pool
                            createMsg
                        |> (\( updated, cmd, maybeEffect ) ->
                                ( { pool = pool
                                  , state =
                                        updated
                                            |> State.DoesNotExist
                                            |> Success
                                  }
                                    |> Pool
                                , cmd |> Cmd.map CreateMsg
                                , maybeEffect |> Maybe.map CreateEffect
                                )
                           )

                _ ->
                    ( section
                    , Cmd.none
                    , Nothing
                    )

        ( CheckMaturity posix, Pool ({ pool } as state) ) ->
            ( if pool.maturity |> Maturity.isActive posix then
                state |> Pool

              else
                { state
                    | state =
                        State.Matured
                            |> Success
                }
                    |> Pool
            , Cmd.none
            , Nothing
            )

        _ ->
            ( section
            , Cmd.none
            , Nothing
            )


subscriptions :
    Sub transactionMsg
    -> Section transaction create
    -> Sub (Msg transactionMsg createMsg)
subscriptions sub section =
    [ case section of
        Pool { state } ->
            case state of
                Success (State.Active _) ->
                    Time.every 1000 CheckMaturity

                _ ->
                    Sub.none

        _ ->
            Sub.none
    , case section of
        Pool { state } ->
            case state of
                Success (State.Active _) ->
                    sub |> Sub.map TransactionMsg

                _ ->
                    Sub.none

        _ ->
            Sub.none
    ]
        |> Sub.batch


toParameter : Section transaction create -> Maybe Parameter
toParameter section =
    case section of
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
toPoolInfo section =
    case section of
        Pool { state } ->
            case state of
                Success (State.Active { poolInfo }) ->
                    poolInfo |> Just

                _ ->
                    Nothing

        _ ->
            Nothing


view :
    { title : String
    , transaction :
        Pool
        -> transaction
        ->
            { first : Element transactionMsg
            , second : Element transactionMsg
            , buttons : Element transactionMsg
            }
    , create :
        Pool
        -> create
        ->
            { first : Element createMsg
            , second : Element createMsg
            , buttons : Element createMsg
            }
    , disabled :
        Pool
        -> transaction
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
    -> { model | zone : Zone, chosenZone : ChosenZone, images : Images }
    -> Blockchain
    -> Section transaction create
    -> Element (Msg transactionMsg createMsg)
view views model blockchain section =
    (case section of
        None ->
            views.empty
                { asset = Nothing
                , collateral = Nothing
                }
                |> (\{ first, second } ->
                        { first = first |> map never
                        , second = second |> map never
                        , buttons =
                            blockchain
                                |> Blockchain.toUser
                                |> Maybe.map
                                    (\_ ->
                                        Button.disabled "Select Pair First"
                                            |> map never
                                    )
                                |> Maybe.withDefault connectButton
                        }
                   )

        Asset asset ->
            views.empty
                { asset = Just asset
                , collateral = Nothing
                }
                |> (\{ first, second } ->
                        { first = first |> map never
                        , second = second |> map never
                        , buttons =
                            blockchain
                                |> Blockchain.toUser
                                |> Maybe.map
                                    (\_ ->
                                        Button.disabled "Select Pair First"
                                            |> map never
                                    )
                                |> Maybe.withDefault connectButton
                        }
                   )

        Collateral collateral ->
            views.empty
                { asset = Nothing
                , collateral = Just collateral
                }
                |> (\{ first, second } ->
                        { first = first |> map never
                        , second = second |> map never
                        , buttons =
                            blockchain
                                |> Blockchain.toUser
                                |> Maybe.map
                                    (\_ ->
                                        Button.disabled "Select Pair First"
                                            |> map never
                                    )
                                |> Maybe.withDefault connectButton
                        }
                   )

        Pair pair ->
            views.empty
                { asset = pair |> Pair.toAsset |> Just
                , collateral = pair |> Pair.toCollateral |> Just
                }
                |> (\{ first, second } ->
                        { first = first |> map never
                        , second = second |> map never
                        , buttons =
                            blockchain
                                |> Blockchain.toUser
                                |> Maybe.map
                                    (\_ ->
                                        Button.disabled "Select Maturity First"
                                            |> map never
                                    )
                                |> Maybe.withDefault connectButton
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
                                { first = first |> map never
                                , second = second |> map never
                                , buttons =
                                    blockchain
                                        |> Blockchain.toUser
                                        |> Maybe.map
                                            (\_ ->
                                                Button.disabled "Loading"
                                                    |> map never
                                            )
                                        |> Maybe.withDefault connectButton
                                }
                           )

                Failure { error, transaction } ->
                    transaction
                        |> views.disabled pool
                        |> (\{ first, second } ->
                                { first = first |> map never
                                , second = second |> map never
                                , buttons =
                                    (case error of
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

                Success (State.Active { transaction }) ->
                    transaction
                        |> views.transaction pool
                        |> (\{ first, second, buttons } ->
                                { first = first |> map TransactionMsg
                                , second = second |> map TransactionMsg
                                , buttons =
                                    blockchain
                                        |> Blockchain.toUser
                                        |> Maybe.map
                                            (\_ -> buttons |> map TransactionMsg)
                                        |> Maybe.withDefault connectButton
                                }
                           )

                Success (State.DoesNotExist create) ->
                    create
                        |> views.create pool
                        |> (\{ first, second, buttons } ->
                                { first = first |> map CreateMsg
                                , second = second |> map CreateMsg
                                , buttons = buttons |> map CreateMsg
                                }
                           )

                Success State.Matured ->
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
                                { first = first |> map never
                                , second = second |> map never
                                , buttons =
                                    Button.error "Already Matured"
                                        |> map never
                                }
                           )
    )
        |> (\{ first, second, buttons } ->
                column
                    [ Region.description
                        ((views.title |> String.toLower)
                            ++ " transaction"
                        )
                    , width shrink
                    , height shrink
                    , padding 20
                    , spacing 20
                    , Background.color Color.light100
                    , Border.rounded 8
                    ]
                    [ el [] (text views.title)
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


connectButton : Element (Msg transactionMsg createMsg)
connectButton =
    Button.view
        { msg = ClickConnect
        , label = "Connect Wallet"
        , description = "connect button"
        }


parameters :
    { model | zone : Zone, chosenZone : ChosenZone, images : Images }
    -> Section transaction create
    -> Element (Msg transactionMsg createMsg)
parameters model section =
    column
        [ Region.description "pool parameters"
        , width <| px 335
        , height shrink
        , padding 20
        , spacing 20
        , Background.color Color.light500
        , Border.rounded 8
        ]
        [ pairParameters model section
        , maturityParameter model section
        ]


pairParameters :
    { model | images : Images }
    -> Section transaction create
    -> Element (Msg transactionMsg createMsg)
pairParameters model section =
    row
        [ width fill
        , height shrink
        , spacing 20
        ]
        [ tokenParameter TokenParam.Asset model section
        , tokenParameter TokenParam.Collateral model section
        ]


tokenParameter :
    TokenParam
    -> { model | images : Images }
    -> Section transaction create
    -> Element (Msg transactionMsg createMsg)
tokenParameter tokenParam { images } section =
    (case ( section, tokenParam ) of
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
        |> (\maybeToken ->
                column
                    [ width fill
                    , height shrink
                    , spacing 6
                    ]
                    [ el
                        [ width shrink
                        , height shrink
                        , Font.size 14
                        ]
                        ((case tokenParam of
                            TokenParam.Asset ->
                                "Asset"

                            TokenParam.Collateral ->
                                "Collateral"
                         )
                            |> text
                        )
                    , Input.button
                        [ Region.description
                            (case tokenParam of
                                TokenParam.Asset ->
                                    "asset button"

                                TokenParam.Collateral ->
                                    "collateral button"
                            )
                        , width fill
                        , height <| px 44
                        , maybeToken
                            |> Maybe.map (\_ -> Color.light100)
                            |> Maybe.withDefault Color.primary500
                            |> Background.color
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
                                (maybeToken
                                    |> Maybe.map
                                        (\token ->
                                            [ images
                                                |> Image.viewToken
                                                    [ width <| px 24
                                                    , alignLeft
                                                    , centerY
                                                    ]
                                                    token
                                            , el
                                                [ width shrink
                                                , height shrink
                                                , alignLeft
                                                , centerY
                                                , Font.size 14
                                                ]
                                                (token
                                                    |> Token.toSymbol
                                                    |> text
                                                )
                                            , images
                                                |> Image.discloser
                                                    [ width <| px 9
                                                    , alignRight
                                                    , centerY
                                                    ]
                                            ]
                                        )
                                    |> Maybe.withDefault
                                        [ el
                                            [ width shrink
                                            , height shrink
                                            , alignLeft
                                            , centerY
                                            , Font.size 14
                                            ]
                                            (text "Select Token")
                                        , images
                                            |> Image.discloser
                                                [ width <| px 9
                                                , alignRight
                                                , centerY
                                                ]
                                        ]
                                )
                        }
                    ]
           )


maturityParameter :
    { model | zone : Zone, chosenZone : ChosenZone, images : Images }
    -> Section transaction create
    -> Element (Msg transactionMsg createMsg)
maturityParameter { zone, chosenZone, images } section =
    column
        [ width fill
        , height shrink
        , spacing 6
        ]
        [ el
            [ width shrink
            , height shrink
            , Font.size 14
            ]
            (text "Maturity")
        , case section of
            Pool { pool } ->
                Input.button
                    [ Region.description "maturity button"
                    , width fill
                    , height <| px 44
                    , Background.color Color.light100
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
                                [ width shrink
                                , height shrink
                                , alignLeft
                                , centerY
                                , Font.size 14
                                ]
                                ((case chosenZone of
                                    ChosenZone.Here ->
                                        Just zone

                                    ChosenZone.UTC ->
                                        Just Time.utc

                                    ChosenZone.Unix ->
                                        Nothing
                                 )
                                    |> Maybe.map
                                        (\givenZone ->
                                            pool.maturity
                                                |> Maturity.toString
                                                    givenZone
                                        )
                                    |> Maybe.withDefault
                                        (pool.maturity
                                            |> Maturity.toUnix
                                            |> String.fromInt
                                        )
                                    |> text
                                )
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
                                [ width shrink
                                , height shrink
                                , alignLeft
                                , centerY
                                , Font.size 14
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
                    , Background.color Color.light100
                    , Border.rounded 8
                    ]
                    (el
                        [ width shrink
                        , height shrink
                        , alignLeft
                        , centerY
                        , Font.size 14
                        ]
                        (text "Select Pair First")
                    )
        ]
