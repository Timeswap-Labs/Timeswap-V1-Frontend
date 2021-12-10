module Page.Transaction.Liquidity.Main exposing
    ( Effect(..)
    , Msg
    , Transaction
    , init
    , initGivenPoolInfo
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
        , rotate
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
import Page.Transaction.Liquidity.Add.Disabled as AddDisabled
import Page.Transaction.Liquidity.Add.Main as Add
import Page.Transaction.Liquidity.Empty as Empty
import Page.Transaction.Liquidity.New.Disabled as NewDisabled
import Page.Transaction.Liquidity.New.Main as New
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
import Utility.Input as Input


type Transaction
    = Transaction
        { state : State
        , tooltip : Maybe Tooltip
        }


type State
    = Add (PoolParam (Status AddDisabled (PoolState Add.Transaction ())))
    | New (PoolParam (Status NewDisabled (PoolState () New.Transaction)))


type PoolParam status
    = None
    | Asset Token
    | Collateral Token
    | Pair Pair
    | Pool Pool status


type Status error poolState
    = Active (Remote error poolState)
    | Matured


type alias AddDisabled =
    { http : Http.Error
    , add : AddDisabled.Transaction
    }


type alias NewDisabled =
    { http : Http.Error
    , new : NewDisabled.Transaction
    }


type PoolState exist doesNotExist
    = Exist PoolInfo exist
    | DoesNotExist doesNotExist


type Msg
    = GoToNew
    | GoToAdd
    | SelectToken TokenParam
    | SelectMaturity
    | QueryAgain
    | ReceiveAnswer Chain Pool (Result Http.Error Answer)
    | CheckMaturity Posix
    | AddMsg Add.Msg
    | NewMsg New.Msg
    | ClickSettings
    | OnMouseEnter Tooltip
    | OnMouseLeave


type Effect
    = OpenTokenList TokenParam
    | OpenMaturityList Pair
    | OpenChooseMaturity Pair
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
            ( { state = Add None
              , tooltip = Nothing
              }
                |> Transaction
            , Cmd.none
            )

        Just (Parameter.Asset asset) ->
            ( { state = Asset asset |> Add
              , tooltip = Nothing
              }
                |> Transaction
            , Cmd.none
            )

        Just (Parameter.Collateral collateral) ->
            ( { state = Collateral collateral |> Add
              , tooltip = Nothing
              }
                |> Transaction
            , Cmd.none
            )

        Just (Parameter.Pair pair) ->
            ( { state = Pair pair |> Add
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
                            |> Add
                  , tooltip = Nothing
                  }
                    |> Transaction
                , get blockchain pool
                )

            else
                ( { state =
                        Matured
                            |> Pool pool
                            |> Add
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
            Add.init
                |> Exist poolInfo
                |> Success
                |> Active
                |> Pool pool
                |> Add
      , tooltip = Nothing
      }
        |> Transaction
    , get blockchain pool
    )


update :
    { model
        | time : Posix
        , slippage : Slippage
        , deadline : Deadline
    }
    -> Blockchain
    -> Msg
    -> Transaction
    -> ( Transaction, Cmd Msg, Maybe Effect )
update model blockchain msg (Transaction transaction) =
    case ( msg, transaction.state ) of
        ( GoToNew, Add None ) ->
            { transaction | state = New None }
                |> noCmdAndEffect

        ( GoToNew, Add (Asset asset) ) ->
            { transaction | state = Asset asset |> New }
                |> noCmdAndEffect

        ( GoToNew, Add (Collateral collateral) ) ->
            { transaction | state = Collateral collateral |> New }
                |> noCmdAndEffect

        ( GoToNew, Add (Pair pair) ) ->
            { transaction | state = Pair pair |> New }
                |> noCmdAndEffect

        ( GoToNew, Add (Pool pool (Active (Success (Exist poolInfo _)))) ) ->
            { transaction
                | state =
                    ()
                        |> Exist poolInfo
                        |> Success
                        |> Active
                        |> Pool pool
                        |> New
            }
                |> noCmdAndEffect

        ( GoToNew, Add (Pool pool (Active (Success (DoesNotExist ())))) ) ->
            { transaction
                | state =
                    New.init
                        |> DoesNotExist
                        |> Success
                        |> Active
                        |> Pool pool
                        |> New
            }
                |> noCmdAndEffect

        ( GoToNew, Add (Pool pool (Active (Failure error))) ) ->
            { transaction
                | state =
                    { http = error.http
                    , new = NewDisabled.init
                    }
                        |> Failure
                        |> Active
                        |> Pool pool
                        |> New
            }
                |> noCmdAndEffect

        ( GoToNew, Add (Pool pool (Active Loading)) ) ->
            { transaction
                | state =
                    Loading
                        |> Active
                        |> Pool pool
                        |> New
            }
                |> noCmdAndEffect

        ( GoToNew, Add (Pool pool Matured) ) ->
            { transaction
                | state =
                    Matured
                        |> Pool pool
                        |> New
            }
                |> noCmdAndEffect

        ( GoToAdd, New None ) ->
            { transaction | state = Add None }
                |> noCmdAndEffect

        ( GoToAdd, New (Asset asset) ) ->
            { transaction | state = Asset asset |> Add }
                |> noCmdAndEffect

        ( GoToAdd, New (Collateral collateral) ) ->
            { transaction | state = Collateral collateral |> Add }
                |> noCmdAndEffect

        ( GoToAdd, New (Pair pair) ) ->
            { transaction | state = Pair pair |> Add }
                |> noCmdAndEffect

        ( GoToAdd, New (Pool pool (Active (Success (Exist poolInfo ())))) ) ->
            { transaction
                | state =
                    Add.init
                        |> Exist poolInfo
                        |> Success
                        |> Active
                        |> Pool pool
                        |> Add
            }
                |> noCmdAndEffect

        ( GoToAdd, New (Pool pool (Active (Success (DoesNotExist _)))) ) ->
            { transaction
                | state =
                    ()
                        |> DoesNotExist
                        |> Success
                        |> Active
                        |> Pool pool
                        |> Add
            }
                |> noCmdAndEffect

        ( GoToAdd, New (Pool pool (Active (Failure error))) ) ->
            { transaction
                | state =
                    { http = error.http
                    , add = AddDisabled.init
                    }
                        |> Failure
                        |> Active
                        |> Pool pool
                        |> Add
            }
                |> noCmdAndEffect

        ( GoToAdd, New (Pool pool (Active Loading)) ) ->
            { transaction
                | state =
                    Loading
                        |> Active
                        |> Pool pool
                        |> Add
            }
                |> noCmdAndEffect

        ( GoToAdd, New (Pool pool Matured) ) ->
            { transaction
                | state =
                    Matured
                        |> Pool pool
                        |> Add
            }
                |> noCmdAndEffect

        ( SelectToken tokenParam, _ ) ->
            ( transaction |> Transaction
            , Cmd.none
            , tokenParam
                |> OpenTokenList
                |> Just
            )

        ( SelectMaturity, Add (Pair pair) ) ->
            ( transaction |> Transaction
            , Cmd.none
            , pair
                |> OpenMaturityList
                |> Just
            )

        ( SelectMaturity, Add (Pool pool _) ) ->
            ( transaction |> Transaction
            , Cmd.none
            , pool.pair
                |> OpenMaturityList
                |> Just
            )

        ( SelectMaturity, New (Pair pair) ) ->
            ( transaction |> Transaction
            , Cmd.none
            , pair
                |> OpenChooseMaturity
                |> Just
            )

        ( SelectMaturity, New (Pool pool _) ) ->
            ( transaction |> Transaction
            , Cmd.none
            , pool.pair
                |> OpenChooseMaturity
                |> Just
            )

        ( QueryAgain, Add (Pool pool (Active (Success _))) ) ->
            ( transaction |> Transaction
            , get blockchain pool
            , Nothing
            )

        ( QueryAgain, Add (Pool pool (Active (Failure _))) ) ->
            ( transaction |> Transaction
            , get blockchain pool
            , Nothing
            )

        ( QueryAgain, New (Pool pool (Active (Success _))) ) ->
            ( transaction |> Transaction
            , get blockchain pool
            , Nothing
            )

        ( QueryAgain, New (Pool pool (Active (Failure _))) ) ->
            ( transaction |> Transaction
            , get blockchain pool
            , Nothing
            )

        ( ReceiveAnswer chain pool result, Add (Pool currentPool (Active remote)) ) ->
            ( (if
                (chain == (blockchain |> Blockchain.toChain))
                    && (pool == currentPool)
               then
                case
                    ( result
                    , remote
                    )
                of
                    ( Ok (Just poolInfo), Success (Exist _ add) ) ->
                        add
                            |> Exist poolInfo
                            |> Success
                            |> Just

                    ( Ok (Just poolInfo), Success (DoesNotExist ()) ) ->
                        Add.init
                            |> Exist poolInfo
                            |> Success
                            |> Just

                    ( Ok (Just poolInfo), Failure error ) ->
                        error.add
                            |> Add.fromAddError
                            |> Exist poolInfo
                            |> Success
                            |> Just

                    ( Ok (Just poolInfo), Loading ) ->
                        Add.init
                            |> Exist poolInfo
                            |> Success
                            |> Just

                    ( Ok Nothing, _ ) ->
                        DoesNotExist ()
                            |> Success
                            |> Just

                    ( Err error, Success (Exist _ add) ) ->
                        { http = error
                        , add = add |> Add.toAddError
                        }
                            |> Failure
                            |> Just

                    ( Err error, Success (DoesNotExist ()) ) ->
                        { http = error
                        , add = AddDisabled.init
                        }
                            |> Failure
                            |> Just

                    ( Err error, Failure failure ) ->
                        { failure | http = error }
                            |> Failure
                            |> Just

                    ( Err error, Loading ) ->
                        { http = error
                        , add = AddDisabled.init
                        }
                            |> Failure
                            |> Just

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
                                    |> Add
                        }
                    )
                |> Maybe.withDefault transaction
                |> Transaction
            , Process.sleep 5000
                |> Task.perform (\_ -> QueryAgain)
            , Nothing
            )

        ( ReceiveAnswer chain pool result, New (Pool currentPool (Active remote)) ) ->
            ( (if
                (chain == (blockchain |> Blockchain.toChain))
                    && (pool == currentPool)
               then
                case
                    ( result
                    , remote
                    )
                of
                    ( Ok (Just poolInfo), _ ) ->
                        Exist poolInfo ()
                            |> Success
                            |> Just

                    ( Ok Nothing, Success (Exist _ ()) ) ->
                        New.init
                            |> DoesNotExist
                            |> Success
                            |> Just

                    ( Ok Nothing, Success (DoesNotExist _) ) ->
                        Nothing

                    ( Ok Nothing, Failure error ) ->
                        error.new
                            |> New.fromNewError
                            |> DoesNotExist
                            |> Success
                            |> Just

                    ( Ok Nothing, Loading ) ->
                        New.init
                            |> DoesNotExist
                            |> Success
                            |> Just

                    ( Err error, Success (Exist _ ()) ) ->
                        { http = error
                        , new = NewDisabled.init
                        }
                            |> Failure
                            |> Just

                    ( Err error, Success (DoesNotExist new) ) ->
                        { http = error
                        , new = new |> New.toNewError
                        }
                            |> Failure
                            |> Just

                    ( Err error, Failure failure ) ->
                        { failure | http = error }
                            |> Failure
                            |> Just

                    ( Err error, Loading ) ->
                        { http = error
                        , new = NewDisabled.init
                        }
                            |> Failure
                            |> Just

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
                                    |> New
                        }
                    )
                |> Maybe.withDefault transaction
                |> Transaction
            , Process.sleep 5000
                |> Task.perform (\_ -> QueryAgain)
            , Nothing
            )

        ( CheckMaturity posix, Add (Pool pool (Active _)) ) ->
            (if pool.maturity |> Maturity.isActive posix then
                transaction

             else
                { transaction
                    | state =
                        Matured
                            |> Pool pool
                            |> Add
                }
            )
                |> noCmdAndEffect

        ( CheckMaturity posix, New (Pool pool (Active _)) ) ->
            (if pool.maturity |> Maturity.isActive posix then
                transaction

             else
                { transaction
                    | state =
                        Matured
                            |> Pool pool
                            |> New
                }
            )
                |> noCmdAndEffect

        ( AddMsg addMsg, Add (Pool pool (Active (Success (Exist poolInfo add)))) ) ->
            add
                |> Add.update model blockchain pool poolInfo addMsg
                |> (\( updated, cmd, maybeEffect ) ->
                        ( { transaction
                            | state =
                                updated
                                    |> Exist poolInfo
                                    |> Success
                                    |> Active
                                    |> Pool pool
                                    |> Add
                          }
                            |> Transaction
                        , cmd |> Cmd.map AddMsg
                        , maybeEffect |> Maybe.map addEffects
                        )
                   )

        ( NewMsg newMsg, New (Pool pool (Active (Success (DoesNotExist new)))) ) ->
            new
                |> New.update model blockchain pool newMsg
                |> (\( updated, cmd, maybeEffect ) ->
                        ( { transaction
                            | state =
                                updated
                                    |> DoesNotExist
                                    |> Success
                                    |> Active
                                    |> Pool pool
                                    |> New
                          }
                            |> Transaction
                        , cmd |> Cmd.map NewMsg
                        , maybeEffect |> Maybe.map newEffects
                        )
                   )

        ( OnMouseEnter tooltip, _ ) ->
            { transaction | tooltip = Just tooltip }
                |> noCmdAndEffect

        ( OnMouseLeave, _ ) ->
            { transaction | tooltip = Nothing }
                |> noCmdAndEffect

        _ ->
            transaction |> noCmdAndEffect


addEffects : Add.Effect -> Effect
addEffects effect =
    case effect of
        Add.OpenConnect ->
            OpenConnect

        Add.OpenConfirm ->
            OpenConfirm


newEffects : New.Effect -> Effect
newEffects effect =
    case effect of
        New.OpenConnect ->
            OpenConnect

        New.OpenConfirm ->
            OpenConfirm


noCmdAndEffect :
    { state : State, tooltip : Maybe Tooltip }
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
        Add (Pool _ (Active (Success (Exist _ add)))) ->
            add
                |> Add.subscriptions
                |> Sub.map AddMsg

        _ ->
            Sub.none
    ]
        |> Sub.batch


toParameter : Transaction -> Maybe Parameter
toParameter (Transaction { state }) =
    case state of
        Add None ->
            Nothing

        New None ->
            Nothing

        Add (Asset asset) ->
            Parameter.Asset asset
                |> Just

        New (Asset asset) ->
            Parameter.Asset asset
                |> Just

        Add (Collateral collateral) ->
            Parameter.Collateral collateral
                |> Just

        New (Collateral collateral) ->
            Parameter.Collateral collateral
                |> Just

        Add (Pair pair) ->
            Parameter.Pair pair
                |> Just

        New (Pair pair) ->
            Parameter.Pair pair
                |> Just

        Add (Pool pool _) ->
            Parameter.Pool pool
                |> Just

        New (Pool pool _) ->
            Parameter.Pool pool
                |> Just


toPoolInfo : Transaction -> Maybe PoolInfo
toPoolInfo (Transaction { state }) =
    case state of
        Add (Pool _ (Active (Success (Exist poolInfo _)))) ->
            poolInfo |> Just

        New (Pool _ (Active (Success (Exist poolInfo ())))) ->
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
    (case transaction.state of
        Add None ->
            { asset = Nothing
            , collateral = Nothing
            }
                |> Empty.view model
                |> (\{ first, second, third } ->
                        { first = first |> map never
                        , second = second |> map never
                        , third = third |> map never
                        , buttons = none |> Debug.log "later"
                        }
                   )

        New None ->
            { asset = Nothing
            , collateral = Nothing
            }
                |> Empty.view model
                |> (\{ first, second, third } ->
                        { first = first |> map never
                        , second = second |> map never
                        , third = third |> map never
                        , buttons = none |> Debug.log "later"
                        }
                   )

        Add (Asset asset) ->
            { asset = Just asset
            , collateral = Nothing
            }
                |> Empty.view model
                |> (\{ first, second, third } ->
                        { first = first |> map never
                        , second = second |> map never
                        , third = third |> map never
                        , buttons = none |> Debug.log "later"
                        }
                   )

        New (Asset asset) ->
            { asset = Just asset
            , collateral = Nothing
            }
                |> Empty.view model
                |> (\{ first, second, third } ->
                        { first = first |> map never
                        , second = second |> map never
                        , third = third |> map never
                        , buttons = none |> Debug.log "later"
                        }
                   )

        Add (Collateral collateral) ->
            { asset = Nothing
            , collateral = Just collateral
            }
                |> Empty.view model
                |> (\{ first, second, third } ->
                        { first = first |> map never
                        , second = second |> map never
                        , third = third |> map never
                        , buttons = none |> Debug.log "later"
                        }
                   )

        New (Collateral collateral) ->
            { asset = Nothing
            , collateral = Just collateral
            }
                |> Empty.view model
                |> (\{ first, second, third } ->
                        { first = first |> map never
                        , second = second |> map never
                        , third = third |> map never
                        , buttons = none |> Debug.log "later"
                        }
                   )

        Add (Pair pair) ->
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
                |> (\{ first, second, third } ->
                        { first = first |> map never
                        , second = second |> map never
                        , third = third |> map never
                        , buttons = none |> Debug.log "later"
                        }
                   )

        New (Pair pair) ->
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
                |> (\{ first, second, third } ->
                        { first = first |> map never
                        , second = second |> map never
                        , third = third |> map never
                        , buttons = none |> Debug.log "later"
                        }
                   )

        Add (Pool pool _) ->
            { first = none
            , second = none
            , third = none
            , buttons = none
            }
                |> Debug.log "later"

        New (Pool pool _) ->
            { first = none
            , second = none
            , third = none
            , buttons = none
            }
                |> Debug.log "later"
    )
        |> (\{ first, second, third, buttons } ->
                column
                    [ (case transaction.state of
                        Add _ ->
                            "liquidity"

                        New _ ->
                            "create pool"
                      )
                        |> Region.description
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
                        , spacing 24
                        ]
                        [ case transaction.state of
                            Add _ ->
                                none

                            New _ ->
                                backButton model
                        , el
                            [ width shrink
                            , height shrink
                            , paddingXY 0 4
                            , Font.size 24
                            , Font.color Color.light100
                            , Font.bold
                            ]
                            ((case transaction.state of
                                Add _ ->
                                    "Liquidity"

                                New _ ->
                                    "Create Pool"
                             )
                                |> text
                            )
                        , case transaction.state of
                            Add _ ->
                                createPoolButton model

                            New _ ->
                                none
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
                            , third
                            , buttons
                            ]
                        ]
                    ]
           )


backButton :
    { model | images : Images }
    -> Element Msg
backButton { images } =
    Input.button
        [ width shrink
        , height shrink
        , alignLeft
        , centerY
        ]
        { onPress = Just GoToAdd
        , label =
            images
                |> Image.arrowDown
                    [ width <| px 18
                    , height <| px 18
                    , rotate (pi / 2)
                    ]
        }


createPoolButton :
    { model | images : Images }
    -> Element Msg
createPoolButton { images } =
    Input.button
        [ width shrink
        , height shrink
        , alignRight
        , centerY
        ]
        { onPress = Just GoToNew
        , label =
            row
                [ width shrink
                , height shrink
                , spacing 4
                ]
                [ images
                    |> Image.plusPositive
                        [ width <| px 12
                        , height <| px 12
                        , centerY
                        ]
                , el
                    [ width shrink
                    , height shrink
                    , Font.size 16
                    , paddingXY 0 4
                    , Font.bold
                    , Font.color Color.positive400
                    ]
                    (text "Create Pool")
                ]
        }


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
        ( Add (Asset asset), TokenParam.Asset ) ->
            Just asset

        ( New (Asset asset), TokenParam.Asset ) ->
            Just asset

        ( Add (Collateral collateral), TokenParam.Collateral ) ->
            Just collateral

        ( New (Collateral collateral), TokenParam.Collateral ) ->
            Just collateral

        ( Add (Pair pair), TokenParam.Asset ) ->
            pair
                |> Pair.toAsset
                |> Just

        ( New (Pair pair), TokenParam.Asset ) ->
            pair
                |> Pair.toAsset
                |> Just

        ( Add (Pair pair), TokenParam.Collateral ) ->
            pair
                |> Pair.toCollateral
                |> Just

        ( New (Pair pair), TokenParam.Collateral ) ->
            pair
                |> Pair.toCollateral
                |> Just

        ( Add (Pool pool _), TokenParam.Asset ) ->
            pool.pair
                |> Pair.toAsset
                |> Just

        ( New (Pool pool _), TokenParam.Asset ) ->
            pool.pair
                |> Pair.toAsset
                |> Just

        ( Add (Pool pool _), TokenParam.Collateral ) ->
            pool.pair
                |> Pair.toCollateral
                |> Just

        ( New (Pool pool _), TokenParam.Collateral ) ->
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
            Add (Pool pool _) ->
                MaturityButton.view model
                    { onPress = SelectMaturity
                    , onMouseEnter = OnMouseEnter
                    , onMouseLeave = OnMouseLeave
                    , tooltip = Tooltip.Maturity
                    , opened = tooltip
                    , maturity = Just pool.maturity
                    }

            New (Pool pool _) ->
                MaturityButton.view model
                    { onPress = SelectMaturity
                    , onMouseEnter = OnMouseEnter
                    , onMouseLeave = OnMouseLeave
                    , tooltip = Tooltip.Maturity
                    , opened = tooltip
                    , maturity = Just pool.maturity
                    }

            Add (Pair _) ->
                MaturityButton.view model
                    { onPress = SelectMaturity
                    , onMouseEnter = OnMouseEnter
                    , onMouseLeave = OnMouseLeave
                    , tooltip = Tooltip.Maturity
                    , opened = tooltip
                    , maturity = Nothing
                    }

            New (Pair _) ->
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