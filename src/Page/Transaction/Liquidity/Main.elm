module Page.Transaction.Liquidity.Main exposing
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
import Blockchain.User.WriteCreate exposing (WriteCreate)
import Blockchain.User.WriteLiquidity exposing (WriteLiquidity)
import Data.Backdrop exposing (Backdrop)
import Data.Chain exposing (Chain)
import Data.ChosenZone exposing (ChosenZone)
import Data.Device exposing (Device(..))
import Data.ERC20 exposing (ERC20)
import Data.Images exposing (Images)
import Data.Maturity as Maturity
import Data.Offset exposing (Offset)
import Data.Or exposing (Or(..))
import Data.Pair as Pair exposing (Pair)
import Data.Parameter as Parameter exposing (Parameter)
import Data.Pool exposing (Pool)
import Data.PriceFeed exposing (PriceFeed)
import Data.Remote as Remote exposing (Remote(..))
import Data.Slippage exposing (Slippage)
import Data.Theme as Theme exposing (Theme)
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
import Page.Answer as Answer exposing (Answer)
import Page.PoolInfo as PoolInfo exposing (PoolInfo)
import Page.Query as Query
import Page.Transaction.Button as Button
import Page.Transaction.Liquidity.Add.Disabled as AddDisabled
import Page.Transaction.Liquidity.Add.Main as Add
import Page.Transaction.Liquidity.Empty as Empty
import Page.Transaction.Liquidity.New.Disabled as NewDisabled
import Page.Transaction.Liquidity.New.Main as New
import Page.Transaction.MaturityButton as MaturityButton
import Page.Transaction.Price exposing (Price)
import Page.Transaction.TokenButton as TokenButton
import Page.Transaction.Tooltip as Tooltip exposing (Tooltip)
import Process
import Task
import Time exposing (Posix)
import Utility.Color as Color
import Utility.Glass as Glass
import Utility.IconButton as IconButton
import Utility.Image as Image
import Utility.Input as Input
import Utility.ThemeColor as ThemeColor


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
    | DoesNotExist Price doesNotExist


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
    | OpenInputMaturity Pair
    | OpenConnect
    | OpenSettings
    | Approve ERC20
    | Liquidity WriteLiquidity
    | Create WriteCreate


init :
    { model | time : Posix }
    -> Blockchain
    -> Maybe Parameter
    -> ( Transaction, Cmd Msg )
init { time } blockchain parameter =
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
                        Remote.loading
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
    { model | time : Posix }
    -> Blockchain
    -> Pool
    -> PoolInfo
    -> ( Transaction, Cmd Msg )
initGivenPoolInfo { time } blockchain pool poolInfo =
    if pool.maturity |> Maturity.isActive time then
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
          --, get blockchain pool
        , Cmd.none
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


initGivenSpot :
    { model | time : Posix }
    -> Blockchain
    -> Pool
    -> Price
    -> ( Transaction, Cmd Msg )
initGivenSpot { time } blockchain pool priceFeed =
    if pool.maturity |> Maturity.isActive time then
        ( { state =
                New.init
                    |> DoesNotExist priceFeed
                    |> Success
                    |> Active
                    |> Pool pool
                    |> New
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


notSupported : Transaction
notSupported =
    { state = Add None
    , tooltip = Nothing
    }
        |> Transaction


update :
    { model | slippage : Slippage }
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

        ( GoToNew, Add (Pool pool (Active (Success (DoesNotExist priceFeed ())))) ) ->
            { transaction
                | state =
                    New.init
                        |> DoesNotExist priceFeed
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

        ( GoToNew, Add (Pool pool (Active (Loading _))) ) ->
            { transaction
                | state =
                    Remote.loading
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

        ( GoToAdd, New (Pool pool (Active (Success (DoesNotExist priceFeed _)))) ) ->
            { transaction
                | state =
                    ()
                        |> DoesNotExist priceFeed
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

        ( GoToAdd, New (Pool pool (Active (Loading _))) ) ->
            { transaction
                | state =
                    Remote.loading
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
                |> OpenInputMaturity
                |> Just
            )

        ( SelectMaturity, New (Pool pool _) ) ->
            ( transaction |> Transaction
            , Cmd.none
            , pool.pair
                |> OpenInputMaturity
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
            (if
                (chain == (blockchain |> Blockchain.toChain))
                    && (pool == currentPool)
             then
                case
                    ( result
                    , remote
                    )
                of
                    ( Ok (Right poolInfo), Success (Exist _ add) ) ->
                        add
                            |> Exist poolInfo
                            |> Success
                            |> Left
                            |> Just

                    ( Ok (Right poolInfo), Success (DoesNotExist _ ()) ) ->
                        Add.init
                            |> Exist poolInfo
                            |> Success
                            |> Left
                            |> Just

                    ( Ok (Right poolInfo), Failure error ) ->
                        error.add
                            |> Add.fromDisabled model blockchain pool poolInfo
                            |> (\( updated, cmd ) ->
                                    ( updated
                                        |> Exist poolInfo
                                        |> Success
                                    , cmd |> Cmd.map AddMsg
                                    )
                                        |> Right
                                        |> Just
                               )

                    ( Ok (Right poolInfo), Loading _ ) ->
                        Add.init
                            |> Exist poolInfo
                            |> Success
                            |> Left
                            |> Just

                    ( Ok (Left priceFeed), _ ) ->
                        DoesNotExist priceFeed ()
                            |> Success
                            |> Left
                            |> Just

                    ( Err error, Success (Exist _ add) ) ->
                        { http = error
                        , add = add |> Add.toDisabled
                        }
                            |> Failure
                            |> Left
                            |> Just

                    ( Err error, Success (DoesNotExist _ ()) ) ->
                        { http = error
                        , add = AddDisabled.init
                        }
                            |> Failure
                            |> Left
                            |> Just

                    ( Err error, Failure failure ) ->
                        { failure | http = error }
                            |> Failure
                            |> Left
                            |> Just

                    ( Err error, Loading _ ) ->
                        { http = error
                        , add = AddDisabled.init
                        }
                            |> Failure
                            |> Left
                            |> Just

             else
                Nothing
            )
                |> Maybe.map
                    (\or ->
                        case or of
                            Left state ->
                                ( { transaction
                                    | state =
                                        state
                                            |> Active
                                            |> Pool pool
                                            |> Add
                                  }
                                , Cmd.none
                                )

                            Right ( updated, cmd ) ->
                                ( { transaction
                                    | state =
                                        updated
                                            |> Active
                                            |> Pool pool
                                            |> Add
                                  }
                                , cmd
                                )
                    )
                |> Maybe.withDefault
                    ( transaction
                    , Cmd.none
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

        ( ReceiveAnswer chain pool result, New (Pool currentPool (Active remote)) ) ->
            (if
                (chain == (blockchain |> Blockchain.toChain))
                    && (pool == currentPool)
             then
                case
                    ( result
                    , remote
                    )
                of
                    ( Ok (Right poolInfo), _ ) ->
                        Exist poolInfo ()
                            |> Success
                            |> Left
                            |> Just

                    ( Ok (Left priceFeed), Success (Exist _ ()) ) ->
                        New.init
                            |> DoesNotExist priceFeed
                            |> Success
                            |> Left
                            |> Just

                    ( Ok (Left priceFeed), Success (DoesNotExist _ new) ) ->
                        new
                            |> DoesNotExist priceFeed
                            |> Success
                            |> Left
                            |> Just

                    ( Ok (Left priceFeed), Failure error ) ->
                        error.new
                            |> New.fromDisabled blockchain pool priceFeed
                            |> (\( updated, cmd ) ->
                                    ( updated
                                        |> DoesNotExist priceFeed
                                        |> Success
                                    , cmd |> Cmd.map NewMsg
                                    )
                                        |> Right
                                        |> Just
                               )

                    ( Ok (Left priceFeed), Loading _ ) ->
                        New.init
                            |> DoesNotExist priceFeed
                            |> Success
                            |> Left
                            |> Just

                    ( Err error, Success (Exist _ ()) ) ->
                        { http = error
                        , new = NewDisabled.init
                        }
                            |> Failure
                            |> Left
                            |> Just

                    ( Err error, Success (DoesNotExist _ new) ) ->
                        { http = error
                        , new = new |> New.toDisabled
                        }
                            |> Failure
                            |> Left
                            |> Just

                    ( Err error, Failure failure ) ->
                        { failure | http = error }
                            |> Failure
                            |> Left
                            |> Just

                    ( Err error, Loading _ ) ->
                        { http = error
                        , new = NewDisabled.init
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
                                            |> New
                                  }
                                , Cmd.none
                                )

                            Just (Right ( updated, cmd )) ->
                                ( { transaction
                                    | state =
                                        updated
                                            |> Active
                                            |> Pool pool
                                            |> New
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

        ( NewMsg newMsg, New (Pool pool (Active (Success (DoesNotExist priceFeed new)))) ) ->
            new
                |> New.update blockchain pool priceFeed newMsg
                |> (\( updated, cmd, maybeEffect ) ->
                        ( { transaction
                            | state =
                                updated
                                    |> DoesNotExist priceFeed
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


addEffects : Add.Effect -> Effect
addEffects effect =
    case effect of
        Add.OpenConnect ->
            OpenConnect

        Add.Approve erc20 ->
            Approve erc20

        Add.Liquidity writeLiquidity ->
            Liquidity writeLiquidity


newEffects : New.Effect -> Effect
newEffects effect =
    case effect of
        New.OpenConnect ->
            OpenConnect

        New.Approve erc20 ->
            Approve erc20

        New.Create writeCreate ->
            Create writeCreate


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

        New (Pool _ (Active (Success (DoesNotExist _ new)))) ->
            new
                |> New.subscriptions
                |> Sub.map NewMsg

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


toPoolInfo : Transaction -> Maybe (Or Price PoolInfo)
toPoolInfo (Transaction { state }) =
    case state of
        Add (Pool _ (Active (Success (Exist poolInfo _)))) ->
            poolInfo
                |> Right
                |> Just

        New (Pool _ (Active (Success (Exist poolInfo ())))) ->
            poolInfo
                |> Right
                |> Just

        Add (Pool _ (Active (Success (DoesNotExist priceFeed _)))) ->
            priceFeed
                |> Left
                |> Just

        New (Pool _ (Active (Success (DoesNotExist priceFeed _)))) ->
            priceFeed
                |> Left
                |> Just

        _ ->
            Nothing


view :
    { model
        | time : Posix
        , offset : Offset
        , chosenZone : ChosenZone
        , device : Device
        , backdrop : Backdrop
        , priceFeed : PriceFeed
        , images : Images
        , theme : Theme
    }
    -> Blockchain
    -> Transaction
    -> Element Msg
view ({ device, backdrop, theme } as model) blockchain (Transaction transaction) =
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
                        , buttons = theme |> Button.selectTokens |> map never
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
                        , buttons = theme |> Button.selectTokens |> map never
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
                        , buttons = theme |> Button.selectTokens |> map never
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
                        , buttons = theme |> Button.selectTokens |> map never
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
                        , buttons = theme |> Button.selectTokens |> map never
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
                        , buttons = theme |> Button.selectTokens |> map never
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
                        , buttons = theme |> Button.selectMaturity |> map never
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
                        , buttons = theme |> Button.selectMaturity |> map never
                        }
                   )

        Add (Pool pool Matured) ->
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
                |> (\{ first, second, third } ->
                        { first = first |> map never
                        , second = second |> map never
                        , third = third |> map never
                        , buttons = theme |> Button.matured |> map never
                        }
                   )

        New (Pool pool Matured) ->
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
                |> (\{ first, second, third } ->
                        { first = first |> map never
                        , second = second |> map never
                        , third = third |> map never
                        , buttons = theme |> Button.matured |> map never
                        }
                   )

        Add (Pool pool (Active (Loading _))) ->
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
                |> (\{ first, second, third } ->
                        { first = first |> map never
                        , second = second |> map never
                        , third = third |> map never
                        , buttons = theme |> Button.loading |> map never
                        }
                   )

        New (Pool pool (Active (Loading _))) ->
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
                |> (\{ first, second, third } ->
                        { first = first |> map never
                        , second = second |> map never
                        , third = third |> map never
                        , buttons = theme |> Button.loading |> map never
                        }
                   )

        Add (Pool pool (Active (Failure { http, add }))) ->
            add
                |> AddDisabled.view model blockchain pool
                |> (\{ first, second, third } ->
                        { first = first |> map never
                        , second = second |> map never
                        , third = third |> map never
                        , buttons = Button.error http |> map never
                        }
                   )

        New (Pool pool (Active (Failure { http, new }))) ->
            new
                |> NewDisabled.view model blockchain pool
                |> (\{ first, second, third } ->
                        { first = first |> map never
                        , second = second |> map never
                        , third = third |> map never
                        , buttons = Button.error http |> map never
                        }
                   )

        Add (Pool pool (Active (Success (DoesNotExist _ ())))) ->
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
                |> (\{ first, second, third } ->
                        { first = first |> map never
                        , second = second |> map never
                        , third = third |> map never
                        , buttons = theme |> Button.doesNotExist |> map never
                        }
                   )

        New (Pool pool (Active (Success (DoesNotExist _ new)))) ->
            new
                |> New.view model blockchain pool
                |> (\{ first, second, third, buttons } ->
                        { first = first |> map NewMsg
                        , second = second |> map NewMsg
                        , third = third |> map NewMsg
                        , buttons = buttons |> map NewMsg
                        }
                   )

        Add (Pool pool (Active (Success (Exist poolInfo add)))) ->
            add
                |> Add.view model blockchain pool poolInfo
                |> (\{ first, second, third, buttons } ->
                        { first = first |> map AddMsg
                        , second = second |> map AddMsg
                        , third = third |> map AddMsg
                        , buttons = buttons |> map AddMsg
                        }
                   )

        New (Pool pool (Active (Success (Exist _ ())))) ->
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
                |> (\{ first, second, third } ->
                        { first = first |> map never
                        , second = second |> map never
                        , third = third |> map never
                        , buttons = theme |> Button.exist |> map never
                        }
                   )
    )
        |> (\{ first, second, third, buttons } ->
                column
                    ([ (case transaction.state of
                            Add _ ->
                                "liquidity"

                            New _ ->
                                "create pool"
                       )
                        |> Region.description
                     , width shrink
                     , height shrink
                     , (case device of
                            Desktop ->
                                23

                            _ ->
                                15
                       )
                        |> padding
                     , spacing 16
                     , Border.rounded 8
                     , Border.width 1
                     , theme |> ThemeColor.border |> Border.color
                     ]
                        ++ Glass.background backdrop theme
                    )
                    [ row
                        [ width fill
                        , height shrink
                        , spacing 24
                        ]
                        [ case transaction.state of
                            Add _ ->
                                none

                            New _ ->
                                IconButton.back model GoToAdd
                        , el
                            [ width shrink
                            , height shrink
                            , paddingXY 0 4
                            , Font.size 24
                            , theme |> ThemeColor.text |> Font.color
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
                    , case device of
                        Desktop ->
                            row
                                [ width shrink
                                , height shrink
                                , spacing 24
                                ]
                                [ column
                                    [ width <| px 343
                                    , height shrink
                                    , spacing 16
                                    , alignTop
                                    ]
                                    [ parameters model transaction
                                    , first
                                    ]
                                , column
                                    [ width <| px 343
                                    , height shrink
                                    , spacing 16
                                    , alignTop
                                    ]
                                    [ second
                                    , third
                                    , buttons
                                    ]
                                ]

                        _ ->
                            column
                                [ width <| px 343
                                , height shrink
                                , spacing 16
                                ]
                                [ parameters model transaction
                                , first
                                , second
                                , third
                                , buttons
                                ]
                    ]
           )


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
    { model | images : Images, theme : Theme }
    -> Element Msg
settingsButton { images, theme } =
    Input.button
        [ width shrink
        , height shrink
        , alignRight
        , centerY
        ]
        { onPress = Just ClickSettings
        , label =
            images
                |> (case theme of
                        Theme.Dark ->
                            Image.option

                        Theme.Light ->
                            Image.settingsSecondary
                   )
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
        , theme : Theme
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
        , model.theme |> ThemeColor.sectionBackground |> Background.color
        , Border.rounded 8
        ]
        [ pairParameters model transaction
        , maturityParameter model transaction
        ]


pairParameters :
    { model | images : Images, theme : Theme }
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
    { model | images : Images, theme : Theme }
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
            , model.theme |> ThemeColor.actionElemLabel |> Font.color
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
    { model | images : Images, theme : Theme }
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
        , theme : Theme
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
            , model.theme |> ThemeColor.actionElemLabel |> Font.color
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
                model.theme
                    |> MaturityButton.disabled
                    |> map never
        ]
