module Page.Transaction.Borrow.Main exposing
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
import Blockchain.User.WriteBorrow exposing (WriteBorrow)
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
        , newTabLink
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
import Page.PoolInfo exposing (PoolInfo)
import Page.Query as Query
import Page.Transaction.Borrow.Borrow.Disabled as Disabled
import Page.Transaction.Borrow.Borrow.Main as Borrow
import Page.Transaction.Borrow.Empty as Empty
import Page.Transaction.Button as Button
import Page.Transaction.MaturityButton as MaturityButton
import Page.Transaction.Price exposing (Price)
import Page.Transaction.TokenButton as TokenButton
import Page.Transaction.Tooltip as Tooltip exposing (Tooltip)
import Process
import Task
import Time exposing (Posix)
import Utility.Glass as Glass
import Utility.Image as Image
import Utility.ThemeColor as ThemeColor


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


type alias Error =
    { http : Http.Error
    , borrow : Disabled.Transaction
    }


type Status
    = Active (Remote Error PoolState)
    | Matured


type PoolState
    = Exist PoolInfo Borrow.Transaction
    | DoesNotExist Price


type Msg
    = SelectToken TokenParam
    | SelectMaturity
    | QueryAgain
    | ReceiveAnswer Chain Pool (Result Http.Error Answer)
    | CheckMaturity Posix
    | BorrowMsg Borrow.Msg
    | ClickSettings
    | OnMouseEnter Tooltip
    | OnMouseLeave


type Effect
    = OpenTokenList TokenParam
    | OpenMaturityList Pair
    | OpenConnect
    | OpenSettings
    | Approve ERC20
    | Borrow WriteBorrow


init :
    { model | time : Posix, endPoint : String }
    -> Blockchain
    -> Maybe Parameter
    -> ( Transaction, Cmd Msg )
init { time, endPoint } blockchain parameter =
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
                        Remote.loading
                            |> Active
                            |> Pool pool
                  , tooltip = Nothing
                  }
                    |> Transaction
                , get blockchain endPoint pool
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
    -> Pool
    -> PoolInfo
    -> ( Transaction, Cmd Msg )
initGivenPoolInfo { time } pool poolInfo =
    if pool.maturity |> Maturity.isActive time then
        ( { state =
                Borrow.init
                    |> Exist poolInfo
                    |> Success
                    |> Active
                    |> Pool pool
          , tooltip = Nothing
          }
            |> Transaction
          --, get blockchain pool
        , Cmd.none
        )

    else
        ( { state = Matured |> Pool pool
          , tooltip = Nothing
          }
            |> Transaction
        , Cmd.none
        )


initGivenSpot :
    { model | time : Posix, endPoint : String }
    -> Blockchain
    -> Pool
    -> Price
    -> ( Transaction, Cmd Msg )
initGivenSpot { time, endPoint } blockchain pool priceFeed =
    if pool.maturity |> Maturity.isActive time then
        ( { state =
                DoesNotExist priceFeed
                    |> Success
                    |> Active
                    |> Pool pool
          , tooltip = Nothing
          }
            |> Transaction
        , get blockchain endPoint pool
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
    { model | slippage : Slippage, endPoint : String }
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
            , get blockchain model.endPoint pool
            , Nothing
            )

        ( QueryAgain, Pool pool (Active (Failure _)) ) ->
            ( transaction |> Transaction
            , get blockchain model.endPoint pool
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
                    ( Ok (Right poolInfo), Success (Exist _ borrow) ) ->
                        borrow
                            |> Exist poolInfo
                            |> Success
                            |> Left
                            |> Just

                    ( Ok (Right poolInfo), Success (DoesNotExist _) ) ->
                        Borrow.init
                            |> Exist poolInfo
                            |> Success
                            |> Left
                            |> Just

                    ( Ok (Right poolInfo), Failure error ) ->
                        error.borrow
                            |> Borrow.fromDisabled model blockchain pool poolInfo
                            |> (\( updated, cmd ) ->
                                    ( updated
                                        |> Exist poolInfo
                                        |> Success
                                    , cmd |> Cmd.map BorrowMsg
                                    )
                                        |> Right
                                        |> Just
                               )

                    ( Ok (Right poolInfo), Loading _ ) ->
                        Borrow.init
                            |> Exist poolInfo
                            |> Success
                            |> Left
                            |> Just

                    ( Ok (Left priceFeed), _ ) ->
                        DoesNotExist priceFeed
                            |> Success
                            |> Left
                            |> Just

                    ( Err error, Success (Exist _ borrow) ) ->
                        { http = error
                        , borrow = borrow |> Borrow.toDisabled
                        }
                            |> Failure
                            |> Left
                            |> Just

                    ( Err error, Success (DoesNotExist _) ) ->
                        { http = error
                        , borrow = Disabled.init
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
                        , borrow = Disabled.init
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

        ( BorrowMsg borrowMsg, Pool pool (Active (Success (Exist poolInfo borrow))) ) ->
            borrow
                |> Borrow.update model blockchain pool poolInfo borrowMsg
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
                        , cmd |> Cmd.map BorrowMsg
                        , maybeEffect |> Maybe.map borrowEffects
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


borrowEffects : Borrow.Effect -> Effect
borrowEffects effect =
    case effect of
        Borrow.OpenConnect ->
            OpenConnect

        Borrow.Approve erc20 ->
            Approve erc20

        Borrow.Borrow writeBorrow ->
            Borrow writeBorrow


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
    -> String
    -> Pool
    -> Cmd Msg
get blockchain endPoint pool =
    blockchain
        |> Blockchain.toChain
        |> (\chain ->
                Http.get
                    { url =
                        pool
                            |> Query.toUrlString chain endPoint
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
        Pool _ (Active (Success (Exist _ borrow))) ->
            borrow
                |> Borrow.subscriptions
                |> Sub.map BorrowMsg

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


toPoolInfo : Transaction -> Maybe (Or Price PoolInfo)
toPoolInfo (Transaction { state }) =
    case state of
        Pool _ (Active (Success (Exist poolInfo _))) ->
            poolInfo
                |> Right
                |> Just

        Pool _ (Active (Success (DoesNotExist priceFeed))) ->
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
        , priceFeed : PriceFeed
        , backdrop : Backdrop
        , images : Images
        , theme : Theme
    }
    -> Blockchain
    -> Transaction
    -> Element Msg
view ({ device, backdrop, theme } as model) blockchain (Transaction transaction) =
    (case transaction.state of
        None ->
            { asset = Nothing
            , collateral = Nothing
            }
                |> Empty.view model
                |> (\{ first, second } ->
                        { first = first |> map never
                        , second = second |> map never
                        , buttons = theme |> Button.selectTokens |> map never
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
                        , buttons = theme |> Button.selectTokens |> map never
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
                        , buttons = theme |> Button.selectTokens |> map never
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
                        , buttons = theme |> Button.selectMaturity |> map never
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
                        , buttons = theme |> Button.matured |> map never
                        }
                   )

        Pool pool (Active (Loading _)) ->
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
                        , buttons = theme |> Button.loading |> map never
                        }
                   )

        Pool pool (Active (Failure { http, borrow })) ->
            borrow
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
                        , buttons = theme |> Button.doesNotExist |> map never
                        }
                   )

        Pool pool (Active (Success (Exist poolInfo borrow))) ->
            borrow
                |> Borrow.view model blockchain pool poolInfo
                |> (\{ first, second, buttons } ->
                        { first = first |> map BorrowMsg
                        , second = second |> map BorrowMsg
                        , buttons = buttons |> map BorrowMsg
                        }
                   )
    )
        |> (\{ first, second, buttons } ->
                column
                    ([ Region.description "borrow transaction"
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
                        , spacing 12
                        ]
                        [ el
                            [ width shrink
                            , height shrink
                            , paddingXY 0 4
                            , Font.size 24
                            , theme |> ThemeColor.text |> Font.color
                            , Font.bold
                            ]
                            (text "Borrow")
                        , tutorialLink model
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
                                , buttons
                                ]
                    ]
           )


tutorialLink :
    { model | images : Images, theme : Theme }
    -> Element Msg
tutorialLink { images, theme } =
    newTabLink []
        { url = "https://youtu.be/A2Nwa3jIlwU"
        , label =
            row
                [ width shrink
                , height shrink
                , spacing 8
                , paddingXY 8 4
                , Border.width 1
                , theme |> ThemeColor.textboxBorder |> Border.color
                , Border.rounded 8
                ]
                [ el [ theme |> ThemeColor.textLight |> Font.color, Font.size 16 ] (text "Guide")
                , images
                    |> Image.video
                        [ width <| px 16, height <| px 16 ]
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
                model.theme
                    |> MaturityButton.disabled
                    |> map never
        ]
