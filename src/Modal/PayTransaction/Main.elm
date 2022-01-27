port module Modal.PayTransaction.Main exposing (Effect(..), Modal, Msg, init, subscriptions, update, view)

import Blockchain.Main as Blockchain exposing (Blockchain)
import Blockchain.User.Due exposing (Due)
import Blockchain.User.Dues as Dues
import Blockchain.User.Main as User exposing (User)
import Blockchain.User.TokenId as TokenId exposing (TokenId)
import Blockchain.User.WritePay as WritePay exposing (WritePay)
import Data.Backdrop exposing (Backdrop)
import Data.Chain exposing (Chain)
import Data.ERC20 exposing (ERC20)
import Data.Images exposing (Images)
import Data.Or exposing (Or(..))
import Data.Pair as Pair
import Data.Pool exposing (Pool)
import Data.Remote as Remote exposing (Remote(..))
import Data.Support exposing (Support)
import Data.Theme exposing (Theme)
import Data.Token as Token
import Data.Uint as Uint exposing (Uint)
import Element
    exposing
        ( Element
        , alignRight
        , below
        , centerX
        , centerY
        , clipY
        , column
        , el
        , fill
        , fillPortion
        , height
        , inFront
        , maximum
        , minimum
        , mouseDown
        , mouseOver
        , moveDown
        , moveLeft
        , none
        , padding
        , paddingEach
        , paddingXY
        , px
        , row
        , scrollbarY
        , shrink
        , spacing
        , text
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Json.Decode as Decode
import Json.Encode exposing (Value)
import Modal.Outside as Outside
import Modal.PayTransaction.Error exposing (Error)
import Modal.PayTransaction.Query as Query
import Modal.PayTransaction.Tooltip as Tooltip exposing (Tooltip)
import Modal.PayTransaction.Total exposing (Total)
import Page.Transaction.MaxButton as MaxButton
import Page.Transaction.Textbox as Textbox
import Process
import Sort.Dict as Dict exposing (Dict)
import Sort.Set as Set exposing (Set)
import Task
import Utility.Color as Color
import Utility.Glass as Glass
import Utility.IconButton as IconButton
import Utility.Image as Image
import Utility.Maybe as Maybe
import Utility.ThemeColor as ThemeColor
import Utility.Truncate as Truncate


type Modal
    = Modal
        { pool : Pool
        , state : State
        , total : Remote Error Total
        , tooltip : Maybe Tooltip
        }


type State
    = Full (Set TokenId)
    | Custom (Dict TokenId CustomInfo)


type alias CustomInfo =
    { assetIn : String
    , collateralOut : Remote Error Uint
    }


type Mode
    = RepayFull
    | RepayCustom


type Msg
    = SwitchMode Mode
    | InputAssetIn TokenId String
    | InputMax TokenId
    | QuerySum ()
    | QueryProportion TokenId ()
    | ClickApprove
    | ClickPay
    | ReceiveSum Value
    | ReceiveProportion Value
    | OnMouseEnter Tooltip
    | OnMouseLeave
    | Exit


type Effect
    = Approve ERC20
    | Pay WritePay


init :
    Blockchain
    -> User
    -> Pool
    -> Set TokenId
    -> ( Modal, Cmd Msg )
init blockchain user pool set =
    ( { pool = pool
      , state = set |> Full
      , total = Remote.loading
      , tooltip = Nothing
      }
        |> Modal
    , user
        |> User.getDues
        |> Remote.map (Dues.getMultiple pool set)
        |> (Remote.map << Maybe.map) Dict.values
        |> (Remote.map << Maybe.map << List.map)
            (\{ debt, collateral } ->
                { assetIn = debt
                , collateralOut = collateral
                }
            )
        |> (Remote.map << Maybe.map)
            (\duesIn ->
                { chain = blockchain |> Blockchain.toChain
                , pool = pool
                , duesIn = duesIn
                }
                    |> Query.sum
                    |> querySum
            )
        |> (Remote.map << Maybe.withDefault) Cmd.none
        |> Remote.withDefault Cmd.none
    )


update :
    Blockchain
    -> User
    -> Msg
    -> Modal
    -> ( Maybe Modal, Cmd Msg, Maybe Effect )
update blockchain user msg (Modal modal) =
    case ( msg, modal.state ) of
        ( SwitchMode RepayFull, Custom dict ) ->
            dict
                |> Dict.keys
                |> Set.fromList TokenId.sorter
                |> (\set ->
                        ( { modal
                            | state = set |> Full
                            , total = Remote.loading
                          }
                            |> Modal
                            |> Just
                        , user
                            |> User.getDues
                            |> Remote.map (Dues.getMultiple modal.pool set)
                            |> (Remote.map << Maybe.map) Dict.values
                            |> (Remote.map << Maybe.map << List.map)
                                (\{ debt, collateral } ->
                                    { assetIn = debt
                                    , collateralOut = collateral
                                    }
                                )
                            |> (Remote.map << Maybe.map)
                                (\duesIn ->
                                    { chain = blockchain |> Blockchain.toChain
                                    , pool = modal.pool
                                    , duesIn = duesIn
                                    }
                                        |> Query.sum
                                        |> querySum
                                )
                            |> (Remote.map << Maybe.withDefault) Cmd.none
                            |> Remote.withDefault Cmd.none
                        , Nothing
                        )
                   )

        ( SwitchMode RepayCustom, Full set ) ->
            { modal
                | state =
                    user
                        |> User.getDues
                        |> Remote.map (Dues.getMultiple modal.pool set)
                        |> (Remote.map << Maybe.map << Dict.map)
                            (\_ { debt, collateral } ->
                                { assetIn =
                                    debt
                                        |> Uint.toAmount
                                            (modal.pool.pair |> Pair.toAsset)
                                , collateralOut = collateral |> Success
                                }
                            )
                        |> (Remote.map << Maybe.withDefault) (Dict.empty TokenId.sorter)
                        |> Remote.withDefault (Dict.empty TokenId.sorter)
                        |> Custom
            }
                |> noCmdAndEffect

        ( InputAssetIn tokenId assetIn, Custom dict ) ->
            if assetIn |> Uint.isAmount (modal.pool.pair |> Pair.toAsset) then
                ( { modal
                    | state =
                        dict
                            |> Dict.insert tokenId
                                { assetIn = assetIn
                                , collateralOut = Remote.loading
                                }
                            |> Dict.map
                                (\_ customInfo ->
                                    case customInfo.collateralOut of
                                        Failure _ ->
                                            { customInfo
                                                | collateralOut = Remote.loading
                                            }

                                        _ ->
                                            customInfo
                                )
                            |> Custom
                    , total = Remote.loading
                  }
                    |> Modal
                    |> Just
                , user
                    |> User.getDues
                    |> Remote.map (Dict.get modal.pool)
                    |> (Remote.map << Maybe.andThen) (Dict.get tokenId)
                    |> (Remote.map << Maybe.map)
                        (\due assetInUint ->
                            { chain = blockchain |> Blockchain.toChain
                            , pool = modal.pool
                            , tokenId = tokenId
                            , due = due
                            , assetIn = assetInUint
                            }
                                |> Query.proportion
                                |> queryProportion
                        )
                    |> (Remote.map << Maybe.apply)
                        (assetIn
                            |> Uint.fromAmount
                                (modal.pool.pair |> Pair.toAsset)
                        )
                    |> (Remote.map << Maybe.withDefault) Cmd.none
                    |> Remote.withDefault Cmd.none
                , Nothing
                )

            else
                modal |> noCmdAndEffect

        ( QuerySum (), Full set ) ->
            case modal.total of
                Failure _ ->
                    ( { modal
                        | total = Remote.loading
                      }
                        |> Modal
                        |> Just
                    , user
                        |> User.getDues
                        |> Remote.map (Dues.getMultiple modal.pool set)
                        |> (Remote.map << Maybe.map) Dict.values
                        |> (Remote.map << Maybe.map << List.map)
                            (\{ debt, collateral } ->
                                { assetIn = debt
                                , collateralOut = collateral
                                }
                            )
                        |> (Remote.map << Maybe.map)
                            (\duesIn ->
                                { chain = blockchain |> Blockchain.toChain
                                , pool = modal.pool
                                , duesIn = duesIn
                                }
                                    |> Query.sum
                                    |> querySum
                            )
                        |> (Remote.map << Maybe.withDefault) Cmd.none
                        |> Remote.withDefault Cmd.none
                    , Nothing
                    )

                _ ->
                    modal |> noCmdAndEffect

        ( QuerySum (), Custom dict ) ->
            case modal.total of
                Failure _ ->
                    ( { modal
                        | total = Remote.loading
                      }
                        |> Modal
                        |> Just
                    , dict
                        |> Dict.values
                        |> List.foldr
                            (\dueIn accumulator ->
                                accumulator
                                    |> Maybe.andThen
                                        (\list ->
                                            case
                                                ( dueIn.assetIn
                                                    |> Uint.fromAmount
                                                        (modal.pool.pair |> Pair.toAsset)
                                                , dueIn.collateralOut
                                                )
                                            of
                                                ( Just assetIn, Success collateralOut ) ->
                                                    { assetIn = assetIn
                                                    , collateralOut = collateralOut
                                                    }
                                                        :: list
                                                        |> Just

                                                _ ->
                                                    Nothing
                                        )
                            )
                            (Just [])
                        |> Maybe.map
                            (\duesIn ->
                                { chain = blockchain |> Blockchain.toChain
                                , pool = modal.pool
                                , duesIn = duesIn
                                }
                                    |> Query.sum
                                    |> querySum
                            )
                        |> Maybe.withDefault Cmd.none
                    , Nothing
                    )

                _ ->
                    modal |> noCmdAndEffect

        ( QueryProportion tokenId (), Custom dict ) ->
            case
                dict
                    |> Dict.get tokenId
            of
                Just customInfo ->
                    case customInfo.collateralOut of
                        Failure _ ->
                            ( { modal
                                | state =
                                    dict
                                        |> Dict.insert tokenId
                                            { customInfo
                                                | collateralOut =
                                                    Remote.loading
                                            }
                                        |> Custom
                              }
                                |> Modal
                                |> Just
                            , user
                                |> User.getDues
                                |> Remote.map (Dict.get modal.pool)
                                |> (Remote.map << Maybe.andThen) (Dict.get tokenId)
                                |> (Remote.map << Maybe.map)
                                    (\due assetInUint ->
                                        { chain = blockchain |> Blockchain.toChain
                                        , pool = modal.pool
                                        , tokenId = tokenId
                                        , due = due
                                        , assetIn = assetInUint
                                        }
                                            |> Query.proportion
                                            |> queryProportion
                                    )
                                |> (Remote.map << Maybe.apply)
                                    (customInfo.assetIn
                                        |> Uint.fromAmount
                                            (modal.pool.pair |> Pair.toAsset)
                                    )
                                |> (Remote.map << Maybe.withDefault) Cmd.none
                                |> Remote.withDefault Cmd.none
                            , Nothing
                            )

                        _ ->
                            modal |> noCmdAndEffect

                _ ->
                    modal |> noCmdAndEffect

        ( ClickApprove, _ ) ->
            (case
                ( modal.total
                , modal.pool.pair
                    |> Pair.toAsset
                    |> Token.toERC20
                )
             of
                ( Success total, Just erc20 ) ->
                    if
                        (user
                            |> User.hasEnoughBalance
                                (modal.pool.pair |> Pair.toAsset)
                                total.assetIn
                        )
                            && (user
                                    |> User.hasEnoughAllowance
                                        erc20
                                        total.assetIn
                                    |> not
                               )
                    then
                        ( modal
                            |> Modal
                            |> Just
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
                |> Maybe.withDefault (modal |> noCmdAndEffect)

        ( ClickPay, Full set ) ->
            (case modal.total of
                Success total ->
                    if
                        (user
                            |> User.hasEnoughBalance
                                (modal.pool.pair |> Pair.toAsset)
                                total.assetIn
                        )
                            && (modal.pool.pair
                                    |> Pair.toAsset
                                    |> Token.toERC20
                                    |> Maybe.map
                                        (\erc20 ->
                                            user
                                                |> User.hasEnoughAllowance
                                                    erc20
                                                    total.assetIn
                                        )
                                    |> Maybe.withDefault True
                               )
                    then
                        ( modal
                            |> Modal
                            |> Just
                        , Cmd.none
                        , user
                            |> User.getDues
                            |> Remote.map (Dues.getMultiple modal.pool set)
                            |> (Remote.map << Maybe.map << Dict.map)
                                (\_ { debt } -> debt)
                            |> (Remote.map << Maybe.andThen)
                                (\assetsIn ->
                                    { pool = modal.pool
                                    , assetsIn = assetsIn
                                    }
                                        |> Pay
                                        |> Just
                                )
                            |> Remote.withDefault Nothing
                        )
                            |> Just

                    else
                        Nothing

                _ ->
                    Nothing
            )
                |> Maybe.withDefault (modal |> noCmdAndEffect)

        ( ClickPay, Custom dict ) ->
            (case modal.total of
                Success total ->
                    if
                        (user
                            |> User.hasEnoughBalance
                                (modal.pool.pair |> Pair.toAsset)
                                total.assetIn
                        )
                            && (modal.pool.pair
                                    |> Pair.toAsset
                                    |> Token.toERC20
                                    |> Maybe.map
                                        (\erc20 ->
                                            user
                                                |> User.hasEnoughAllowance
                                                    erc20
                                                    total.assetIn
                                        )
                                    |> Maybe.withDefault True
                               )
                    then
                        ( modal
                            |> Modal
                            |> Just
                        , Cmd.none
                        , dict
                            |> Dict.foldr
                                (\tokenId dueIn accumulator ->
                                    accumulator
                                        |> Maybe.andThen
                                            (\accumulatedDict ->
                                                case
                                                    ( dueIn.assetIn
                                                        |> Uint.fromAmount
                                                            (modal.pool.pair |> Pair.toAsset)
                                                    , dueIn.collateralOut
                                                    )
                                                of
                                                    ( Just assetIn, Success _ ) ->
                                                        accumulatedDict
                                                            |> Dict.insert tokenId assetIn
                                                            |> Just

                                                    _ ->
                                                        Nothing
                                            )
                                )
                                (Dict.empty TokenId.sorter |> Just)
                            |> Maybe.map
                                (\assetsIn ->
                                    { pool = modal.pool
                                    , assetsIn = assetsIn
                                    }
                                        |> Pay
                                )
                        )
                            |> Just

                    else
                        Nothing

                _ ->
                    Nothing
            )
                |> Maybe.withDefault (modal |> noCmdAndEffect)

        ( ReceiveSum value, Full set ) ->
            case value |> Decode.decodeValue Query.decoderSum of
                Ok answer ->
                    if
                        (answer.chain == (blockchain |> Blockchain.toChain))
                            && (answer.pool == modal.pool)
                            && (user
                                    |> User.getDues
                                    |> Remote.map (Dues.getMultiple modal.pool set)
                                    |> (Remote.map << Maybe.map) Dict.values
                                    |> (Remote.map << Maybe.map << List.map)
                                        (\{ debt, collateral } ->
                                            { assetIn = debt
                                            , collateralOut = collateral
                                            }
                                        )
                                    |> (Remote.map << Maybe.map) ((==) answer.duesIn)
                                    |> (Remote.map << Maybe.withDefault) False
                                    |> Remote.withDefault False
                               )
                    then
                        ( { modal
                            | total =
                                answer.result
                                    |> toRemote
                          }
                            |> Modal
                            |> Just
                        , answer.result
                            |> Result.map (\_ -> Cmd.none)
                            |> Result.withDefault
                                (Process.sleep 5000
                                    |> Task.perform QuerySum
                                )
                        , Nothing
                        )

                    else
                        modal |> noCmdAndEffect

                _ ->
                    modal |> noCmdAndEffect

        ( ReceiveSum value, Custom dict ) ->
            case value |> Decode.decodeValue Query.decoderSum of
                Ok answer ->
                    if
                        (answer.chain == (blockchain |> Blockchain.toChain))
                            && (answer.pool == modal.pool)
                            && (dict
                                    |> Dict.values
                                    |> List.foldr
                                        (\dueIn accumulator ->
                                            accumulator
                                                |> Maybe.andThen
                                                    (\list ->
                                                        case
                                                            ( dueIn.assetIn
                                                                |> Uint.fromAmount
                                                                    (modal.pool.pair |> Pair.toAsset)
                                                            , dueIn.collateralOut
                                                            )
                                                        of
                                                            ( Just assetIn, Success collateralOut ) ->
                                                                { assetIn = assetIn
                                                                , collateralOut = collateralOut
                                                                }
                                                                    :: list
                                                                    |> Just

                                                            _ ->
                                                                Nothing
                                                    )
                                        )
                                        (Just [])
                                    |> Maybe.map ((==) answer.duesIn)
                                    |> Maybe.withDefault False
                               )
                    then
                        ( { modal
                            | total =
                                answer.result
                                    |> toRemote
                          }
                            |> Modal
                            |> Just
                        , answer.result
                            |> Result.map (\_ -> Cmd.none)
                            |> Result.withDefault
                                (Process.sleep 5000
                                    |> Task.perform QuerySum
                                )
                        , Nothing
                        )

                    else
                        modal |> noCmdAndEffect

                _ ->
                    modal |> noCmdAndEffect

        ( ReceiveProportion value, Custom dict ) ->
            case value |> Decode.decodeValue Query.decoderProportion of
                Ok answer ->
                    if
                        (answer.chain == (blockchain |> Blockchain.toChain))
                            && (answer.pool == modal.pool)
                            && (user
                                    |> User.getDues
                                    |> Remote.map (Dict.get modal.pool)
                                    |> (Remote.map << Maybe.andThen)
                                        (Dict.get answer.tokenId)
                                    |> (Remote.map << Maybe.map) ((==) answer.due)
                                    |> (Remote.map << Maybe.withDefault) False
                                    |> Remote.withDefault False
                               )
                            && (dict
                                    |> Dict.get answer.tokenId
                                    |> Maybe.map
                                        (\{ assetIn } ->
                                            (answer.assetIn
                                                |> Uint.toAmount
                                                    (modal.pool.pair |> Pair.toAsset)
                                            )
                                                == assetIn
                                        )
                                    |> Maybe.withDefault False
                               )
                    then
                        ( { modal
                            | state =
                                dict
                                    |> Dict.get answer.tokenId
                                    |> Maybe.map
                                        (\customInfo ->
                                            dict
                                                |> Dict.insert answer.tokenId
                                                    { customInfo
                                                        | collateralOut =
                                                            answer.result
                                                                |> toRemote
                                                    }
                                        )
                                    |> Maybe.withDefault dict
                                    |> Custom
                          }
                            |> Modal
                            |> Just
                        , answer.result
                            |> Result.map
                                (\_ ->
                                    dict
                                        |> Dict.values
                                        |> List.foldr
                                            (\dueIn accumulator ->
                                                accumulator
                                                    |> Maybe.andThen
                                                        (\list ->
                                                            case
                                                                ( dueIn.assetIn
                                                                    |> Uint.fromAmount
                                                                        (modal.pool.pair |> Pair.toAsset)
                                                                , dueIn.collateralOut
                                                                )
                                                            of
                                                                ( Just assetIn, Success collateralOut ) ->
                                                                    { assetIn = assetIn
                                                                    , collateralOut = collateralOut
                                                                    }
                                                                        :: list
                                                                        |> Just

                                                                _ ->
                                                                    Nothing
                                                        )
                                            )
                                            (Just [])
                                        |> Maybe.map
                                            (\duesIn ->
                                                { chain = blockchain |> Blockchain.toChain
                                                , pool = modal.pool
                                                , duesIn = duesIn
                                                }
                                                    |> Query.sum
                                                    |> querySum
                                            )
                                        |> Maybe.withDefault Cmd.none
                                )
                            |> Result.withDefault
                                (Process.sleep 5000
                                    |> Task.perform (QueryProportion answer.tokenId)
                                )
                        , Nothing
                        )

                    else
                        modal |> noCmdAndEffect

                _ ->
                    modal |> noCmdAndEffect

        ( OnMouseEnter tooltip, _ ) ->
            { modal | tooltip = Just tooltip }
                |> noCmdAndEffect

        ( OnMouseLeave, _ ) ->
            { modal | tooltip = Nothing }
                |> noCmdAndEffect

        ( Exit, _ ) ->
            ( Nothing
            , Cmd.none
            , Nothing
            )

        _ ->
            modal |> noCmdAndEffect


toRemote :
    Result Error answer
    -> Remote Error answer
toRemote result =
    case result of
        Ok claims ->
            Success claims

        Err error ->
            Failure error


noCmdAndEffect :
    { pool : Pool
    , state : State
    , total : Remote Error Total
    , tooltip : Maybe Tooltip
    }
    -> ( Maybe Modal, Cmd Msg, Maybe Effect )
noCmdAndEffect modal =
    ( modal
        |> Modal
        |> Just
    , Cmd.none
    , Nothing
    )


port queryProportion : Value -> Cmd msg


port querySum : Value -> Cmd msg


port receiveProportion : (Value -> msg) -> Sub msg


port receiveSum : (Value -> msg) -> Sub msg


subscriptions : Sub Msg
subscriptions =
    [ receiveProportion ReceiveProportion
    , receiveSum ReceiveSum
    ]
        |> Sub.batch


view :
    { model
        | backdrop : Backdrop
        , images : Images
        , theme : Theme
    }
    -> Blockchain
    -> Modal
    -> Element Msg
view ({ backdrop, images, theme } as model) blockchain modal =
    Outside.view model
        { onClick = Exit
        , modal =
            column
                ([ width <| minimum 375 fill
                 , height shrink
                 , spacing 16
                 , centerX
                 , centerY
                 , Border.rounded 8
                 , Border.color Color.transparent100
                 , Border.width 1
                 ]
                    ++ Glass.background backdrop theme
                )
                [ column
                    [ width fill
                    ]
                    [ header model
                    , body model modal blockchain
                    ]
                ]
        }


header :
    { model
        | theme : Theme
        , images : Images
    }
    -> Element Msg
header model =
    row
        [ width fill
        , height shrink
        , paddingEach
            { top = 24
            , right = 24
            , bottom = 20
            , left = 24
            }
        , Border.widthEach
            { top = 0
            , right = 0
            , bottom = 1
            , left = 0
            }
        , model.theme |> ThemeColor.border |> Border.color
        ]
        [ el
            [ width shrink
            , height shrink
            , centerY
            , Font.size 18
            , Font.bold
            , paddingXY 0 3
            , model.theme |> ThemeColor.text |> Font.color
            ]
            (text "Repay debt")
        , IconButton.exit model Exit
        ]


body :
    { model
        | theme : Theme
        , images : Images
    }
    -> Modal
    -> Blockchain
    -> Element Msg
body model ((Modal { state }) as modal) blockchain =
    column
        [ width fill
        , padding 20
        , spacing 16
        ]
        [ row
            [ width fill
            , height <| px 44
            , spacing 16
            , padding 4
            , model.theme |> ThemeColor.border |> Background.color
            , Border.rounded 8
            ]
            [ switchRepayFull model modal
            , switchRepayCustom model modal
            ]
        , repayList model modal blockchain
        , confirmBtn model modal
        ]


switchRepayFull :
    { model
        | theme : Theme
    }
    -> Modal
    -> Element Msg
switchRepayFull { theme } (Modal modal) =
    Input.button
        ([ width fill
         , height <| px 36
         , paddingXY 12 8
         ]
            ++ (case modal.state of
                    Full _ ->
                        [ theme |> ThemeColor.primaryBtn |> Background.color
                        , Border.rounded 4
                        ]

                    _ ->
                        []
               )
        )
        { onPress =
            case modal.state of
                Full _ ->
                    Nothing

                Custom _ ->
                    Just (SwitchMode RepayFull)
        , label =
            el
                [ centerX
                , centerY
                , (case modal.state of
                    Full _ ->
                        Color.light100

                    Custom _ ->
                        theme |> ThemeColor.textLight
                  )
                    |> Font.color
                , Font.size 16
                , Font.bold
                ]
                (text "Repay Full")
        }


switchRepayCustom :
    { model
        | theme : Theme
    }
    -> Modal
    -> Element Msg
switchRepayCustom { theme } (Modal modal) =
    Input.button
        ([ width fill
         , height <| px 36
         , paddingXY 12 8
         ]
            ++ (case modal.state of
                    Custom _ ->
                        [ theme |> ThemeColor.primaryBtn |> Background.color
                        , Border.rounded 4
                        ]

                    _ ->
                        []
               )
        )
        { onPress =
            case modal.state of
                Full _ ->
                    Just (SwitchMode RepayCustom)

                Custom _ ->
                    Nothing
        , label =
            el
                [ centerX
                , centerY
                , (case modal.state of
                    Full _ ->
                        theme |> ThemeColor.textLight

                    Custom _ ->
                        Color.light100
                  )
                    |> Font.color
                , Font.size 16
                , Font.bold
                ]
                (text "Repay Custom")
        }


repayList :
    { model
        | theme : Theme
        , images : Images
    }
    -> Modal
    -> Blockchain
    -> Element Msg
repayList model ((Modal { state, pool }) as modal) blockchain =
    column
        [ width fill
        , spacing 12
        ]
        (case state of
            Full tokenIdSet ->
                -- blockchain
                --     |> Blockchain.toUser
                --     |> Maybe.map User.getDues
                --     |> (Maybe.map << Remote.map) (Dues.getMultiple pool tokenIdSet)
                --     |> (Maybe.map << Remote.map << Maybe.map) Dict.toList
                --     |> (Maybe.map << Remote.map << Maybe.map << List.map)
                --         (\( tokenId, due ) ->
                --             fullPosition model modal tokenId due
                --         )
                --     |> (Maybe.map << Remote.map << Maybe.withDefault) [ none ]
                --     |> (Maybe.map << Remote.withDefault) [ none ]
                --     |> Maybe.withDefault [ none ]
                User.getDuesDummy
                    |> Dues.getMultiple pool TokenId.dummy
                    |> Maybe.map Dict.toList
                    |> (Maybe.map << List.map)
                        (\( tokenId, due ) ->
                            fullPosition model modal tokenId due
                        )
                    |> Maybe.withDefault [ none ]

            Custom dict ->
                -- blockchain
                --     |> Blockchain.toUser
                --     |> Maybe.map
                --         (\user ->
                --             user
                --                 |> User.getDues
                --                 |> Remote.map (Dues.getMultiple pool (dict |> Dict.keys |> Set.fromList TokenId.sorter))
                --                 |> (Remote.map << Maybe.map) Dict.toList
                --                 |> (Remote.map << Maybe.map << List.map)
                --                     (\( tokenId, due ) ->
                --                         customPosition model modal user tokenId due dict
                --                     )
                --                 |> (Remote.map << Maybe.withDefault) [ none ]
                --                 |> Remote.withDefault [ none ]
                --         )
                --     |> Maybe.withDefault [ none ]
                blockchain
                    |> Blockchain.toUser
                    |> Maybe.map
                        (\user ->
                            User.getDuesDummy
                                |> Dues.getMultiple pool TokenId.dummy
                                |> Maybe.map Dict.toList
                                |> (Maybe.map << List.map)
                                    (\( tokenId, due ) ->
                                        customPosition model modal user tokenId due dict
                                    )
                                |> Maybe.withDefault [ none ]
                        )
                    |> Maybe.withDefault [ none ]
        )


fullPosition :
    { model
        | theme : Theme
        , images : Images
    }
    -> Modal
    -> TokenId
    -> Due
    -> Element Msg
fullPosition { theme, images } ((Modal { pool, tooltip }) as modal) tokenId due =
    column
        [ width fill
        , paddingXY 16 12
        , spacing 12
        , theme |> ThemeColor.sectionBackground |> Background.color
        , Border.rounded 8
        ]
        [ row [ width fill, spacing 6, centerY ]
            [ el
                [ Font.size 14
                , theme |> ThemeColor.textLight |> Font.color
                ]
                (text "Position ID")
            , el
                [ alignRight
                , theme |> ThemeColor.textLight |> Font.color
                , Font.size 14
                , Font.bold
                ]
                (text (tokenId |> TokenId.toString))
            , images
                |> Image.link
                    [ width <| px 16, height <| px 16 ]
            ]
        , row [ width fill, spacing 6, centerY ]
            [ el
                [ Font.size 14
                , theme |> ThemeColor.text |> Font.color
                ]
                (text "Amount to repay")
            , el
                [ Font.size 14
                , Font.bold
                , alignRight
                , theme |> ThemeColor.text |> Font.color
                ]
                (Truncate.viewAmount
                    { onMouseEnter = OnMouseEnter
                    , onMouseLeave = OnMouseLeave
                    , tooltip = Tooltip.Debt tokenId
                    , opened = tooltip
                    , token = pool.pair |> Pair.toAsset
                    , amount = due.debt
                    , theme = theme
                    }
                )
            , el
                [ alignRight
                , paddingXY 3 1
                , Border.rounded 4
                , theme |> ThemeColor.border |> Background.color
                ]
                (Truncate.viewSymbol
                    { onMouseEnter = OnMouseEnter
                    , onMouseLeave = OnMouseLeave
                    , tooltip = Tooltip.Debt tokenId
                    , opened = tooltip
                    , token = pool.pair |> Pair.toAsset
                    , theme = theme
                    }
                )
            ]
        , row [ width fill, spacing 6, centerY ]
            [ el
                [ Font.size 14
                , theme |> ThemeColor.text |> Font.color
                ]
                (text "Collateral to unlock")
            , el
                [ Font.size 14
                , Font.bold
                , alignRight
                , theme |> ThemeColor.text |> Font.color
                ]
                (Truncate.viewAmount
                    { onMouseEnter = OnMouseEnter
                    , onMouseLeave = OnMouseLeave
                    , tooltip = Tooltip.Collateral tokenId
                    , opened = tooltip
                    , token = pool.pair |> Pair.toCollateral
                    , amount = due.collateral
                    , theme = theme
                    }
                )
            , el
                [ alignRight
                , paddingXY 3 1
                , Border.rounded 4
                , theme |> ThemeColor.border |> Background.color
                ]
                (Truncate.viewSymbol
                    { onMouseEnter = OnMouseEnter
                    , onMouseLeave = OnMouseLeave
                    , tooltip = Tooltip.Collateral tokenId
                    , opened = tooltip
                    , token = pool.pair |> Pair.toCollateral
                    , theme = theme
                    }
                )
            ]
        ]


customPosition :
    { model
        | theme : Theme
        , images : Images
    }
    -> Modal
    -> User
    -> TokenId
    -> Due
    -> Dict TokenId CustomInfo
    -> Element Msg
customPosition ({ theme, images } as model) ((Modal { pool, state, tooltip }) as modal) user tokenId due customInfoDict =
    column
        [ width fill
        , paddingXY 16 12
        , spacing 12
        , theme |> ThemeColor.sectionBackground |> Background.color
        , Border.rounded 8
        ]
        [ row [ width fill, spacing 6, centerY ]
            [ el
                [ Font.size 14
                , theme |> ThemeColor.textLight |> Font.color
                ]
                (text "Position ID")
            , el
                [ alignRight
                , theme |> ThemeColor.textLight |> Font.color
                , Font.size 14
                , Font.bold
                ]
                (text (tokenId |> TokenId.toString))
            , images
                |> Image.link
                    [ width <| px 16, height <| px 16 ]
            ]
        , row [ width fill, spacing 6, centerY ]
            [ el
                [ Font.size 14
                , theme |> ThemeColor.textLight |> Font.color
                ]
                (text "Total debt to pay")
            , el
                [ Font.size 14
                , Font.bold
                , alignRight
                , theme |> ThemeColor.textLight |> Font.color
                ]
                (Truncate.viewAmount
                    { onMouseEnter = OnMouseEnter
                    , onMouseLeave = OnMouseLeave
                    , tooltip = Tooltip.Debt tokenId
                    , opened = tooltip
                    , token = pool.pair |> Pair.toAsset
                    , amount = due.debt
                    , theme = theme
                    }
                )
            , el
                [ alignRight
                , paddingXY 3 1
                , Border.rounded 4
                , theme |> ThemeColor.border |> Background.color
                ]
                (Truncate.viewSymbol
                    { onMouseEnter = OnMouseEnter
                    , onMouseLeave = OnMouseLeave
                    , tooltip = Tooltip.Debt tokenId
                    , opened = tooltip
                    , token = pool.pair |> Pair.toAsset
                    , theme = theme
                    }
                )
            ]
        , row
            [ width fill
            , spacing 8
            , centerY
            ]
            [ el
                [ Font.size 14
                , theme |> ThemeColor.actionElemLabel |> Font.color
                ]
                (text "Enter amount to repay")
            , user
                |> User.getBalance (pool.pair |> Pair.toAsset)
                |> Maybe.map
                    (\balance ->
                        MaxButton.view
                            { onPress = InputMax tokenId
                            , onMouseEnter = OnMouseEnter
                            , onMouseLeave = OnMouseLeave
                            , tooltip = Tooltip.Balance tokenId
                            , opened = tooltip
                            , token = pool.pair |> Pair.toAsset
                            , balance = balance
                            , theme = model.theme
                            }
                    )
                |> Maybe.withDefault none
            ]
        , Textbox.view model
            { onMouseEnter = OnMouseEnter
            , onMouseLeave = OnMouseLeave
            , tooltip = Tooltip.TextboxToken tokenId
            , opened = tooltip
            , token = pool.pair |> Pair.toAsset
            , onClick = Nothing
            , onChange = InputAssetIn tokenId
            , text =
                Dict.get tokenId customInfoDict
                    |> Maybe.map (\customInfo -> customInfo.assetIn |> Left)
                    |> Maybe.withDefault ("0" |> Left)
            , description = "asset in textbox"
            }
        , row [ width fill, spacing 6, centerY ]
            [ el
                [ Font.size 14
                , theme |> ThemeColor.text |> Font.color
                ]
                (text "Collateral to unlock")
            , el
                [ Font.size 14
                , Font.bold
                , alignRight
                , theme |> ThemeColor.text |> Font.color
                ]
                (Truncate.viewAmount
                    { onMouseEnter = OnMouseEnter
                    , onMouseLeave = OnMouseLeave
                    , tooltip = Tooltip.Collateral tokenId
                    , opened = tooltip
                    , token = pool.pair |> Pair.toCollateral
                    , amount = due.collateral
                    , theme = theme
                    }
                )
            , el
                [ alignRight
                , paddingXY 3 1
                , Border.rounded 4
                , theme |> ThemeColor.border |> Background.color
                ]
                (Truncate.viewSymbol
                    { onMouseEnter = OnMouseEnter
                    , onMouseLeave = OnMouseLeave
                    , tooltip = Tooltip.Collateral tokenId
                    , opened = tooltip
                    , token = pool.pair |> Pair.toCollateral
                    , theme = theme
                    }
                )
            ]
        ]


confirmBtn :
    { model
        | theme : Theme
        , images : Images
    }
    -> Modal
    -> Element Msg
confirmBtn { theme, images } ((Modal { state, pool }) as modal) =
    Input.button
        [ width fill
        , centerY
        ]
        { onPress = Just ClickPay
        , label =
            el
                [ width fill
                , height <| px 44
                , centerX
                , centerY
                , paddingXY 12 14
                , Border.rounded 4
                , Font.size 16
                , Font.bold
                , Font.color Color.light100
                , Font.center
                , theme |> ThemeColor.primaryBtn |> Background.color
                ]
                (text "Confirm Repay")
        }
