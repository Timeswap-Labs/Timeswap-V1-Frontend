port module Page.Position.Liq.Main exposing
    ( Effect(..)
    , Msg
    , Position
    , init
    , subscriptions
    , update
    , view
    )

import Blockchain.Main as Blockchain exposing (Blockchain)
import Blockchain.User.Due exposing (Due)
import Blockchain.User.Liq exposing (Liq)
import Blockchain.User.Main as User exposing (User)
import Blockchain.User.Return exposing (Return)
import Blockchain.User.TokenId exposing (TokenId)
import Blockchain.User.WriteBurn exposing (WriteBurn)
import Blockchain.User.WriteFlashRepay exposing (WriteFlashRepay)
import Data.Address exposing (Address)
import Data.Backdrop exposing (Backdrop)
import Data.Chain exposing (Chain)
import Data.ChosenZone exposing (ChosenZone)
import Data.Device exposing (Device(..))
import Data.Images exposing (Images)
import Data.Maturity as Maturity
import Data.Offset exposing (Offset)
import Data.Or exposing (Or(..))
import Data.Pair as Pair
import Data.Pool exposing (Pool)
import Data.Remote as Remote exposing (Remote(..))
import Data.Theme as Theme exposing (Theme)
import Data.TokenParam as TokenParam
import Data.Uint as Uint
import Data.Web exposing (Web)
import Element
    exposing
        ( Element
        , alignRight
        , below
        , centerX
        , centerY
        , column
        , el
        , fill
        , height
        , none
        , padding
        , paddingXY
        , paragraph
        , px
        , row
        , shrink
        , spacing
        , text
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Http
import Json.Decode as Decode
import Json.Encode exposing (Value)
import Page.Answer as PoolInfoAnswer
import Page.PoolInfo exposing (PoolInfo)
import Page.Position.Liq.Error exposing (Error)
import Page.Position.Liq.Query as Query exposing (ActiveReturn)
import Page.Position.Liq.Tooltip as Tooltip exposing (Tooltip)
import Page.Query as PoolInfoQuery
import Page.Transaction.Button as Button
import Process
import Sort.Dict as Dict
import Task
import Time exposing (Posix)
import Utility.Class as Class
import Utility.Color as Color
import Utility.Duration as Duration
import Utility.Glass as Glass
import Utility.Image as Image
import Utility.Loading as Loading
import Utility.Maybe as Maybe
import Utility.PairImage as PairImage
import Utility.ThemeColor as ThemeColor
import Utility.Tooltip as TooltipUtil
import Utility.Truncate as Truncate


type Position
    = Position
        { pool : Pool
        , return : Web ( PoolInfo, Status )
        , tooltip : Maybe Tooltip
        }


type alias Status =
    Maturity.Status (Remote Error Return) (Remote Error ActiveReturn)


type Msg
    = ClickAddMore
    | ClickBurn
    | ClickApproveAndFlashRepay Address
    | ClickFlashRepay Address
    | ClickReturn
    | Tick Posix
    | PoolInfoQueryAgain
    | QueryAgain Posix
    | ReceivePoolInfo Chain Pool (Result Http.Error PoolInfoAnswer.Answer)
    | ReceiveLiqReturn Value
    | OnMouseEnter Tooltip
    | OnMouseLeave


type Effect
    = InputPool Pool
    | Burn WriteBurn
    | ApproveAndFlashRepay WriteFlashRepay
    | FlashRepay WriteFlashRepay


init :
    { model | time : Posix, endPoint : String }
    -> Blockchain
    -> Pool
    -> ( Position, Cmd Msg )
init { endPoint } blockchain pool =
    ( { pool = pool
      , return = Remote.loading
      , tooltip = Nothing
      }
        |> Position
    , get blockchain endPoint pool
    )


update :
    { model | time : Posix, endPoint : String }
    -> Blockchain
    -> User
    -> Msg
    -> Position
    -> ( Maybe Position, Cmd Msg, Maybe Effect )
update { time, endPoint } blockchain user msg (Position position) =
    case msg of
        ClickAddMore ->
            ( Nothing
            , Cmd.none
            , InputPool position.pool |> Just
            )

        ClickBurn ->
            ( Position position |> Just
            , Cmd.none
            , if
                position.pool.maturity
                    |> Maturity.isActive time
              then
                Nothing

              else
                user
                    |> User.getLiqs
                    |> Remote.map (Dict.get position.pool)
                    |> (Remote.map << Maybe.andThen)
                        (\liquidityIn ->
                            { pool = position.pool
                            , liquidityIn = liquidityIn
                            }
                                |> Burn
                                |> Just
                        )
                    |> Remote.withDefault Nothing
            )

        ClickApproveAndFlashRepay addr ->
            ( Position position |> Just
            , Cmd.none
            , user
                |> User.getDues
                |> Remote.map (Dict.get position.pool)
                |> (Remote.map << Maybe.andThen)
                    (\dict ->
                        { pool = position.pool
                        , cdtAddress = addr
                        , tokenIds = dict |> Dict.keys
                        }
                            |> ApproveAndFlashRepay
                            |> Just
                    )
                |> Remote.withDefault Nothing
            )

        ClickFlashRepay addr ->
            ( Position position |> Just
            , Cmd.none
            , user
                |> User.getDues
                |> Remote.map (Dict.get position.pool)
                |> (Remote.map << Maybe.andThen)
                    (\dict ->
                        { pool = position.pool
                        , cdtAddress = addr
                        , tokenIds = dict |> Dict.keys
                        }
                            |> FlashRepay
                            |> Just
                    )
                |> Remote.withDefault Nothing
            )

        ClickReturn ->
            ( Nothing
            , Cmd.none
            , Nothing
            )

        Tick posix ->
            ( { position
                | return =
                    case position.return of
                        Success ( poolInfo, status ) ->
                            case status of
                                Maturity.Active remote ->
                                    ( poolInfo
                                    , remote
                                        |> Remote.update posix
                                        |> Maturity.Active
                                    )
                                        |> Success

                                Maturity.Matured remote ->
                                    ( poolInfo
                                    , remote
                                        |> Remote.update posix
                                        |> Maturity.Matured
                                    )
                                        |> Success

                        _ ->
                            position.return
                                |> Remote.update posix
              }
                |> Position
                |> Just
            , Cmd.none
            , Nothing
            )

        PoolInfoQueryAgain ->
            ( position
                |> Position
                |> Just
            , get blockchain endPoint position.pool
            , Nothing
            )

        QueryAgain posix ->
            ( { position
                | return =
                    case
                        ( position.pool.maturity
                            |> Maturity.isActive posix
                        , position.return
                        )
                    of
                        ( False, Success ( poolInfo, Maturity.Active _ ) ) ->
                            ( poolInfo
                            , Maturity.Matured Remote.loading
                            )
                                |> Success

                        _ ->
                            position.return
              }
                |> Position
                |> Just
            , case position.return of
                Success ( poolInfo, _ ) ->
                    user
                        |> User.getLiqs
                        |> Remote.map (Dict.get position.pool)
                        |> (Remote.map << Maybe.map)
                            (\liquidityIn ->
                                user
                                    |> User.getDues
                                    |> Remote.map (Dict.get position.pool)
                                    |> (Remote.map << Maybe.map)
                                        (\dict ->
                                            dict
                                                |> Dict.keys
                                                |> query blockchain
                                                    poolInfo
                                                    liquidityIn
                                                    position
                                                    (dict
                                                        |> Dict.toList
                                                        |> List.head
                                                        |> Maybe.map (\( _, due ) -> due.collateralizedDebt)
                                                        |> Maybe.withDefault Nothing
                                                    )
                                        )
                                    |> (Remote.map << Maybe.withDefault) (query blockchain poolInfo liquidityIn position Nothing [])
                                    |> Remote.withDefault (query blockchain poolInfo liquidityIn position Nothing [])
                            )
                        |> (Remote.map << Maybe.withDefault) Cmd.none
                        |> Remote.withDefault Cmd.none

                _ ->
                    Cmd.none
            , Nothing
            )

        ReceivePoolInfo chain pool result ->
            if
                (chain == (blockchain |> Blockchain.toChain))
                    && (pool == position.pool)
            then
                case result of
                    Ok (Right poolInfo) ->
                        ( { position
                            | return =
                                case
                                    ( position.pool.maturity
                                        |> Maturity.isActive time
                                    , position.return
                                    )
                                of
                                    ( True, Success ( _, (Maturity.Active _) as status ) ) ->
                                        ( poolInfo
                                        , status
                                        )
                                            |> Success

                                    ( False, Success ( _, (Maturity.Matured _) as status ) ) ->
                                        ( poolInfo
                                        , status
                                        )
                                            |> Success

                                    ( True, _ ) ->
                                        ( poolInfo
                                        , Maturity.Active Remote.loading
                                        )
                                            |> Success

                                    ( False, _ ) ->
                                        ( poolInfo
                                        , Maturity.Matured Remote.loading
                                        )
                                            |> Success
                          }
                            |> Position
                            |> Just
                        , Process.sleep 5000
                            |> Task.perform (\_ -> PoolInfoQueryAgain)
                        , Nothing
                        )

                    Ok (Left _) ->
                        ( position
                            |> Position
                            |> Just
                        , Process.sleep 5000
                            |> Task.perform (\_ -> PoolInfoQueryAgain)
                        , Nothing
                        )

                    Err error ->
                        ( { position | return = error |> Failure }
                            |> Position
                            |> Just
                        , Process.sleep 5000
                            |> Task.perform (\_ -> PoolInfoQueryAgain)
                        , Nothing
                        )

            else
                ( position
                    |> Position
                    |> Just
                , Process.sleep 5000
                    |> Task.perform (\_ -> PoolInfoQueryAgain)
                , Nothing
                )

        ReceiveLiqReturn value ->
            case
                ( value |> Decode.decodeValue Query.decoder
                , position.return
                )
            of
                ( Ok answer, Success ( poolInfo, status ) ) ->
                    if
                        (answer.chain == (blockchain |> Blockchain.toChain))
                            && (answer.pool == position.pool)
                            && (answer.poolInfo == poolInfo)
                    then
                        case answer.result of
                            Ok (Maturity.Active activeReturn) ->
                                ( { position
                                    | return =
                                        ( poolInfo
                                        , Success activeReturn
                                            |> Maturity.Active
                                        )
                                            |> Success
                                  }
                                    |> Position
                                    |> Just
                                , Cmd.none
                                , Nothing
                                )

                            Ok (Maturity.Matured return) ->
                                if (return.asset |> Uint.isZero) && (return.collateral |> Uint.isZero) then
                                    ( Nothing
                                    , Cmd.none
                                    , Nothing
                                    )

                                else
                                    ( { position
                                        | return =
                                            ( poolInfo
                                            , Success return
                                                |> Maturity.Matured
                                            )
                                                |> Success
                                      }
                                        |> Position
                                        |> Just
                                    , Cmd.none
                                    , Nothing
                                    )

                            Err error ->
                                ( { position
                                    | return =
                                        ( poolInfo
                                        , case status of
                                            Maturity.Active _ ->
                                                Failure error
                                                    |> Maturity.Active

                                            Maturity.Matured _ ->
                                                Failure error
                                                    |> Maturity.Matured
                                        )
                                            |> Success
                                  }
                                    |> Position
                                    |> Just
                                , Cmd.none
                                , Nothing
                                )

                    else
                        ( position
                            |> Position
                            |> Just
                        , Cmd.none
                        , Nothing
                        )

                _ ->
                    ( position
                        |> Position
                        |> Just
                    , Cmd.none
                    , Nothing
                    )

        OnMouseEnter tooltip ->
            ( { position | tooltip = Just tooltip }
                |> Position
                |> Just
            , Cmd.none
            , Nothing
            )

        OnMouseLeave ->
            ( { position | tooltip = Nothing }
                |> Position
                |> Just
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
                            |> PoolInfoQuery.toUrlString chain endPoint
                    , expect =
                        PoolInfoAnswer.decoder
                            |> Http.expectJson
                                (ReceivePoolInfo chain pool)
                    }
           )


query :
    Blockchain
    -> PoolInfo
    -> Liq
    ->
        { position
            | pool : Pool
        }
    -> Maybe Address
    -> List TokenId
    -> Cmd Msg
query blockchain poolInfo liquidityIn { pool } cdtAddress tokenIds =
    { chain = blockchain |> Blockchain.toChain
    , pool = pool
    , poolInfo = poolInfo
    , liquidityIn = liquidityIn
    , tokenIds = tokenIds
    , cdtAddress = cdtAddress
    }
        |> Query.givenLiq
        |> queryLiq


port queryLiq : Value -> Cmd msg


port receiveLiqReturn : (Value -> msg) -> Sub msg


subscriptions : Position -> Sub Msg
subscriptions (Position { return }) =
    [ return
        |> Remote.map Tuple.second
        |> Remote.map
            (\status ->
                case status of
                    Maturity.Active remote ->
                        remote |> Remote.subscriptions Tick

                    Maturity.Matured remote ->
                        remote |> Remote.subscriptions Tick
            )
        |> Remote.withDefault
            (return
                |> Remote.subscriptions Tick
            )
    , receiveLiqReturn ReceiveLiqReturn
    , Time.every 5000 QueryAgain
    ]
        |> Sub.batch


view :
    { model
        | time : Posix
        , offset : Offset
        , chosenZone : ChosenZone
        , device : Device
        , backdrop : Backdrop
        , theme : Theme
        , images : Images
    }
    -> Blockchain
    -> Position
    -> Element Msg
view ({ device, backdrop, theme } as model) blockchain (Position position) =
    column
        [ width shrink
        , height shrink
        , spacing 20
        ]
        [ returnButton model
        , column
            ([ Region.description "liquidity positions"
             , (case device of
                    Desktop ->
                        758

                    _ ->
                        375
               )
                |> px
                |> width
             , height shrink
             , (case device of
                    Desktop ->
                        24

                    _ ->
                        16
               )
                |> padding
             , spacing 30
             , Border.rounded 8
             , Border.width 1
             , theme |> ThemeColor.border |> Border.color
             ]
                ++ Glass.background backdrop theme
            )
            [ header model position
            , viewLiq model position
            , viewDues model blockchain position
            ]
        ]


returnButton : { model | images : Images, theme : Theme } -> Element Msg
returnButton { images, theme } =
    Input.button
        [ width shrink
        , height shrink
        ]
        { onPress = Just ClickReturn
        , label =
            row
                [ width shrink
                , height shrink
                , spacing 12
                ]
                [ images
                    |> (case theme of
                            Theme.Dark ->
                                Image.arrowLeft

                            Theme.Light ->
                                Image.arrowLeftDark
                       )
                        [ width <| px 16
                        , height <| px 16
                        , centerY
                        ]
                , el
                    [ width shrink
                    , height shrink
                    , Font.size 16
                    , Font.bold
                    , paddingXY 0 2
                    , theme |> ThemeColor.text |> Font.color
                    , centerY
                    ]
                    (text "Back to Liquidity")
                ]
        }


header :
    { model
        | time : Posix
        , offset : Offset
        , chosenZone : ChosenZone
        , theme : Theme
        , images : Images
    }
    -> { position | pool : Pool, tooltip : Maybe Tooltip }
    -> Element Msg
header { time, offset, chosenZone, theme, images } { pool, tooltip } =
    row
        [ width fill
        , height shrink
        , spacing 12
        ]
        [ el
            [ width shrink
            , height shrink
            , centerY
            ]
            (images
                |> PairImage.view
                    { pair = pool.pair
                    , length = 32
                    }
            )
        , el
            [ width shrink
            , height shrink
            , centerY
            ]
            (Truncate.viewPairSymbol
                { onMouseEnter = OnMouseEnter
                , onMouseLeave = OnMouseLeave
                , tooltip = Tooltip.Pair
                , opened = tooltip
                , pair = pool.pair
                , fontSize = 16
                , fontPadding = 2
                , theme = theme
                }
            )
        , el
            [ width shrink
            , height shrink
            , alignRight
            , centerY
            ]
            (Duration.viewMaturity
                { onMouseEnter = OnMouseEnter
                , onMouseLeave = OnMouseLeave
                , tooltip = Tooltip.Maturity
                , opened = tooltip
                , time = time
                , offset = offset
                , chosenZone = chosenZone
                , maturity = pool.maturity
                , theme = theme
                }
            )
        , if pool.maturity |> Maturity.isActive time then
            addMoreButton

          else
            addMoreDisabled
        , if pool.maturity |> Maturity.isActive time then
            burnDisabled theme

          else
            burnButton theme
        ]


addMoreButton : Element Msg
addMoreButton =
    Input.button
        [ width <| px 102
        , height <| px 44
        , Border.rounded 4
        , Border.width 1
        , Border.color Color.primary300
        ]
        { onPress = Just ClickAddMore
        , label =
            el
                [ width shrink
                , height shrink
                , centerX
                , centerY
                , Font.size 16
                , Font.color Color.light100
                , Font.bold
                ]
                (text "Add More")
        }


addMoreDisabled : Element msg
addMoreDisabled =
    el
        [ width <| px 102
        , height <| px 44
        , Border.rounded 4
        , Border.width 1
        , Border.color Color.transparent200
        ]
        (el
            [ width shrink
            , height shrink
            , centerX
            , centerY
            , Font.size 16
            , Font.color Color.transparent200
            , Font.bold
            ]
            (text "Add More")
        )


burnButton : Theme -> Element Msg
burnButton theme =
    Input.button
        [ width <| px 102
        , height <| px 44
        , Border.rounded 4
        , Class.is "shiningBtn"
        , theme |> ThemeColor.primaryBtn |> Background.color
        ]
        { onPress = Just ClickBurn
        , label =
            el
                [ width shrink
                , height shrink
                , centerX
                , centerY
                , Font.size 16
                , Font.color Color.light100
                , Font.bold
                ]
                (text "Burn")
        }


burnDisabled : Theme -> Element msg
burnDisabled theme =
    el
        [ width <| px 102
        , height <| px 44
        , Border.rounded 4
        , theme |> ThemeColor.btnBackground |> Background.color
        ]
        (el
            [ width shrink
            , height shrink
            , centerX
            , centerY
            , Font.size 16
            , theme |> ThemeColor.textLight |> Font.color
            , Font.bold
            ]
            (text "Burn")
        )


viewLiq :
    { model | images : Images, theme : Theme }
    ->
        { pool : Pool
        , return : Web ( PoolInfo, Status )
        , tooltip : Maybe Tooltip
        }
    -> Element Msg
viewLiq { images, theme } { pool, return, tooltip } =
    row
        [ width fill
        , height <| px 82
        , theme |> ThemeColor.positionBG |> Background.color
        , Border.rounded 8
        , paddingXY 24 16
        , spacing 48
        ]
        (case return of
            Success ( _, status ) ->
                case status of
                    Maturity.Active (Loading timeline) ->
                        [ column
                            [ width shrink
                            , height shrink
                            , spacing 8
                            ]
                            [ el
                                [ width shrink
                                , height shrink
                                , Font.size 14
                                , paddingXY 0 3
                                , theme |> ThemeColor.textLight |> Font.color
                                ]
                                (text "LP tokens owned (%)")
                            , el
                                [ width shrink
                                , height <| px 24
                                ]
                                (el
                                    [ width shrink
                                    , height shrink
                                    , centerY
                                    ]
                                    (Loading.view timeline theme)
                                )
                            ]
                        ]

                    Maturity.Active (Failure _) ->
                        [ column
                            [ width shrink
                            , height shrink
                            , spacing 8
                            ]
                            [ el
                                [ width shrink
                                , height shrink
                                , Font.size 14
                                , paddingXY 0 3
                                , theme |> ThemeColor.textLight |> Font.color
                                ]
                                (text "LP tokens owned (%)")
                            , row [ width shrink, height shrink, spacing 4 ]
                                [ images
                                    |> Image.error
                                        [ width <| px 24, height <| px 24 ]
                                , el [ Font.size 14, Font.color Color.negative400 ] (text "Error occured")
                                ]
                            ]
                        ]

                    Maturity.Active (Success activeReturn) ->
                        [ column
                            [ width shrink
                            , height shrink
                            , spacing 8
                            ]
                            [ el
                                [ width shrink
                                , height shrink
                                , Font.size 14
                                , paddingXY 0 3
                                , theme |> ThemeColor.textLight |> Font.color
                                ]
                                (text "LP tokens owned (%)")
                            , el
                                [ width shrink
                                , height shrink
                                , Font.size 18
                                , paddingXY 0 3
                                , Font.color Color.positive400
                                ]
                                (String.concat [ activeReturn.liqPercent |> String.fromFloat, "%" ] |> text)
                            ]
                        ]

                    Maturity.Matured (Loading timeline) ->
                        [ column
                            [ width shrink
                            , height shrink
                            , spacing 8
                            ]
                            [ el
                                [ width shrink
                                , height shrink
                                , Font.size 14
                                , paddingXY 0 3
                                , theme |> ThemeColor.textLight |> Font.color
                                ]
                                (text "Est. Asset at maturity")
                            , el
                                [ width shrink
                                , height <| px 24
                                ]
                                (el
                                    [ width shrink
                                    , height shrink
                                    , centerY
                                    ]
                                    (Loading.view timeline theme)
                                )
                            ]
                        , el
                            [ width <| px 1
                            , height fill
                            , theme |> ThemeColor.textDisabled |> Background.color
                            ]
                            none
                        , column
                            [ width shrink
                            , height shrink
                            , spacing 8
                            ]
                            [ el
                                [ width shrink
                                , height shrink
                                , Font.size 14
                                , paddingXY 0 3
                                , theme |> ThemeColor.textLight |> Font.color
                                ]
                                (text "Est. Collateral at maturity")
                            , el
                                [ width shrink
                                , height <| px 24
                                ]
                                (el
                                    [ width shrink
                                    , height shrink
                                    , centerY
                                    ]
                                    (Loading.view timeline theme)
                                )
                            ]
                        ]

                    Maturity.Matured (Failure _) ->
                        [ column
                            [ width shrink
                            , height shrink
                            , spacing 8
                            ]
                            [ el
                                [ width shrink
                                , height shrink
                                , Font.size 14
                                , paddingXY 0 3
                                , theme |> ThemeColor.textLight |> Font.color
                                ]
                                (text "Est. Asset at maturity")
                            , row [ width shrink, height shrink, spacing 4 ]
                                [ images |> Image.error [ width <| px 24, height <| px 24 ]
                                , el [ Font.size 14, Font.color Color.negative400 ] (text "Error occured")
                                ]
                            ]
                        ]

                    Maturity.Matured (Success liqReturn) ->
                        [ column
                            [ width shrink
                            , height shrink
                            , spacing 8
                            ]
                            [ el
                                [ width shrink
                                , height shrink
                                , Font.size 14
                                , paddingXY 0 3
                                , theme |> ThemeColor.textLight |> Font.color
                                ]
                                (text "Est. Asset at maturity")
                            , row
                                [ width shrink
                                , height <| px 24
                                , spacing 12
                                ]
                                [ row
                                    [ width shrink
                                    , height shrink
                                    , spacing 6
                                    ]
                                    [ images
                                        |> Image.viewToken
                                            [ width <| px 24
                                            , height <| px 24
                                            , centerY
                                            ]
                                            (pool.pair |> Pair.toAsset)
                                    , Truncate.viewSymbol
                                        { onMouseEnter = OnMouseEnter
                                        , onMouseLeave = OnMouseLeave
                                        , tooltip = Tooltip.Symbol TokenParam.Asset
                                        , opened = tooltip
                                        , token = pool.pair |> Pair.toAsset
                                        , theme = theme
                                        , customStyles = []
                                        }
                                    ]
                                , Truncate.viewAmount
                                    { onMouseEnter = OnMouseEnter
                                    , onMouseLeave = OnMouseLeave
                                    , tooltip = Tooltip.Amount TokenParam.Asset
                                    , opened = tooltip
                                    , token = pool.pair |> Pair.toAsset
                                    , amount = liqReturn.asset
                                    , theme = theme
                                    , customStyles = []
                                    }
                                ]
                            ]
                        , el
                            [ width <| px 1
                            , height fill
                            , theme |> ThemeColor.textDisabled |> Background.color
                            ]
                            none
                        , column
                            [ width shrink
                            , height shrink
                            , spacing 8
                            ]
                            [ el
                                [ width shrink
                                , height shrink
                                , Font.size 14
                                , paddingXY 0 3
                                , theme |> ThemeColor.textLight |> Font.color
                                ]
                                (text "Est. Collateral at maturity")
                            , row
                                [ width shrink
                                , height <| px 24
                                , spacing 12
                                ]
                                [ row
                                    [ width shrink
                                    , height shrink
                                    , spacing 6
                                    ]
                                    [ images
                                        |> Image.viewToken
                                            [ width <| px 24
                                            , height <| px 24
                                            , centerY
                                            ]
                                            (pool.pair |> Pair.toCollateral)
                                    , Truncate.viewSymbol
                                        { onMouseEnter = OnMouseEnter
                                        , onMouseLeave = OnMouseLeave
                                        , tooltip = Tooltip.Symbol TokenParam.Collateral
                                        , opened = tooltip
                                        , token = pool.pair |> Pair.toCollateral
                                        , theme = theme
                                        , customStyles = []
                                        }
                                    ]
                                , Truncate.viewAmount
                                    { onMouseEnter = OnMouseEnter
                                    , onMouseLeave = OnMouseLeave
                                    , tooltip = Tooltip.Amount TokenParam.Collateral
                                    , opened = tooltip
                                    , token = pool.pair |> Pair.toCollateral
                                    , amount = liqReturn.collateral
                                    , theme = theme
                                    , customStyles = []
                                    }
                                ]
                            ]
                        ]

            Failure _ ->
                [ row [ width shrink, height shrink, spacing 4 ]
                    [ images |> Image.error [ width <| px 24, height <| px 24 ]
                    , el [ Font.size 14, Font.color Color.negative400 ] (text "Error in fetching pool info")
                    ]
                ]

            Loading timeline ->
                el
                    [ width shrink
                    , height shrink
                    , centerY
                    ]
                    (Loading.view timeline theme)
                    |> List.singleton
        )


viewDues :
    { model | images : Images, theme : Theme }
    -> Blockchain
    ->
        { pool : Pool
        , return : Web ( PoolInfo, Status )
        , tooltip : Maybe Tooltip
        }
    -> Element Msg
viewDues { images, theme } blockchain { pool, return, tooltip } =
    case ( blockchain |> Blockchain.toUser, return ) of
        ( Just user, Success ( _, status ) ) ->
            case status of
                Maturity.Active (Success activeReturn) ->
                    user
                        |> User.getDues
                        |> Remote.map (Dict.get pool)
                        |> (Remote.map << Maybe.map)
                            (\dict ->
                                dict
                                    |> Dict.foldl
                                        (\_ { debt, collateral, collateralizedDebt } accumulator ->
                                            accumulator
                                                |> Maybe.andThen
                                                    (\accumulatedDue ->
                                                        Just Due
                                                            |> Maybe.apply
                                                                (Uint.add accumulatedDue.debt debt)
                                                            |> Maybe.apply
                                                                (Uint.add accumulatedDue.collateral collateral)
                                                            |> Maybe.apply
                                                                (collateralizedDebt |> Just)
                                                    )
                                        )
                                        (Due Uint.zero Uint.zero Nothing |> Just)
                                    |> (\maybeDue ->
                                            case maybeDue of
                                                Just totalDue ->
                                                    if totalDue.debt == Uint.zero then
                                                        none

                                                    else
                                                        column
                                                            [ width fill
                                                            , height fill
                                                            , theme |> ThemeColor.positionBG |> Background.color
                                                            , Border.rounded 8
                                                            , paddingXY 24 16
                                                            , spacing 20
                                                            ]
                                                            [ row [ spacing 8 ]
                                                                [ el
                                                                    [ width shrink
                                                                    , height shrink
                                                                    , Font.size 14
                                                                    , paddingXY 0 3
                                                                    , theme |> ThemeColor.textLight |> Font.color
                                                                    ]
                                                                    (text "Aggregate Borrow Position")
                                                                , images
                                                                    |> (case theme of
                                                                            Theme.Dark ->
                                                                                Image.info

                                                                            Theme.Light ->
                                                                                Image.infoDark
                                                                       )
                                                                        [ width <| px 12
                                                                        , height <| px 12
                                                                        , Font.center
                                                                        , centerX
                                                                        , Events.onMouseEnter (OnMouseEnter Tooltip.BorrowPositionInfo)
                                                                        , Events.onMouseLeave OnMouseLeave
                                                                        , (if tooltip == Just Tooltip.BorrowPositionInfo then
                                                                            paragraph
                                                                                [ Font.size 14
                                                                                , width <| px 345
                                                                                , theme |> ThemeColor.textLight |> Font.color
                                                                                ]
                                                                                [ "This is your total borrowed position in this Pool." |> text
                                                                                , " For more details and partial repayment, visit the Borrow positions screen."
                                                                                    |> text
                                                                                ]
                                                                                |> TooltipUtil.belowAlignLeft theme

                                                                           else
                                                                            none
                                                                          )
                                                                            |> below
                                                                        ]
                                                                ]
                                                            , row
                                                                [ width fill
                                                                , height fill
                                                                , spacing 48
                                                                ]
                                                                [ column
                                                                    [ width shrink
                                                                    , height shrink
                                                                    , spacing 8
                                                                    , centerY
                                                                    ]
                                                                    [ el
                                                                        [ width shrink
                                                                        , height shrink
                                                                        , Font.size 14
                                                                        , paddingXY 0 3
                                                                        , theme |> ThemeColor.textLight |> Font.color
                                                                        ]
                                                                        ("Total Assets to Repay"
                                                                            |> text
                                                                        )
                                                                    , row
                                                                        [ width shrink
                                                                        , height <| px 24
                                                                        , spacing 12
                                                                        ]
                                                                        [ row
                                                                            [ width shrink
                                                                            , height shrink
                                                                            , spacing 6
                                                                            ]
                                                                            [ images
                                                                                |> Image.viewToken
                                                                                    [ width <| px 24
                                                                                    , height <| px 24
                                                                                    , centerY
                                                                                    ]
                                                                                    (pool.pair |> Pair.toAsset)
                                                                            , Truncate.viewSymbol
                                                                                { onMouseEnter = OnMouseEnter
                                                                                , onMouseLeave = OnMouseLeave
                                                                                , tooltip = Tooltip.Symbol TokenParam.Asset
                                                                                , opened = tooltip
                                                                                , token = pool.pair |> Pair.toAsset
                                                                                , theme = theme
                                                                                , customStyles = []
                                                                                }
                                                                            ]
                                                                        , Truncate.viewAmount
                                                                            { onMouseEnter = OnMouseEnter
                                                                            , onMouseLeave = OnMouseLeave
                                                                            , tooltip = Tooltip.TotalDebt TokenParam.Asset
                                                                            , opened = tooltip
                                                                            , token = pool.pair |> Pair.toAsset
                                                                            , amount = totalDue.debt
                                                                            , theme = theme
                                                                            , customStyles = []
                                                                            }
                                                                        ]
                                                                    ]
                                                                , el
                                                                    [ width <| px 1
                                                                    , height fill
                                                                    , theme |> ThemeColor.textDisabled |> Background.color
                                                                    ]
                                                                    none
                                                                , column
                                                                    [ width shrink
                                                                    , height shrink
                                                                    , spacing 8
                                                                    ]
                                                                    [ el
                                                                        [ width shrink
                                                                        , height shrink
                                                                        , Font.size 14
                                                                        , paddingXY 0 3
                                                                        , theme |> ThemeColor.textLight |> Font.color
                                                                        ]
                                                                        ("Total Collateral to unlock"
                                                                            |> text
                                                                        )
                                                                    , row
                                                                        [ width shrink
                                                                        , height <| px 24
                                                                        , spacing 12
                                                                        ]
                                                                        [ row
                                                                            [ width shrink
                                                                            , height shrink
                                                                            , spacing 6
                                                                            ]
                                                                            [ images
                                                                                |> Image.viewToken
                                                                                    [ width <| px 24
                                                                                    , height <| px 24
                                                                                    , centerY
                                                                                    ]
                                                                                    (pool.pair |> Pair.toCollateral)
                                                                            , Truncate.viewSymbol
                                                                                { onMouseEnter = OnMouseEnter
                                                                                , onMouseLeave = OnMouseLeave
                                                                                , tooltip = Tooltip.TotalCollateralSymbol TokenParam.Collateral
                                                                                , opened = tooltip
                                                                                , token = pool.pair |> Pair.toCollateral
                                                                                , theme = theme
                                                                                , customStyles = []
                                                                                }
                                                                            ]
                                                                        , Truncate.viewAmount
                                                                            { onMouseEnter = OnMouseEnter
                                                                            , onMouseLeave = OnMouseLeave
                                                                            , tooltip = Tooltip.TotalCollateral TokenParam.Collateral
                                                                            , opened = tooltip
                                                                            , token = pool.pair |> Pair.toCollateral
                                                                            , amount = totalDue.collateral
                                                                            , theme = theme
                                                                            , customStyles = []
                                                                            }
                                                                        ]
                                                                    ]
                                                                , case ( activeReturn.isCDTApproved, activeReturn.isFlashRepayAllowed ) of
                                                                    ( False, _ ) ->
                                                                        row [ alignRight, centerY ]
                                                                            [ approveFlashRepayButton theme totalDue.collateralizedDebt ]

                                                                    ( True, False ) ->
                                                                        row [ alignRight, centerY ]
                                                                            [ disabledFlashRepayButton theme tooltip ]

                                                                    ( True, True ) ->
                                                                        row [ alignRight, centerY ]
                                                                            [ flashRepayButton theme totalDue.collateralizedDebt ]
                                                                ]
                                                            ]

                                                _ ->
                                                    none
                                       )
                            )
                        |> (Remote.map << Maybe.withDefault) none
                        |> Remote.withDefault none

                _ ->
                    none

        _ ->
            none


approveFlashRepayButton : Theme -> Maybe Address -> Element Msg
approveFlashRepayButton theme maybeCDTAddress =
    case maybeCDTAddress of
        Just cdtAddress ->
            Button.view
                { onPress = ClickApproveAndFlashRepay cdtAddress
                , text = "Approve & Flash Repay"
                , theme = theme
                }

        _ ->
            none


flashRepayButton : Theme -> Maybe Address -> Element Msg
flashRepayButton theme maybeCDTAddress =
    case maybeCDTAddress of
        Just cdtAddress ->
            Button.view
                { onPress = ClickFlashRepay cdtAddress
                , text = "Flash Repay"
                , theme = theme
                }

        _ ->
            none


disabledFlashRepayButton : Theme -> Maybe Tooltip -> Element Msg
disabledFlashRepayButton theme tooltip =
    el
        [ Region.description "Flash Repay disabled"
        , width fill
        , height <| px 44
        , paddingXY 12 4
        , theme |> ThemeColor.btnBackground |> Background.color
        , Border.rounded 4
        , Events.onMouseEnter (OnMouseEnter Tooltip.FlashRepayDisabled)
        , Events.onMouseLeave OnMouseLeave
        , (if tooltip == Just Tooltip.FlashRepayDisabled then
            el
                [ Font.size 14
                , width fill
                , theme |> ThemeColor.textLight |> Font.color
                ]
                ("Flash Repay is currently not possible as per our computation." |> text)
                |> TooltipUtil.belowAlignRight theme

           else
            none
          )
            |> below
        ]
        (el
            [ centerX
            , centerY
            , Font.size 16
            , Font.bold
            , paddingXY 0 4
            , theme |> ThemeColor.textDisabled |> Font.color
            ]
            (text "Flash Repay")
        )
