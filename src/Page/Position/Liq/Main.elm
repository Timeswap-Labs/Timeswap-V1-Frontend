port module Page.Position.Liq.Main exposing
    ( Effect(..)
    , Msg
    , Position
    , init
    , subscriptions
    , update
    )

import Blockchain.Main as Blockchain exposing (Blockchain)
import Blockchain.User.Liq exposing (Liq)
import Blockchain.User.Main as User exposing (User)
import Blockchain.User.Return exposing (Return)
import Blockchain.User.WriteBurn exposing (WriteBurn)
import Data.Backdrop exposing (Backdrop)
import Data.Chain exposing (Chain)
import Data.ChosenZone exposing (ChosenZone)
import Data.Device exposing (Device(..))
import Data.Images exposing (Images)
import Data.Maturity as Maturity
import Data.Offset exposing (Offset)
import Data.Or exposing (Or(..))
import Data.Pool exposing (Pool)
import Data.Remote as Remote exposing (Remote(..))
import Data.Theme as Theme exposing (Theme)
import Data.Web exposing (Web)
import Element
    exposing
        ( Element
        , alignRight
        , centerX
        , centerY
        , column
        , el
        , fill
        , height
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
import Json.Decode as Decode
import Json.Encode exposing (Value)
import Page.Answer as PoolInfoAnswer
import Page.PoolInfo exposing (PoolInfo)
import Page.Position.Liq.Error exposing (Error)
import Page.Position.Liq.Query as Query
import Page.Position.Liq.Tooltip as Tooltip exposing (Tooltip)
import Page.Query as PoolInfoQuery
import Process
import Sort.Dict as Dict
import Task
import Time exposing (Posix)
import Utility.Color as Color
import Utility.Duration as Duration
import Utility.Glass as Glass
import Utility.Image as Image
import Utility.Loading as Loading
import Utility.PairImage as PairImage
import Utility.Truncate as Truncate


type Position
    = Position
        { pool : Pool
        , return : Web ( PoolInfo, Status )
        , tooltip : Maybe Tooltip
        }


type alias Status =
    Maturity.Status (Remote Error Return) (Remote Error Float)


type Msg
    = ClickAddMore
    | ClickBurn
    | ClickReturn
    | Tick Posix
    | PoolInfoQueryAgain
    | QueryAgain Posix
    | ReceivePoolInfo Chain Pool (Result Http.Error PoolInfoAnswer.Answer)
    | ReceiveAnswer Value
    | OnMouseEnter Tooltip
    | OnMouseLeave


type Effect
    = InputPool Pool
    | Burn WriteBurn


init :
    { model | time : Posix }
    -> Blockchain
    -> Pool
    -> ( Position, Cmd Msg )
init { time } blockchain pool =
    ( { pool = pool
      , return = Remote.loading
      , tooltip = Nothing
      }
        |> Position
    , if
        pool.maturity
            |> Maturity.isActive time
      then
        Cmd.none

      else
        get blockchain pool
    )


update :
    { model | time : Posix }
    -> Blockchain
    -> User
    -> Msg
    -> Position
    -> ( Maybe Position, Cmd Msg, Maybe Effect )
update { time } blockchain user msg (Position position) =
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
            , get blockchain position.pool
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
                                query blockchain poolInfo liquidityIn position
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
                          -- |> Debug.log "fix"
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

        ReceiveAnswer value ->
            case
                ( value |> Decode.decodeValue Query.decoder
                , position.return
                )
            of
                ( Ok answer, Success ( poolInfo, status ) ) ->
                    if
                        (answer.chainId == (blockchain |> Blockchain.toChain))
                            && (answer.pool == position.pool)
                            && (answer.poolInfo == poolInfo)
                    then
                        ( { position
                            | return =
                                ( poolInfo
                                , case answer.result of
                                    Ok (Maturity.Active float) ->
                                        Success float
                                            |> Maturity.Active

                                    Ok (Maturity.Matured return) ->
                                        Success return
                                            |> Maturity.Matured

                                    Err error ->
                                        case status of
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
    -> Pool
    -> Cmd Msg
get blockchain pool =
    blockchain
        |> Blockchain.toChain
        |> (\chain ->
                Http.get
                    { url =
                        pool
                            |> PoolInfoQuery.toUrlString chain
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
    -> Cmd Msg
query blockchain poolInfo liquidityIn { pool } =
    { chainId = blockchain |> Blockchain.toChain
    , pool = pool
    , poolInfo = poolInfo
    , liquidityIn = liquidityIn
    }
        |> Query.givenLiq
        |> queryLiq


port queryStake : Value -> Cmd msg


port queryLiq : Value -> Cmd msg


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
    , Time.every 1000 QueryAgain
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
    -> User
    -> Position
    -> Element Msg
view ({ device, backdrop, theme } as model) user (Position position) =
    column
        [ width shrink
        , height shrink
        , spacing 20
        ]
        [ returnButton model
        , column
            ([ Region.description "lend positions"
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
             , Border.color Color.transparent100
             ]
                ++ Glass.background backdrop theme
            )
            [ header model position ]
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
                    , paddingXY 0 2
                    , Font.color Color.transparent500
                    , centerY
                    ]
                    (text "Back to liquidity")
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
            burnDisabled

          else
            burnButton
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


burnButton : Element Msg
burnButton =
    Input.button
        [ width <| px 102
        , height <| px 44
        , Border.rounded 4
        , Background.color Color.primary500
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


burnDisabled : Element msg
burnDisabled =
    el
        [ width <| px 102
        , height <| px 44
        , Border.rounded 4
        , Background.color Color.transparent200
        ]
        (el
            [ width shrink
            , height shrink
            , centerX
            , centerY
            , Font.size 16
            , Font.color Color.transparent300
            , Font.bold
            ]
            (text "Burn")
        )


viewLiq :
    { model | images : Images, theme : Theme }
    -> User
    ->
        { pool : Pool
        , return : Web ( PoolInfo, Status )
        , tooltip : Maybe Tooltip
        }
    -> Element Msg
viewLiq { images, theme } user { pool, return, tooltip } =
    row
        [ width fill
        , height <| px 82
        , Background.color Color.dark500
        , Border.rounded 8
        , paddingXY 24 0
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
                                , Font.color Color.transparent300
                                ]
                                (text "Asset to Receive")
                            , el
                                [ width shrink
                                , height <| px 24
                                ]
                                (el
                                    [ width shrink
                                    , height shrink
                                    , centerY
                                    ]
                                    (Loading.view timeline)
                                )
                            ]
                        ]

                    Maturity.Active (Failure error) ->
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
                                , Font.color Color.transparent300
                                ]
                                (text "Asset to Receive")
                            , none

                            -- |> Debug.log "edit"
                            ]
                        ]

                    _ ->
                        []

            Failure error ->
                []

            -- |> Debug.log "change"
            Loading timeline ->
                el
                    [ width shrink
                    , height shrink
                    , centerY
                    ]
                    (Loading.view timeline)
                    |> List.singleton
        )
