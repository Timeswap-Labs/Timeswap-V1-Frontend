port module Page.Position.Claim.Main exposing
    ( Effect(..)
    , Msg
    , Position
    , init
    , subscriptions
    , update
    , view
    )

import Blockchain.Main as Blockchain exposing (Blockchain)
import Blockchain.User.Claim exposing (Claim)
import Blockchain.User.Main as User exposing (User)
import Blockchain.User.Return exposing (Return)
import Blockchain.User.WriteWithdraw exposing (WriteWithdraw)
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
import Data.Theme exposing (Theme)
import Data.TokenParam as TokenParam
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
import Page.Position.Claim.Error as Error exposing (Error)
import Page.Position.Claim.Query as Query
import Page.Position.Claim.Tooltip as Tooltip exposing (Tooltip)
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
        , return : Maybe (Web ( PoolInfo, Remote Error Return ))
        , tooltip : Maybe Tooltip
        }


type Msg
    = ClickLendMore
    | ClickClaim
    | ClickReturn
    | CheckMaturity Posix
    | Tick Posix
    | PoolInfoQueryAgain
    | QueryAgain Posix
    | ReceivePoolInfo Chain Pool (Result Http.Error PoolInfoAnswer.Answer)
    | ReceiveAnswer Value
    | OnMouseEnter Tooltip
    | OnMouseLeave


type Effect
    = InputPool Pool
    | Withdraw WriteWithdraw


init :
    { model | time : Posix }
    -> Blockchain
    -> Pool
    -> ( Position, Cmd Msg )
init { time } blockchain pool =
    ( { pool = pool
      , return =
            if
                pool.maturity
                    |> Maturity.isActive time
            then
                Nothing

            else
                Just Remote.loading
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
        ClickLendMore ->
            ( Nothing
            , Cmd.none
            , InputPool position.pool |> Just
            )

        ClickClaim ->
            ( Position position |> Just
            , Cmd.none
            , if
                position.pool.maturity
                    |> Maturity.isActive time
              then
                Nothing

              else
                user
                    |> User.getClaims
                    |> Remote.map (Dict.get position.pool)
                    |> (Remote.map << Maybe.andThen)
                        (\claimsIn ->
                            { pool = position.pool
                            , claimsIn = claimsIn
                            }
                                |> Withdraw
                                |> Just
                        )
                    |> Remote.withDefault Nothing
            )

        ClickReturn ->
            ( Nothing
            , Cmd.none
            , Nothing
            )

        CheckMaturity posix ->
            case
                ( position.pool.maturity
                    |> Maturity.isActive posix
                , position.return
                )
            of
                ( False, Nothing ) ->
                    ( { position
                        | return = Just Remote.loading
                      }
                        |> Position
                        |> Just
                    , get blockchain position.pool
                    , Nothing
                    )

                _ ->
                    ( position
                        |> Position
                        |> Just
                    , Cmd.none
                    , Nothing
                    )

        Tick posix ->
            ( { position
                | return =
                    case position.return of
                        Just (Success ( poolInfo, remote )) ->
                            ( poolInfo
                            , remote |> Remote.update posix
                            )
                                |> Success
                                |> Just

                        Just remote ->
                            remote
                                |> Remote.update posix
                                |> Just

                        _ ->
                            position.return
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
            , case position.return of
                Just _ ->
                    get blockchain position.pool

                _ ->
                    Cmd.none
            , Nothing
            )

        QueryAgain _ ->
            ( position
                |> Position
                |> Just
            , case position.return of
                Just (Success ( poolInfo, _ )) ->
                    user
                        |> User.getClaims
                        |> Remote.map (Dict.get position.pool)
                        |> (Remote.map << Maybe.map)
                            (\claimsIn ->
                                query blockchain poolInfo claimsIn position
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
                                case position.return of
                                    Just (Success ( _, return )) ->
                                        Just (Success ( poolInfo, return ))

                                    _ ->
                                        Just (Success ( poolInfo, Remote.loading ))
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
                            |> Debug.log "fix"
                        , Process.sleep 5000
                            |> Task.perform (\_ -> PoolInfoQueryAgain)
                        , Nothing
                        )

                    Err error ->
                        ( { position
                            | return =
                                error
                                    |> Failure
                                    |> Just
                          }
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
                ( Ok answer, Just (Success ( poolInfo, _ )) ) ->
                    if
                        (answer.chainId == (blockchain |> Blockchain.toChain))
                            && (answer.pool == position.pool)
                            && (answer.poolInfo == poolInfo)
                    then
                        ( { position
                            | return =
                                ( poolInfo, answer.result |> toRemote )
                                    |> Success
                                    |> Just
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


toRemote :
    Result Error answer
    -> Remote Error answer
toRemote result =
    case result of
        Ok claims ->
            Success claims

        Err error ->
            Failure error


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
    -> Claim
    ->
        { position
            | pool : Pool
        }
    -> Cmd Msg
query blockchain poolInfo claim { pool } =
    { chainId = blockchain |> Blockchain.toChain
    , pool = pool
    , poolInfo = poolInfo
    , claimsIn = claim
    }
        |> Query.givenClaim
        |> queryClaim


port queryClaim : Value -> Cmd msg


subscriptions : Position -> Sub Msg
subscriptions (Position { return }) =
    [ return
        |> (Maybe.map << Remote.map) Tuple.second
        |> (Maybe.map << Remote.map) (Remote.subscriptions Tick)
        |> (Maybe.map << Remote.withDefault)
            (return
                |> Maybe.map (Remote.subscriptions Tick)
                |> Maybe.withDefault Sub.none
            )
        |> Maybe.withDefault Sub.none
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
            [ header model position
            , viewClaim model user position
            ]
        ]


returnButton : { model | images : Images } -> Element Msg
returnButton { images } =
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
                    |> Image.arrowDown
                        [ width <| px 16
                        , height <| px 16
                        , centerY
                        , (pi / 2)
                            |> rotate
                        ]
                , el
                    [ width shrink
                    , height shrink
                    , Font.size 16
                    , paddingXY 0 2
                    , Font.color Color.transparent500
                    , centerY
                    ]
                    (text "Back to lend")
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
            lendMoreButton

          else
            lendMoreDisabled
        , if pool.maturity |> Maturity.isActive time then
            lendDisabled

          else
            lendButton
        ]


lendMoreButton : Element Msg
lendMoreButton =
    Input.button
        [ width <| px 102
        , height <| px 44
        , Border.rounded 4
        , Border.width 1
        , Border.color Color.primary300
        ]
        { onPress = Just ClickLendMore
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
                (text "Lend More")
        }


lendMoreDisabled : Element msg
lendMoreDisabled =
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
            (text "Lend More")
        )


lendButton : Element Msg
lendButton =
    Input.button
        [ width <| px 102
        , height <| px 44
        , Border.rounded 4
        , Background.color Color.primary500
        ]
        { onPress = Just ClickClaim
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
                (text "Claim")
        }


lendDisabled : Element msg
lendDisabled =
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
            (text "Claim")
        )


viewClaim :
    { model | images : Images, theme : Theme }
    -> User
    ->
        { pool : Pool
        , return : Maybe (Web ( PoolInfo, Remote Error Return ))
        , tooltip : Maybe Tooltip
        }
    -> Element Msg
viewClaim { images, theme } user { pool, return, tooltip } =
    row
        [ width fill
        , height <| px 82
        , Background.color Color.dark500
        , Border.rounded 8
        , paddingXY 24 0
        , spacing 48
        ]
        (return
            |> Maybe.map
                (\remote ->
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
                            , Font.color Color.transparent300
                            ]
                            (text "Asset to Receive")
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
                                    }
                                ]
                            , case remote of
                                Loading timeline ->
                                    el
                                        [ width shrink
                                        , height shrink
                                        ]
                                        (Loading.view timeline)

                                Failure error ->
                                    none |> Debug.log "show error"

                                Success ( _, Loading timeline ) ->
                                    el
                                        [ width shrink
                                        , height shrink
                                        ]
                                        (Loading.view timeline)

                                Success ( _, Failure error ) ->
                                    none |> Debug.log "show error"

                                Success ( _, Success { asset } ) ->
                                    Truncate.viewAmount
                                        { onMouseEnter = OnMouseEnter
                                        , onMouseLeave = OnMouseLeave
                                        , tooltip = Tooltip.Amount TokenParam.Asset
                                        , opened = tooltip
                                        , token = pool.pair |> Pair.toAsset
                                        , amount = asset
                                        , theme = theme
                                        }
                            ]
                        ]
                    , el
                        [ width <| px 1
                        , height fill
                        , Background.color Color.transparent100
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
                            , Font.color Color.transparent300
                            ]
                            (text "Collateral to Receive")
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
                                    }
                                ]
                            , case remote of
                                Loading timeline ->
                                    el
                                        [ width shrink
                                        , height shrink
                                        ]
                                        (Loading.view timeline)

                                Failure error ->
                                    none |> Debug.log "show error"

                                Success ( _, Loading timeline ) ->
                                    el
                                        [ width shrink
                                        , height shrink
                                        ]
                                        (Loading.view timeline)

                                Success ( _, Failure error ) ->
                                    none |> Debug.log "show error"

                                Success ( _, Success { collateral } ) ->
                                    Truncate.viewAmount
                                        { onMouseEnter = OnMouseEnter
                                        , onMouseLeave = OnMouseLeave
                                        , tooltip = Tooltip.Amount TokenParam.Collateral
                                        , opened = tooltip
                                        , token = pool.pair |> Pair.toCollateral
                                        , amount = collateral
                                        , theme = theme
                                        }
                            ]
                        ]
                    ]
                )
            |> Maybe.withDefault
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
                        (text "Amount to Receive")
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
                                }
                            ]
                        , user
                            |> User.getClaims
                            |> Remote.map (Dict.get pool)
                            |> (\remote ->
                                    case remote of
                                        Loading timeline ->
                                            el
                                                [ width shrink
                                                , height shrink
                                                ]
                                                (Loading.view timeline)

                                        Failure error ->
                                            none |> Debug.log "show error"

                                        Success (Just { bond }) ->
                                            Truncate.viewAmount
                                                { onMouseEnter = OnMouseEnter
                                                , onMouseLeave = OnMouseLeave
                                                , tooltip = Tooltip.Amount TokenParam.Asset
                                                , opened = tooltip
                                                , token = pool.pair |> Pair.toAsset
                                                , amount = bond
                                                , theme = theme
                                                }

                                        _ ->
                                            none |> Debug.log "show error"
                               )
                        ]
                    ]
                , el
                    [ width <| px 1
                    , height fill
                    , Background.color Color.transparent100
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
                        , Font.color Color.transparent300
                        ]
                        (text "Amount Protecting")
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
                                }
                            ]
                        , user
                            |> User.getClaims
                            |> Remote.map (Dict.get pool)
                            |> (\remote ->
                                    case remote of
                                        Loading timeline ->
                                            el
                                                [ width shrink
                                                , height shrink
                                                ]
                                                (Loading.view timeline)

                                        Failure error ->
                                            none |> Debug.log "show error"

                                        Success (Just { insurance }) ->
                                            Truncate.viewAmount
                                                { onMouseEnter = OnMouseEnter
                                                , onMouseLeave = OnMouseLeave
                                                , tooltip = Tooltip.Amount TokenParam.Collateral
                                                , opened = tooltip
                                                , token = pool.pair |> Pair.toCollateral
                                                , amount = insurance
                                                , theme = theme
                                                }

                                        _ ->
                                            none |> Debug.log "show error"
                               )
                        ]
                    ]
                ]
        )
