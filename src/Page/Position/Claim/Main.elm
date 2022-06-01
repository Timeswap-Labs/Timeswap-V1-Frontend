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
import Data.Theme as Theme exposing (Theme)
import Data.TokenParam as TokenParam
import Data.Uint as Uint
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
import Page.Position.Claim.Error exposing (Error)
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
import Utility.ThemeColor as ThemeColor
import Utility.Truncate as Truncate


type Position
    = Position
        { pool : Pool
        , state : State
        , tooltip : Maybe Tooltip
        }


type State
    = Active (Remote Error Return)
    | Matured (Web ( PoolInfo, Remote Error Return ))


type Msg
    = ClickLendMore
    | ClickClaim
    | ClickReturn
    | CheckMaturity Posix
    | Tick Posix
    | PoolInfoQueryAgain
    | QueryAgain Posix
    | ReceivePoolInfo Chain Pool (Result Http.Error PoolInfoAnswer.Answer)
    | ReceiveSum Value
    | ReceiveReturn Value
    | OnMouseEnter Tooltip
    | OnMouseLeave


type Effect
    = InputPool Pool
    | Withdraw WriteWithdraw


init :
    { model | time : Posix }
    -> Blockchain
    -> User
    -> Pool
    -> ( Position, Cmd Msg )
init { time } blockchain user pool =
    ( { pool = pool
      , state =
            if
                pool.maturity
                    |> Maturity.isActive time
            then
                Remote.loading
                    |> Active

            else
                Remote.loading
                    |> Matured
      , tooltip = Nothing
      }
        |> Position
    , if
        pool.maturity
            |> Maturity.isActive time
      then
        user
            |> User.getClaims
            |> Remote.map (Dict.get pool)
            |> (Remote.map << Maybe.map)
                (\claim ->
                    { pool = pool }
                        |> queryActive blockchain claim
                )
            |> (Remote.map << Maybe.withDefault) Cmd.none
            |> Remote.withDefault Cmd.none

      else
        get blockchain pool
    )


update :
    Blockchain
    -> User
    -> Msg
    -> Position
    -> ( Maybe Position, Cmd Msg, Maybe Effect )
update blockchain user msg (Position position) =
    case ( msg, position.state ) of
        ( ClickLendMore, Active _ ) ->
            ( Nothing
            , Cmd.none
            , InputPool position.pool |> Just
            )

        ( ClickClaim, Matured _ ) ->
            ( Position position |> Just
            , Cmd.none
            , user
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

        ( ClickReturn, _ ) ->
            ( Nothing
            , Cmd.none
            , Nothing
            )

        ( CheckMaturity posix, Active _ ) ->
            if
                position.pool.maturity
                    |> Maturity.isActive posix
            then
                position |> noCmdAndEffect

            else
                ( { position
                    | state =
                        Remote.loading
                            |> Matured
                  }
                    |> Position
                    |> Just
                , get blockchain position.pool
                , Nothing
                )

        ( Tick posix, Active remote ) ->
            { position
                | state =
                    remote
                        |> Remote.update posix
                        |> Active
            }
                |> noCmdAndEffect

        ( Tick posix, Matured (Success ( poolInfo, remote )) ) ->
            { position
                | state =
                    ( poolInfo
                    , remote |> Remote.update posix
                    )
                        |> Success
                        |> Matured
            }
                |> noCmdAndEffect

        ( Tick posix, Matured remote ) ->
            { position
                | state =
                    remote
                        |> Remote.update posix
                        |> Matured
            }
                |> noCmdAndEffect

        ( PoolInfoQueryAgain, Matured _ ) ->
            ( position
                |> Position
                |> Just
            , get blockchain position.pool
            , Nothing
            )

        ( QueryAgain _, Active _ ) ->
            ( position
                |> Position
                |> Just
            , user
                |> User.getClaims
                |> Remote.map (Dict.get position.pool)
                |> (Remote.map << Maybe.map)
                    (\claims ->
                        queryActive blockchain claims position
                    )
                |> (Remote.map << Maybe.withDefault) Cmd.none
                |> Remote.withDefault Cmd.none
            , Nothing
            )

        ( QueryAgain _, Matured (Success ( poolInfo, _ )) ) ->
            ( position
                |> Position
                |> Just
            , user
                |> User.getClaims
                |> Remote.map (Dict.get position.pool)
                |> (Remote.map << Maybe.map)
                    (\claimsIn ->
                        queryMatured blockchain poolInfo claimsIn position
                    )
                |> (Remote.map << Maybe.withDefault) Cmd.none
                |> Remote.withDefault Cmd.none
            , Nothing
            )

        ( ReceivePoolInfo chain pool result, Matured remote ) ->
            if
                (chain == (blockchain |> Blockchain.toChain))
                    && (pool == position.pool)
            then
                case result of
                    Ok (Right poolInfo) ->
                        ( { position
                            | state =
                                (case remote of
                                    Success ( _, return ) ->
                                        Success ( poolInfo, return )

                                    _ ->
                                        Success ( poolInfo, Remote.loading )
                                )
                                    |> Matured
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
                        ( { position
                            | state =
                                error
                                    |> Failure
                                    |> Matured
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

        ( ReceiveSum value, Active _ ) ->
            (case value |> Decode.decodeValue Query.decoderSum of
                Ok answer ->
                    if
                        (answer.chain == (blockchain |> Blockchain.toChain))
                            && (answer.pool == position.pool)
                            && (user
                                    |> User.getClaims
                                    |> Remote.map (Dict.get position.pool)
                                    |> (Remote.map << Maybe.map)
                                        (\claims ->
                                            answer.claims == claims
                                        )
                                    |> (Remote.map << Maybe.withDefault) False
                                    |> Remote.withDefault False
                               )
                    then
                        { position
                            | state =
                                answer.result
                                    |> toRemote
                                    |> Active
                        }

                    else
                        position

                _ ->
                    position
            )
                |> noCmdAndEffect

        ( ReceiveReturn value, Matured (Success ( poolInfo, _ )) ) ->
            case value |> Decode.decodeValue Query.decoderReturn of
                Ok answer ->
                    if
                        (answer.chain == (blockchain |> Blockchain.toChain))
                            && (answer.pool == position.pool)
                            && (answer.poolInfo == poolInfo)
                            && (user
                                    |> User.getClaims
                                    |> Remote.map (Dict.get position.pool)
                                    |> (Remote.map << Maybe.map)
                                        (\claimsIn ->
                                            (answer.claimsIn == claimsIn)
                                                && (claimsIn.bondPrincipal |> Uint.isZero |> not)
                                                && (claimsIn.insurancePrincipal |> Uint.isZero |> not)
                                        )
                                    |> (Remote.map << Maybe.withDefault) False
                                    |> Remote.withDefault False
                               )
                    then
                        ( { position
                            | state =
                                ( poolInfo
                                , answer.result |> toRemote
                                )
                                    |> Success
                                    |> Matured
                          }
                            |> Position
                            |> Just
                        , Cmd.none
                        , Nothing
                        )

                    else if
                        (answer.chain == (blockchain |> Blockchain.toChain))
                            && (answer.pool == position.pool)
                            && (answer.poolInfo == poolInfo)
                            && (user
                                    |> User.getClaims
                                    |> Remote.map (Dict.get position.pool)
                                    |> (Remote.map << Maybe.map)
                                        (\claimsIn ->
                                            answer.claimsIn
                                                == claimsIn
                                                && (claimsIn.bondPrincipal |> Uint.isZero)
                                                && (claimsIn.insurancePrincipal |> Uint.isZero)
                                        )
                                    |> (Remote.map << Maybe.withDefault) False
                                    |> Remote.withDefault False
                               )
                    then
                        ( Nothing
                        , Cmd.none
                        , Nothing
                        )

                    else
                        ( position |> Position |> Just
                        , Cmd.none
                        , Nothing
                        )

                _ ->
                    ( position |> Position |> Just
                    , Cmd.none
                    , Nothing
                    )

        ( OnMouseEnter tooltip, _ ) ->
            { position | tooltip = Just tooltip }
                |> noCmdAndEffect

        ( OnMouseLeave, _ ) ->
            { position | tooltip = Nothing }
                |> noCmdAndEffect

        _ ->
            position |> noCmdAndEffect


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
    , tooltip : Maybe Tooltip
    }
    -> ( Maybe Position, Cmd Msg, Maybe Effect )
noCmdAndEffect position =
    ( position
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


queryActive :
    Blockchain
    -> Claim
    ->
        { position
            | pool : Pool
        }
    -> Cmd Msg
queryActive blockchain claim { pool } =
    { chain = blockchain |> Blockchain.toChain
    , pool = pool
    , claims = claim
    }
        |> Query.givenSum
        |> querySum


queryMatured :
    Blockchain
    -> PoolInfo
    -> Claim
    ->
        { position
            | pool : Pool
        }
    -> Cmd Msg
queryMatured blockchain poolInfo claim { pool } =
    { chain = blockchain |> Blockchain.toChain
    , pool = pool
    , poolInfo = poolInfo
    , claimsIn = claim
    }
        |> Query.givenReturn
        |> queryClaim


port querySum : Value -> Cmd msg


port queryClaim : Value -> Cmd msg


port receiveSum : (Value -> msg) -> Sub msg


port receiveReturn : (Value -> msg) -> Sub msg


subscriptions : Position -> Sub Msg
subscriptions (Position { state }) =
    [ case state of
        Active remote ->
            remote |> Remote.subscriptions Tick

        Matured (Success ( _, remote )) ->
            remote |> Remote.subscriptions Tick

        Matured remote ->
            remote |> Remote.subscriptions Tick
    , receiveSum ReceiveSum
    , receiveReturn ReceiveReturn
    , case state of
        Active _ ->
            Time.every 1000 CheckMaturity

        _ ->
            Sub.none
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
    -> Position
    -> Element Msg
view ({ device, backdrop, theme } as model) (Position position) =
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
             , theme |> ThemeColor.border |> Border.color
             ]
                ++ Glass.background backdrop theme
            )
            [ header model position
            , viewClaim model position
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
        , spacing 16
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
            (Duration.viewExpiredMaturity
                { onMouseEnter = OnMouseEnter
                , onMouseLeave = OnMouseLeave
                , tooltip = Tooltip.Maturity
                , opened = tooltip
                , time = time
                , offset = offset
                , chosenZone = chosenZone
                , maturity = pool.maturity
                , theme = theme
                , images = images
                }
            )
        , if pool.maturity |> Maturity.isActive time then
            lendMoreButton theme

          else
            none
        , if pool.maturity |> Maturity.isActive time then
            claimDisabled theme

          else
            claimButton theme
        ]


lendMoreButton : Theme -> Element Msg
lendMoreButton theme =
    Input.button
        [ width <| px 110
        , height <| px 44
        , Border.rounded 4
        , Border.width 1
        , theme |> ThemeColor.btnHoverBG |> Border.color
        ]
        { onPress = Just ClickLendMore
        , label =
            el
                [ width shrink
                , height shrink
                , centerX
                , centerY
                , Font.size 16
                , Font.bold
                , theme |> ThemeColor.text |> Font.color
                ]
                (text "Lend More")
        }


claimButton : Theme -> Element Msg
claimButton theme =
    Input.button
        [ width <| px 102
        , height <| px 44
        , Border.rounded 4
        , theme |> ThemeColor.primaryBtn |> Background.color
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


claimDisabled : Theme -> Element msg
claimDisabled theme =
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
            , Font.color Color.transparent300
            , Font.bold
            ]
            (text "Claim")
        )


viewClaim :
    { model | images : Images, theme : Theme }
    ->
        { pool : Pool
        , state : State
        , tooltip : Maybe Tooltip
        }
    -> Element Msg
viewClaim ({ theme } as model) ({ state } as position) =
    row
        [ width fill
        , height <| px 82
        , theme |> ThemeColor.positionBG |> Background.color
        , Border.rounded 8
        , paddingXY 24 16
        , spacing 48
        ]
        (case state of
            Active remote ->
                [ viewBond model position remote
                , line model
                , viewInsurance model position remote
                ]

            Matured remote ->
                [ viewAssetReturn model position remote
                , line model
                , viewCollateralReturn model position remote
                ]
        )


viewBond :
    { model | images : Images, theme : Theme }
    ->
        { position
            | pool : Pool
            , tooltip : Maybe Tooltip
        }
    -> Remote Error Return
    -> Element Msg
viewBond { images, theme } { pool, tooltip } remote =
    column
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
                    , customStyles = []
                    }
                ]
            , case remote of
                Loading timeline ->
                    el
                        [ width shrink
                        , height shrink
                        ]
                        (Loading.view timeline theme)

                Failure error ->
                    none

                -- |> Debug.log "show error"
                Success { asset } ->
                    Truncate.viewAmount
                        { onMouseEnter = OnMouseEnter
                        , onMouseLeave = OnMouseLeave
                        , tooltip = Tooltip.Amount TokenParam.Asset
                        , opened = tooltip
                        , token = pool.pair |> Pair.toAsset
                        , amount = asset
                        , theme = theme
                        , customStyles = []
                        }
            ]
        ]


viewInsurance :
    { model | images : Images, theme : Theme }
    ->
        { position
            | pool : Pool
            , tooltip : Maybe Tooltip
        }
    -> Remote Error Return
    -> Element Msg
viewInsurance { images, theme } { pool, tooltip } remote =
    column
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
                    , customStyles = []
                    }
                ]
            , case remote of
                Loading timeline ->
                    el
                        [ width shrink
                        , height shrink
                        ]
                        (Loading.view timeline theme)

                Failure error ->
                    none

                -- |> Debug.log "show error"
                Success { collateral } ->
                    Truncate.viewAmount
                        { onMouseEnter = OnMouseEnter
                        , onMouseLeave = OnMouseLeave
                        , tooltip = Tooltip.Amount TokenParam.Collateral
                        , opened = tooltip
                        , token = pool.pair |> Pair.toCollateral
                        , amount = collateral
                        , theme = theme
                        , customStyles = []
                        }
            ]
        ]


viewAssetReturn :
    { model | images : Images, theme : Theme }
    ->
        { position
            | pool : Pool
            , tooltip : Maybe Tooltip
        }
    -> Web ( poolInfo, Remote Error Return )
    -> Element Msg
viewAssetReturn { images, theme } { pool, tooltip } remote =
    column
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
                    , customStyles = []
                    }
                ]
            , case remote of
                Loading timeline ->
                    el
                        [ width shrink
                        , height shrink
                        ]
                        (Loading.view timeline theme)

                Failure error ->
                    none

                -- |> Debug.log "show error"
                Success ( _, Loading timeline ) ->
                    el
                        [ width shrink
                        , height shrink
                        ]
                        (Loading.view timeline theme)

                Success ( _, Failure error ) ->
                    none

                -- |> Debug.log "show error"
                Success ( _, Success { asset } ) ->
                    Truncate.viewAmount
                        { onMouseEnter = OnMouseEnter
                        , onMouseLeave = OnMouseLeave
                        , tooltip = Tooltip.Amount TokenParam.Asset
                        , opened = tooltip
                        , token = pool.pair |> Pair.toAsset
                        , amount = asset
                        , theme = theme
                        , customStyles = []
                        }
            ]
        ]


viewCollateralReturn :
    { model | images : Images, theme : Theme }
    ->
        { position
            | pool : Pool
            , tooltip : Maybe Tooltip
        }
    -> Web ( poolInfo, Remote Error Return )
    -> Element Msg
viewCollateralReturn { images, theme } { pool, tooltip } remote =
    column
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
                    , tooltip = Tooltip.Symbol TokenParam.Asset
                    , opened = tooltip
                    , token = pool.pair |> Pair.toCollateral
                    , theme = theme
                    , customStyles = []
                    }
                ]
            , case remote of
                Loading timeline ->
                    el
                        [ width shrink
                        , height shrink
                        ]
                        (Loading.view timeline theme)

                Failure error ->
                    none

                -- |> Debug.log "show error"
                Success ( _, Loading timeline ) ->
                    el
                        [ width shrink
                        , height shrink
                        ]
                        (Loading.view timeline theme)

                Success ( _, Failure error ) ->
                    none

                -- |> Debug.log "show error"
                Success ( _, Success { collateral } ) ->
                    Truncate.viewAmount
                        { onMouseEnter = OnMouseEnter
                        , onMouseLeave = OnMouseLeave
                        , tooltip = Tooltip.Amount TokenParam.Collateral
                        , opened = tooltip
                        , token = pool.pair |> Pair.toCollateral
                        , amount = collateral
                        , theme = theme
                        , customStyles = []
                        }
            ]
        ]


line : { model | theme : Theme } -> Element msg
line { theme } =
    el
        [ width <| px 1
        , height fill
        , theme |> ThemeColor.textDisabled |> Background.color
        ]
        none
