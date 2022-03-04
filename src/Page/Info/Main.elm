module Page.Info.Main exposing (Msg, PoolsData, init, update, view)

import Blockchain.Main as Blockchain exposing (Blockchain)
import Data.Backdrop exposing (Backdrop)
import Data.Chain as Chain exposing (Chain)
import Data.ChosenZone exposing (ChosenZone)
import Data.Images exposing (Images)
import Data.Offset exposing (Offset)
import Data.Pair as Pair exposing (Pair)
import Data.Pool as Pool exposing (Pool)
import Data.Remote as Remote exposing (Remote(..))
import Data.Theme as Theme exposing (Theme)
import Data.Token as Token
import Data.Web as Web
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
        , minimum
        , none
        , padding
        , paddingEach
        , paddingXY
        , px
        , rotate
        , row
        , shrink
        , spacing
        , table
        , text
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Keyed as Keyed
import Http
import Page.Info.Answer as Answer exposing (Answer)
import Page.Info.Tooltip as Tooltip exposing (Tooltip)
import Page.PoolInfo as PoolInfo exposing (PoolInfo)
import Process
import Sort.Dict as Dict exposing (Dict)
import Sort.Set as Set exposing (Set)
import Task
import Time exposing (Posix)
import Url.Builder as Builder
import Utility.Calculate as Calculate
import Utility.Color as Color
import Utility.Duration as Duration
import Utility.Glass as Glass
import Utility.Image as Image
import Utility.Loading as Loading
import Utility.PairImage as PairImage
import Utility.ThemeColor as ThemeColor
import Utility.Truncate as Truncate


type PoolsData
    = PoolsData
        { data : Remote Http.Error Answer
        , expanded : Set Pair
        , tooltip : Maybe Tooltip
        }


type Msg
    = OnMouseEnter Tooltip
    | OnMouseLeave
    | Expand Pair
    | Collapse Pair
    | ReceiveAnswer Chain (Result Http.Error Answer)
    | QueryAgain


init :
    { model | time : Posix }
    -> Blockchain
    -> ( PoolsData, Cmd Msg )
init { time } blockchain =
    ( { data = Remote.loading
      , expanded = Set.empty Pair.sorter
      , tooltip = Nothing
      }
        |> PoolsData
    , get blockchain
    )


get :
    Blockchain
    -> Cmd Msg
get blockchain =
    blockchain
        |> Blockchain.toChain
        |> (\chain ->
                Http.get
                    { url =
                        Builder.crossOrigin "https://backend-new-conv.herokuapp.com/v1/activepools"
                            []
                            [ chain |> Chain.toQueryParameter ]
                    , expect =
                        Answer.decoder
                            |> Http.expectJson
                                (ReceiveAnswer chain)
                    }
           )


update : Msg -> Blockchain -> PoolsData -> ( PoolsData, Cmd Msg )
update msg blockchain (PoolsData page) =
    case msg of
        Expand pair ->
            case page.data of
                Success poolsDataDict ->
                    ( { page
                        | expanded =
                            poolsDataDict
                                |> getPairSet
                                |> Set.toList
                                |> (\list ->
                                        if list |> List.member pair then
                                            page.expanded
                                                |> Set.insert pair

                                        else
                                            page.expanded
                                   )
                      }
                        |> PoolsData
                    , Cmd.none
                    )

                _ ->
                    ( page |> PoolsData
                    , Cmd.none
                    )

        Collapse pair ->
            ( { page
                | expanded =
                    page.expanded |> Set.remove pair
              }
                |> PoolsData
            , Cmd.none
            )

        OnMouseEnter tooltip ->
            ( { page | tooltip = Just tooltip }
                |> PoolsData
            , Cmd.none
            )

        OnMouseLeave ->
            ( { page | tooltip = Nothing }
                |> PoolsData
            , Cmd.none
            )

        ReceiveAnswer chain result ->
            if chain == (blockchain |> Blockchain.toChain) then
                ( { page
                    | data =
                        case result of
                            Ok a ->
                                Success a

                            Err error ->
                                Failure error

                    -- result
                    --     |> Web.fromResult
                  }
                    |> PoolsData
                , case result |> Web.fromResult of
                    Failure _ ->
                        Process.sleep 10000
                            |> Task.perform (\_ -> QueryAgain)

                    _ ->
                        Cmd.none
                )

            else
                ( page |> PoolsData
                , Cmd.none
                )

        QueryAgain ->
            ( page |> PoolsData
            , get blockchain
            )


view :
    { model
        | time : Posix
        , offset : Offset
        , chosenZone : ChosenZone
        , backdrop : Backdrop
        , theme : Theme
        , images : Images
    }
    -> PoolsData
    -> Element Msg
view ({ backdrop, theme } as model) ((PoolsData { data }) as page) =
    case data of
        Success poolsDataDict ->
            column
                ([ width <| minimum 800 fill
                 , height shrink
                 , spacing 30
                 , padding 24
                 , alignTop
                 , centerX
                 , Border.rounded 8
                 , Border.width 1
                 , theme |> ThemeColor.textboxBorder |> Border.color
                 ]
                    ++ Glass.background backdrop theme
                )
                [ title model
                , allPairs model poolsDataDict page
                ]

        Loading timeline ->
            Loading.view timeline theme

        Failure _ ->
            none


title : { model | theme : Theme } -> Element Msg
title { theme } =
    el
        [ paddingXY 0 4
        , Font.bold
        , Font.size 24
        , theme |> ThemeColor.text |> Font.color
        ]
        (text "All Pairs")


allPairs :
    { model
        | time : Posix
        , images : Images
        , theme : Theme
        , offset : Offset
        , chosenZone : ChosenZone
    }
    -> Answer
    -> PoolsData
    -> Element Msg
allPairs ({ time, images } as model) poolsDataDict page =
    Keyed.column
        [ width fill
        , height shrink
        , spacing 16
        ]
        (poolsDataDict
            |> getPairSet
            |> constructPoolData poolsDataDict
            |> Dict.toList
            |> List.map
                (\( pair, poolList ) ->
                    ( pair |> Pair.toString
                    , singlePair model page ( pair, poolList )
                    )
                )
        )


singlePair :
    { model
        | time : Posix
        , images : Images
        , theme : Theme
        , offset : Offset
        , chosenZone : ChosenZone
    }
    -> PoolsData
    -> ( Pair, List ( Pool, PoolInfo ) )
    -> Element Msg
singlePair ({ time, images, theme } as model) ((PoolsData { expanded, tooltip }) as page) ( pair, list ) =
    column
        [ width fill
        , height shrink
        ]
        [ row
            [ width fill
            , height <| px 72
            , paddingXY 24 0
            , spacing 18
            , Border.widthEach
                { top = 1
                , right = 1
                , bottom =
                    if expanded |> Set.toList |> List.member pair then
                        0

                    else
                        1
                , left = 1
                }
            , theme |> ThemeColor.textboxBorder |> Border.color
            ]
            [ images
                |> PairImage.view
                    { pair = pair
                    , length = 32
                    }
            , Truncate.viewPairSymbol
                { onMouseEnter = OnMouseEnter
                , onMouseLeave = OnMouseLeave
                , tooltip = Tooltip.PairSymbol pair
                , opened = tooltip
                , pair = pair
                , fontSize = 16
                , fontPadding = 2
                , theme = theme
                }
            , pairSize list theme
            , discloser model page ( pair, list )
            ]
            |> (\element ->
                    if list |> List.isEmpty then
                        element

                    else
                        Input.button
                            [ width fill
                            , height shrink
                            ]
                            { onPress =
                                if pair |> Set.memberOf expanded then
                                    Collapse pair |> Just

                                else
                                    Expand pair |> Just
                            , label = element
                            }
               )
        , if (pair |> Set.memberOf expanded) && (list |> List.isEmpty |> not) then
            poolDetails model page list

          else
            none
        ]


pairSize : List ( Pool, PoolInfo ) -> Theme -> Element Msg
pairSize list theme =
    el
        [ width shrink
        , height shrink
        , alignRight
        , centerY
        , Font.regular
        , Font.size 16
        , theme |> ThemeColor.text |> Font.color
        ]
        (list
            |> List.length
            |> String.fromInt
            |> (\string ->
                    if string == "1" then
                        "1 Pool"

                    else
                        string ++ " Pools"
               )
            |> text
        )


discloser :
    { model | images : Images, theme : Theme }
    -> PoolsData
    -> ( Pair, List ( Pool, PoolInfo ) )
    -> Element Msg
discloser { images, theme } (PoolsData { expanded }) ( pair, list ) =
    if list |> List.isEmpty then
        el
            [ width <| px 12
            , height <| px 12
            , alignRight
            , centerY
            ]
            none

    else
        images
            |> (case theme of
                    Theme.Dark ->
                        Image.discloser

                    Theme.Light ->
                        Image.arrowDownDark
               )
                [ width <| px 12
                , alignRight
                , centerY
                , if pair |> Set.memberOf expanded then
                    degrees 180 |> rotate

                  else
                    degrees 0 |> rotate
                ]


poolDetails :
    { model
        | time : Posix
        , images : Images
        , theme : Theme
        , offset : Offset
        , chosenZone : ChosenZone
    }
    -> PoolsData
    -> List ( Pool, PoolInfo )
    -> Element Msg
poolDetails ({ theme } as model) page poolList =
    table
        [ width fill
        , height shrink
        ]
        { data =
            poolList
                |> List.map
                    (\( pool, poolInfo ) ->
                        { pool = pool
                        , poolInfo = poolInfo
                        }
                    )
        , columns =
            [ { header =
                    el
                        [ width fill
                        , height shrink
                        , paddingXY 24 20
                        , theme |> ThemeColor.tableHeaderBG |> Background.color
                        , Border.solid
                        , Border.widthEach
                            { top = 1
                            , right = 0
                            , bottom = 1
                            , left = 1
                            }
                        , theme |> ThemeColor.textboxBorder |> Border.color
                        , Font.bold
                        , Font.size 12
                        , theme |> ThemeColor.textLight |> Font.color
                        ]
                        (text "MATURITY TIME")
              , width = fill
              , view =
                    \{ pool } ->
                        el
                            [ height <| px 72
                            , paddingEach
                                { top = 0
                                , right = 0
                                , bottom = 0
                                , left = 24
                                }
                            , Border.solid
                            , Border.widthEach
                                { top = 0
                                , right = 0
                                , bottom = 1
                                , left = 1
                                }
                            , theme |> ThemeColor.textboxBorder |> Border.color
                            ]
                            (maturityInfo model pool page)
              }
            , { header =
                    el
                        [ width fill
                        , height shrink
                        , paddingXY 0 20
                        , theme |> ThemeColor.tableHeaderBG |> Background.color
                        , Border.solid
                        , Border.widthEach
                            { top = 1
                            , right = 0
                            , bottom = 1
                            , left = 0
                            }
                        , theme |> ThemeColor.textboxBorder |> Border.color
                        , Font.bold
                        , Font.size 12
                        , theme |> ThemeColor.textLight |> Font.color
                        , Font.center
                        ]
                        (text "LIQUIDITY")
              , width = px 170
              , view =
                    \info ->
                        el
                            [ width fill
                            , height <| px 72
                            , Border.solid
                            , Border.widthEach
                                { top = 0
                                , right = 0
                                , bottom = 1
                                , left = 0
                                }
                            , theme |> ThemeColor.textboxBorder |> Border.color
                            ]
                            (liquidities model page info)
              }
            , { header =
                    el
                        [ width fill
                        , height shrink
                        , paddingXY 0 20
                        , theme |> ThemeColor.tableHeaderBG |> Background.color
                        , Border.solid
                        , Border.widthEach
                            { top = 1
                            , right = 0
                            , bottom = 1
                            , left = 0
                            }
                        , theme |> ThemeColor.textboxBorder |> Border.color
                        , Font.bold
                        , Font.size 12
                        , theme |> ThemeColor.textLight |> Font.color
                        , Font.center
                        ]
                        (text "MAX APR")
              , width = px 130
              , view =
                    \{ poolInfo } ->
                        el
                            [ height <| px 72
                            , Border.solid
                            , Border.widthEach
                                { top = 0
                                , right = 0
                                , bottom = 1
                                , left = 0
                                }
                            , theme |> ThemeColor.textboxBorder |> Border.color
                            ]
                            (estimatedAPR poolInfo)
              }
            , { header =
                    el
                        [ width fill
                        , height shrink
                        , paddingXY 0 20
                        , theme |> ThemeColor.tableHeaderBG |> Background.color
                        , Border.solid
                        , Border.widthEach
                            { top = 1
                            , right = 1
                            , bottom = 1
                            , left = 0
                            }
                        , theme |> ThemeColor.textboxBorder |> Border.color
                        , Font.bold
                        , Font.size 12
                        , theme |> ThemeColor.textLight |> Font.color
                        , Font.center
                        ]
                        (text "MIN CDP")
              , width = px 190
              , view =
                    \info ->
                        el
                            [ height <| px 72
                            , Border.solid
                            , Border.widthEach
                                { top = 0
                                , right = 1
                                , bottom = 1
                                , left = 0
                                }
                            , theme |> ThemeColor.textboxBorder |> Border.color
                            ]
                            (collateralFactor model page info)
              }
            ]
        }


maturityInfo :
    { model | time : Posix, chosenZone : ChosenZone, offset : Offset, images : Images, theme : Theme }
    -> Pool
    -> PoolsData
    -> Element Msg
maturityInfo { time, chosenZone, offset, images, theme } pool (PoolsData { tooltip }) =
    row
        [ width shrink
        , height shrink
        , alignLeft
        , centerY
        , spacing 12
        ]
        [ images
            |> Image.hourglassPrimary
                [ width <| px 16
                , centerY
                ]
        , column
            [ width shrink
            , height shrink
            , spacing 1
            ]
            [ el
                [ width shrink
                , height shrink
                , paddingXY 0 3
                , spacing 4
                , Font.size 14
                , Font.bold
                , Font.color Color.transparent500
                ]
                (Duration.viewMaturity
                    { onMouseEnter = OnMouseEnter
                    , onMouseLeave = OnMouseLeave
                    , tooltip = Tooltip.Maturity pool
                    , opened = tooltip
                    , time = time
                    , offset = offset
                    , chosenZone = chosenZone
                    , maturity = pool.maturity
                    , theme = theme
                    }
                )
            ]
        ]


liquidities :
    { model | theme : Theme }
    -> PoolsData
    ->
        { pool : Pool
        , poolInfo : PoolInfo
        }
    -> Element Msg
liquidities ({ theme } as model) (PoolsData { tooltip }) { pool, poolInfo } =
    column
        [ width shrink
        , centerX
        , centerY
        , spacing 4
        , Font.bold
        , Font.size 14
        ]
        [ row
            [ Font.size 14
            , model.theme |> ThemeColor.text |> Font.color
            , alignRight
            , spacing 6
            ]
            [ Truncate.viewAmount
                { onMouseEnter = OnMouseEnter
                , onMouseLeave = OnMouseLeave
                , tooltip = Tooltip.AssetLiquidity pool
                , opened = tooltip
                , token = pool.pair |> Pair.toAsset
                , amount = poolInfo.x
                , theme = model.theme
                , customStyles = [ Font.size 14 ]
                }
            , text (pool.pair |> Pair.toAsset |> Token.toSymbol)
            ]

        -- , row
        --     [ Font.size 14
        --     , model.theme |> ThemeColor.text |> Font.color
        --     , alignRight
        --     , spacing 6
        --     ]
        --     [ Truncate.viewAmount
        --         { onMouseEnter = OnMouseEnter
        --         , onMouseLeave = OnMouseLeave
        --         , tooltip = Tooltip.CollateralLiquidity pool
        --         , opened = tooltip
        --         , token = pool.pair |> Pair.toCollateral
        --         , amount = poolInfo.collateralReserve
        --         , theme = model.theme
        --         , customStyles = [ Font.size 14 ]
        --         }
        --     , text (pool.pair |> Pair.toCollateral |> Token.toSymbol)
        --     ]
        ]


estimatedAPR :
    PoolInfo
    -> Element Msg
estimatedAPR poolInfo =
    el
        [ width <| px 130
        , height shrink
        , paddingXY 0 3
        , centerX
        , centerY
        ]
        (el
            [ width shrink
            , height shrink
            , paddingXY 10 8
            , centerX
            , Background.color Color.positive100
            , Border.rounded 28
            , Font.bold
            , Font.size 14
            , Font.color Color.positive500
            , Font.center
            ]
            (poolInfo.apr |> Calculate.apr)
        )


collateralFactor :
    { model | theme : Theme }
    -> PoolsData
    ->
        { pool : Pool
        , poolInfo : PoolInfo
        }
    -> Element Msg
collateralFactor ({ theme } as model) (PoolsData { tooltip }) { pool, poolInfo } =
    el
        [ px 170
            |> width
        , height shrink
        , paddingXY 0 3
        , centerX
        , centerY
        ]
        (el
            [ centerX ]
            (row
                [ Font.size 14
                , model.theme |> ThemeColor.text |> Font.color
                , alignRight
                , spacing 6
                ]
                [ Truncate.viewAmount
                    { onMouseEnter = OnMouseEnter
                    , onMouseLeave = OnMouseLeave
                    , tooltip = Tooltip.CDP pool
                    , opened = tooltip
                    , token = pool.pair |> Pair.toAsset
                    , amount = poolInfo.cdp.ratio
                    , theme = model.theme
                    , customStyles = [ Font.size 14 ]
                    }
                , Truncate.viewCDPSymbol
                    { onMouseEnter = OnMouseEnter
                    , onMouseLeave = OnMouseLeave
                    , tooltip = Tooltip.CDPSymbol pool
                    , opened = tooltip
                    , pair = pool.pair
                    , theme = theme
                    }
                ]
            )
        )


getPairSet : Answer -> Set Pair
getPairSet poolsDataDict =
    poolsDataDict
        |> Dict.keys
        |> List.map (\pool -> pool.pair)
        |> Set.fromList Pair.sorter


constructPoolData :
    Dict Pool PoolInfo
    -> Set Pair
    -> Dict Pair (List ( Pool, PoolInfo ))
constructPoolData poolsDataDict pairSet =
    pairSet
        |> Set.toList
        |> List.foldl
            (\pair accDict ->
                accDict
                    |> Dict.insert
                        pair
                        (poolsDataDict
                            |> Dict.toList
                            |> List.filter (\( pool, _ ) -> pair == pool.pair)
                        )
            )
            (Dict.empty Pair.sorter)
