module Page.Markets.Main exposing (Msg, PoolsData, init, update, view)

import Blockchain.Main as Blockchain exposing (Blockchain)
import Data.Backdrop exposing (Backdrop)
import Data.Chain as Chain exposing (Chain)
import Data.Chains exposing (Chains)
import Data.ChosenZone exposing (ChosenZone)
import Data.Images exposing (Images)
import Data.Offset exposing (Offset)
import Data.Pair as Pair exposing (Pair)
import Data.Parameter as Parameter
import Data.Pool exposing (Pool)
import Data.PriceFeed as PriceFeed exposing (PriceFeed)
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
        , below
        , centerX
        , centerY
        , column
        , el
        , fill
        , height
        , link
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
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Keyed as Keyed
import Http
import Page.Markets.Answer as Answer exposing (Answer)
import Page.Markets.Tooltip as Tooltip exposing (Tooltip)
import Page.PoolInfo exposing (PoolInfo)
import Page.Route as Route
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
import Utility.Tooltip as TooltipUtil
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
    -> Chains
    -> ( PoolsData, Cmd Msg )
init { time } blockchain chains =
    ( { data = Remote.loading
      , expanded = Set.empty Pair.sorter
      , tooltip = Nothing
      }
        |> PoolsData
    , get blockchain chains
    )


get :
    Blockchain
    -> Chains
    -> Cmd Msg
get blockchain chains =
    blockchain
        |> Blockchain.toChain
        |> (\chain ->
                Http.get
                    { url =
                        Builder.crossOrigin "https://ts-mainnet-week8.herokuapp.com/v1/activepools"
                            []
                            [ chain |> Chain.toQueryParameter ]
                    , expect =
                        Answer.decoder chain chains
                            |> Http.expectJson
                                (ReceiveAnswer chain)
                    }
           )


update : Msg -> Blockchain -> Chains -> PoolsData -> ( PoolsData, Cmd Msg )
update msg blockchain chains (PoolsData page) =
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
                    , expanded =
                        case result of
                            Ok a ->
                                getFirstPairSet a

                            Err _ ->
                                Set.empty Pair.sorter
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
            , get blockchain chains
            )


view :
    { model
        | time : Posix
        , offset : Offset
        , chosenZone : ChosenZone
        , backdrop : Backdrop
        , theme : Theme
        , images : Images
        , priceFeed : PriceFeed
    }
    -> PoolsData
    -> Element Msg
view ({ backdrop, theme } as model) ((PoolsData { data }) as page) =
    case data of
        Success poolsDataDict ->
            column
                ([ width <| minimum 1200 fill
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
            el
                [ Font.size 18
                , Font.color Color.negative500
                ]
                (text "Error occurred while fetching pools")


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
        , priceFeed : PriceFeed
    }
    -> Answer
    -> PoolsData
    -> Element Msg
allPairs model poolsDataDict page =
    if (poolsDataDict |> Dict.size) == 0 then
        el
            [ Font.size 16
            , model.theme |> ThemeColor.text |> Font.color
            , alignLeft
            ]
            (text "No pools are available at the moment")

    else
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
        , priceFeed : PriceFeed
    }
    -> PoolsData
    -> ( Pair, List ( Pool, PoolInfo ) )
    -> Element Msg
singlePair ({ images, theme } as model) ((PoolsData { expanded, tooltip }) as page) ( pair, list ) =
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
        , priceFeed : PriceFeed
    }
    -> PoolsData
    -> List ( Pool, PoolInfo )
    -> Element Msg
poolDetails ({ theme, images } as model) ((PoolsData { tooltip }) as page) poolList =
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
                    row
                        [ width fill
                        , height shrink
                        , spacing 8
                        , paddingXY 0 20
                        , centerX
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
                        , Font.center
                        , theme |> ThemeColor.textLight |> Font.color
                        ]
                        [ el [ Font.size 12, Font.center, centerX ] (text "TOTAL LENT")
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
                                , Events.onMouseEnter (OnMouseEnter Tooltip.TotalLendInfo)
                                , Events.onMouseLeave OnMouseLeave
                                , (if tooltip == Just Tooltip.TotalLendInfo then
                                    el
                                        [ Font.size 14
                                        , model.theme |> ThemeColor.textLight |> Font.color
                                        ]
                                        ("This is the total amount lent in the Pool, after adjusting for the Fee" |> text)
                                        |> TooltipUtil.belowAlignLeft model.theme

                                   else
                                    none
                                  )
                                    |> below
                                ]
                        ]
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
                            (totalLentAmount model page info)
              }
            , { header =
                    row
                        [ width fill
                        , height shrink
                        , paddingXY 0 20
                        , spacing 8
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
                        , theme |> ThemeColor.textLight |> Font.color
                        , Font.center
                        ]
                        [ el [ Font.size 12, Font.center, centerX ] (text "TOTAL BORROWED")
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
                                , Events.onMouseEnter (OnMouseEnter Tooltip.TotalBorrowInfo)
                                , Events.onMouseLeave OnMouseLeave
                                , (if tooltip == Just Tooltip.TotalBorrowInfo then
                                    el
                                        [ Font.size 14
                                        , model.theme |> ThemeColor.textLight |> Font.color
                                        ]
                                        ("This is the total amount borrowed from the Pool, after adjusting for the Fee" |> text)
                                        |> TooltipUtil.belowAlignLeft model.theme

                                   else
                                    none
                                  )
                                    |> below
                                ]
                        ]
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
                            (totalBorrowedAmount model page info)
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
                        (text "MIN CDP")
              , width = px 130
              , view =
                    \info ->
                        el
                            [ height <| px 72
                            , centerY
                            , Border.solid
                            , Border.widthEach
                                { top = 0
                                , right = 0
                                , bottom = 1
                                , left = 0
                                }
                            , theme |> ThemeColor.textboxBorder |> Border.color
                            ]
                            (collateralFactor model page info)
              }
            , { header =
                    el
                        [ width fill
                        , height fill
                        , theme |> ThemeColor.tableHeaderBG |> Background.color
                        , Border.solid
                        , Border.widthEach
                            { top = 1
                            , right = 1
                            , bottom = 1
                            , left = 0
                            }
                        , theme |> ThemeColor.textboxBorder |> Border.color
                        ]
                        none
              , width = px 211
              , view =
                    \{ pool } ->
                        el
                            [ height <| px 72
                            , paddingEach
                                { top = 0
                                , right = 24
                                , bottom = 0
                                , left = 0
                                }
                            , Border.solid
                            , Border.widthEach
                                { top = 0
                                , right = 1
                                , bottom = 1
                                , left = 0
                                }
                            , theme |> ThemeColor.textboxBorder |> Border.color
                            ]
                            (buttons model pool)
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
liquidities model (PoolsData { tooltip }) { pool, poolInfo } =
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
        ]


totalLentAmount :
    { model | theme : Theme }
    -> PoolsData
    ->
        { pool : Pool
        , poolInfo : PoolInfo
        }
    -> Element Msg
totalLentAmount model (PoolsData { tooltip }) { pool, poolInfo } =
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
            (case poolInfo.totalLend of
                Just totalLend ->
                    [ Truncate.viewAmount
                        { onMouseEnter = OnMouseEnter
                        , onMouseLeave = OnMouseLeave
                        , tooltip = Tooltip.TotalLend pool
                        , opened = tooltip
                        , token = pool.pair |> Pair.toAsset
                        , amount = totalLend
                        , theme = model.theme
                        , customStyles = [ Font.size 14 ]
                        }
                    , text (pool.pair |> Pair.toAsset |> Token.toSymbol)
                    ]

                _ ->
                    [ text "N/A" ]
            )
        ]


totalBorrowedAmount :
    { model | theme : Theme }
    -> PoolsData
    ->
        { pool : Pool
        , poolInfo : PoolInfo
        }
    -> Element Msg
totalBorrowedAmount model (PoolsData { tooltip }) { pool, poolInfo } =
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
            (case poolInfo.totalBorrow of
                Just totalBorrow ->
                    [ Truncate.viewAmount
                        { onMouseEnter = OnMouseEnter
                        , onMouseLeave = OnMouseLeave
                        , tooltip = Tooltip.TotalBorrow pool
                        , opened = tooltip
                        , token = pool.pair |> Pair.toAsset
                        , amount = totalBorrow
                        , theme = model.theme
                        , customStyles = [ Font.size 14 ]
                        }
                    , text (pool.pair |> Pair.toAsset |> Token.toSymbol)
                    ]

                _ ->
                    [ text "N/A" ]
            )
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
            , centerX
            , Font.bold
            , Font.size 14
            , Font.color Color.warning500
            , Font.center
            ]
            (poolInfo.apr |> Calculate.apr)
        )


collateralFactor :
    { model | theme : Theme, priceFeed : PriceFeed }
    -> PoolsData
    ->
        { pool : Pool
        , poolInfo : PoolInfo
        }
    -> Element Msg
collateralFactor { theme, priceFeed } (PoolsData { tooltip }) { pool, poolInfo } =
    el
        [ px 130
            |> width
        , height shrink
        , paddingEach
            { top = 5
            , right = 0
            , bottom = 3
            , left = 0
            }
        , centerX
        , centerY
        , Font.center
        , Font.bold
        ]
        (el
            [ centerX, centerY ]
            (column
                [ Font.size 14
                , theme |> ThemeColor.text |> Font.color
                , alignRight
                ]
                [ Calculate.cdp
                    { onMouseEnter = OnMouseEnter
                    , onMouseLeave = OnMouseLeave
                    , cdpTooltip = Tooltip.CDP pool
                    , opened = tooltip
                    , pair = pool.pair
                    , cdp = poolInfo.cdp
                    , theme = theme
                    }
                    priceFeed
                    14
                , if
                    case priceFeed of
                        PriceFeed.Ignore ->
                            False

                        PriceFeed.Utilize ->
                            poolInfo.cdp.percent
                                |> Maybe.map (\_ -> True)
                                |> Maybe.withDefault False
                  then
                    none

                  else
                    Truncate.viewCDPSymbol
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


buttons :
    { model | theme : Theme }
    -> Pool
    -> Element msg
buttons { theme } pool =
    row
        [ width <| px 177
        , centerY
        , alignRight
        , spacing 7
        ]
        [ el
            [ width fill
            , height shrink
            ]
            (link
                [ width fill
                , height <| px 44
                , theme |> ThemeColor.primaryBtn |> Background.color
                , Border.rounded 4
                , Font.bold
                , Font.size 16
                , Font.color Color.light100
                ]
                { url = Route.toUrlString (Route.Lend (Parameter.Pool pool |> Just))
                , label =
                    el
                        [ centerX
                        , centerY
                        ]
                        (text "Lend")
                }
            )
        , el
            [ width fill
            , height shrink
            ]
            (link
                [ width fill
                , height <| px 44
                , theme |> ThemeColor.primaryBtn |> Background.color
                , Border.rounded 4
                , Font.bold
                , Font.size 16
                , Font.color Color.light100
                ]
                { url = Route.toUrlString (Route.Borrow (Parameter.Pool pool |> Just))
                , label =
                    el
                        [ centerX
                        , centerY
                        ]
                        (text "Borrow")
                }
            )
        ]


getPairSet : Answer -> Set Pair
getPairSet poolsDataDict =
    poolsDataDict
        |> Dict.keys
        |> List.map (\pool -> pool.pair)
        |> Set.fromList Pair.sorter


getFirstPairSet : Answer -> Set Pair
getFirstPairSet poolsDataDict =
    poolsDataDict
        |> Dict.keys
        |> List.map (\pool -> pool.pair)
        |> List.head
        |> Maybe.map (\head -> [ head ])
        |> Maybe.withDefault []
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
