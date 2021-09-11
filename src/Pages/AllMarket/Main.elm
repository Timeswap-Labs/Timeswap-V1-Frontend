module Pages.AllMarket.Main exposing (Msg, Page, init, update, view)

import Data.Chain exposing (Chain(..))
import Data.Device as Device exposing (Device)
import Data.Images exposing (Images)
import Data.Maturity exposing (Maturity)
import Data.Pair as Pair exposing (Pair)
import Data.Pools as Pools exposing (PoolInfo, Pools)
import Data.Remote exposing (Remote)
import Data.TokenImages exposing (TokenImages)
import Data.ZoneInfo exposing (ZoneInfo)
import Element
    exposing
        ( Element
        , alignRight
        , alignTop
        , centerX
        , centerY
        , column
        , el
        , fill
        , height
        , none
        , paddingXY
        , px
        , rotate
        , row
        , shrink
        , spacing
        , text
        , width
        )
import Element.Font as Font
import Element.Input as Input
import Element.Keyed as Keyed
import Pages.PairMarket.ListPools as ListPools
import Pages.PairMarket.Tooltip exposing (Tooltip)
import Sort.Set as Set exposing (Set)
import Time exposing (Posix)
import Utility.Color as Color
import Utility.Glass as Glass
import Utility.Image as Image
import Utility.PairInfo as PairInfo


type Page
    = Page
        { expandedSet : Set Pair
        , tooltip : Maybe Tooltip
        }


init : { model | pools : Pools } -> Page
init { pools } =
    { expandedSet =
        pools
            |> Pools.getFirst
            |> Maybe.map (Set.singleton Pair.sorter)
            |> Maybe.withDefault (Set.empty Pair.sorter)
    , tooltip = Nothing
    }
        |> Page


type Msg
    = Expand Pair
    | Collapse Pair
    | OnMouseEnter Tooltip
    | OnMouseLeave


type alias Msgs =
    { onMouseEnter : Tooltip -> Msg
    , onMouseLeave : Msg
    }


update : { model | pools : Pools } -> Msg -> Page -> Page
update { pools } msg (Page page) =
    case msg of
        Expand pair ->
            { page
                | expandedSet =
                    pools
                        |> Pools.toPairs
                        |> (\list ->
                                if list |> List.member pair then
                                    page.expandedSet
                                        |> Set.insert pair

                                else
                                    page.expandedSet
                           )
            }
                |> Page

        Collapse pair ->
            { page
                | expandedSet =
                    page.expandedSet |> Set.remove pair
            }
                |> Page

        OnMouseEnter tooltip ->
            { page | tooltip = Just tooltip }
                |> Page

        OnMouseLeave ->
            { page | tooltip = Nothing }
                |> Page


msgs : Msgs
msgs =
    { onMouseEnter = OnMouseEnter
    , onMouseLeave = OnMouseLeave
    }


view :
    { model
        | device : Device
        , time : Posix
        , zoneInfo : Maybe ZoneInfo
        , images : Images
        , tokenImages : TokenImages
        , pools : Pools
    }
    -> Page
    -> Element Msg
view ({ device } as model) (Page page) =
    column
        [ (if Device.isPhone device then
            px 335

           else if Device.isTablet device then
            px 552

           else
            px 1068
          )
            |> width
        , height shrink
        , spacing 30
        , alignTop
        , centerX
        ]
        [ title model
        , allPairs model page
        ]


title : { model | device : Device } -> Element msg
title { device } =
    el
        [ paddingXY 0 4
        , Font.bold
        , if Device.isPhoneOrTablet device then
            Font.size 18

          else
            Font.size 24
        , Font.color Color.transparent500
        ]
        (text "All Pairs")


allPairs :
    { model
        | device : Device
        , time : Posix
        , zoneInfo : Maybe ZoneInfo
        , images : Images
        , tokenImages : TokenImages
        , pools : Pools
    }
    -> { page | expandedSet : Set Pair, tooltip : Maybe Tooltip }
    -> Element Msg
allPairs ({ time, pools } as model) page =
    Keyed.column
        [ width fill
        , height shrink
        , spacing 12
        ]
        (pools
            |> Pools.toList time
            |> List.map
                (\( pair, list ) ->
                    ( pair |> Pair.toKey
                    , singlePair model page ( pair, list )
                    )
                )
        )


singlePair :
    { model
        | device : Device
        , time : Posix
        , zoneInfo : Maybe ZoneInfo
        , images : Images
        , tokenImages : TokenImages
        , pools : Pools
    }
    -> { page | expandedSet : Set Pair, tooltip : Maybe Tooltip }
    -> ( Pair, List ( Maturity, Remote () PoolInfo ) )
    -> Element Msg
singlePair ({ tokenImages } as model) ({ expandedSet, tooltip } as page) ( pair, list ) =
    column
        [ width fill
        , height shrink
        ]
        [ row
            ([ width fill
             , height <| px 72
             , paddingXY 24 0
             , spacing 18
             ]
                ++ Glass.lightPrimary 1
            )
            [ PairInfo.icons tokenImages pair
            , PairInfo.symbols pair
            , pairSize list
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
                                if pair |> Set.memberOf expandedSet then
                                    Collapse pair |> Just

                                else
                                    Expand pair |> Just
                            , label = element
                            }
               )
        , if (pair |> Set.memberOf expandedSet) && (list |> List.isEmpty |> not) then
            ListPools.view msgs model { pair = pair, tooltip = tooltip } list

          else
            none
        ]


pairSize : List ( Maturity, Remote () PoolInfo ) -> Element msg
pairSize list =
    el
        [ width shrink
        , height shrink
        , alignRight
        , centerY
        , Font.regular
        , Font.size 16
        , Font.color Color.transparent500
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
    { model | images : Images }
    -> { page | expandedSet : Set Pair }
    -> ( Pair, List ( Maturity, Remote () PoolInfo ) )
    -> Element Msg
discloser { images } { expandedSet } ( pair, list ) =
    if list |> List.isEmpty then
        el
            [ width <| px 12
            , height <| px 12
            , alignRight
            , centerY
            ]
            none

    else
        Image.discloser images
            [ width <| px 12
            , alignRight
            , centerY
            , if pair |> Set.memberOf expandedSet then
                degrees 180 |> rotate

              else
                degrees 0 |> rotate
            ]
