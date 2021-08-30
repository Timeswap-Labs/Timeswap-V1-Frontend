module Pages.AllMarket.Main exposing (Msg, Page, init, toUrl, update, view)

import Data.Chain exposing (Chain(..))
import Data.Device as Device exposing (Device)
import Data.Maturity as Maturity exposing (Maturity)
import Data.Pair as Pair exposing (Pair)
import Data.Pools as Pools exposing (PoolInfo, Pools)
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
import Sort.Set as Set exposing (Set)
import Time exposing (Posix)
import User
import Utility.Color as Color
import Utility.Glass as Glass
import Utility.Image as Image
import Utility.PairInfo as PairInfo


type Page
    = Page (Set Pair)


init : { model | pools : Pools, user : Maybe { user | chain : Chain } } -> Page
init { pools, user } =
    user
        |> User.toChain
        |> (\chain ->
                pools
                    |> Pools.getFirst
                    |> Maybe.map (Set.singleton (Pair.sorter chain))
                    |> Maybe.withDefault (Set.empty (Pair.sorter chain))
                    |> Page
           )


toUrl : String
toUrl =
    "#market"


type Msg
    = Expand Pair
    | Collapse Pair


update : { model | pools : Pools } -> Msg -> Page -> Page
update { pools } msg (Page set) =
    case msg of
        Expand pair ->
            pools
                |> Pools.toPairs
                |> (\list ->
                        if list |> List.member pair then
                            set
                                |> Set.insert pair
                                |> Page

                        else
                            Page set
                   )

        Collapse pair ->
            set |> Set.remove pair |> Page


view :
    { model
        | device : Device
        , time : Posix
        , zoneInfo : Maybe ZoneInfo
        , pools : Pools
    }
    -> Page
    -> Element Msg
view ({ device } as model) (Page set) =
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
        , allPairs model set
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
        , pools : Pools
    }
    -> Set Pair
    -> Element Msg
allPairs ({ pools } as model) set =
    Keyed.column
        [ width fill
        , height shrink
        , spacing 12
        ]
        (pools
            |> Pools.toList
            |> List.map
                (\( pair, list ) ->
                    ( pair |> Pair.toKey
                    , singlePair model set ( pair, list )
                    )
                )
        )


singlePair :
    { model
        | device : Device
        , time : Posix
        , zoneInfo : Maybe ZoneInfo
        , pools : Pools
    }
    -> Set Pair
    -> ( Pair, List PoolInfo )
    -> Element Msg
singlePair ({ time } as model) set ( pair, list ) =
    list
        |> List.filter (\{ maturity } -> maturity |> Maturity.isActive time)
        |> (\filteredList ->
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
                        [ PairInfo.icons { iconSize = 32 } pair
                        , PairInfo.symbols { fontSize = 16, isBold = True } pair
                        , size model pair
                        , discloser set pair filteredList
                        ]
                        |> (\element ->
                                if filteredList |> List.isEmpty then
                                    element

                                else
                                    Input.button
                                        [ width fill
                                        , height shrink
                                        ]
                                        { onPress =
                                            if pair |> Set.memberOf set then
                                                Collapse pair |> Just

                                            else
                                                Expand pair |> Just
                                        , label = element
                                        }
                           )
                    , if (pair |> Set.memberOf set) && (list |> List.isEmpty |> not) then
                        ListPools.view model pair filteredList

                      else
                        none
                    ]
           )


size : { model | time : Posix, pools : Pools } -> Pair -> Element msg
size { time, pools } pair =
    el
        [ width shrink
        , height shrink
        , alignRight
        , centerY
        , Font.regular
        , Font.size 16
        , Font.color Color.transparent500
        ]
        (pools
            |> Pools.getSize time pair
            |> String.fromInt
            |> (\string ->
                    if string == "1" then
                        "1 Pool"

                    else
                        string ++ " Pools"
               )
            |> text
        )


discloser : Set Pair -> Pair -> List { poolInfo | maturity : Maturity } -> Element msg
discloser set pair list =
    if list |> List.isEmpty then
        el
            [ width <| px 12
            , height <| px 12
            , alignRight
            , centerY
            ]
            none

    else
        Image.discloser
            [ width <| px 12
            , alignRight
            , centerY
            , if pair |> Set.memberOf set then
                degrees 180 |> rotate

              else
                degrees 0 |> rotate
            ]
