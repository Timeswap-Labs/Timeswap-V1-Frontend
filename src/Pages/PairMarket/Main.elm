module Pages.PairMarket.Main exposing (Msg, Page, getPair, init, toUrl, update, view)

import Data.Device as Device exposing (Device)
import Data.Pair as Pair exposing (Pair)
import Data.Pools as Pools exposing (Pools)
import Data.ZoneInfo exposing (ZoneInfo)
import Element
    exposing
        ( Element
        , above
        , alignLeft
        , alignRight
        , alignTop
        , alpha
        , below
        , centerX
        , centerY
        , column
        , el
        , fill
        , height
        , none
        , padding
        , paddingEach
        , paddingXY
        , paragraph
        , px
        , row
        , shrink
        , spacing
        , text
        , width
        )
import Element.Font as Font
import Pages.PairMarket.ListPools as ListPools
import Time exposing (Posix)
import Utility.Color as Color
import Utility.PairInfo as PairInfo


type Page
    = Page Pair


init : Pair -> Page
init pair =
    Page pair


toUrl : Pair -> String
toUrl pair =
    [ "#market"
    , pair |> Pair.toFragment
    ]
        |> String.join "?"


getPair : Page -> Pair
getPair (Page pair) =
    pair


type Msg
    = Msg


update : Msg -> Page -> Page
update msg page =
    page


view :
    { model
        | device : Device
        , time : Posix
        , zoneInfo : Maybe ZoneInfo
        , pools : Pools
    }
    -> Page
    -> Element msg
view ({ device, pools } as model) (Page pair) =
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
        , spacing 24
        , alignTop
        , centerX
        ]
        [ title model pair
        , pools
            |> Pools.toListSinglePool pair
            |> ListPools.view model pair
        ]


title : { model | device : Device, time : Posix, pools : Pools } -> Pair -> Element msg
title model pair =
    row
        [ width fill
        , height shrink
        , spacing 14
        ]
        [ PairInfo.icons { iconSize = 32 } pair
        , PairInfo.symbols { fontSize = 18, isBold = True } pair
        , size model pair
        ]


size : { model | device : Device, time : Posix, pools : Pools } -> Pair -> Element msg
size { device, time, pools } pair =
    el
        [ width shrink
        , height shrink
        , alignRight
        , centerY
        , Font.regular
        , if Device.isPhoneOrTablet device then
            Font.size 16

          else
            Font.size 18
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
