module Pages.PairMarket.Main exposing
    ( Msg
    , Page
    , fromFragment
    , getPair
    , init
    , update
    , view
    )

import Data.Device as Device exposing (Device)
import Data.Images exposing (Images)
import Data.Pair exposing (Pair)
import Data.Pools as Pools exposing (Pools)
import Data.TokenImages exposing (TokenImages)
import Data.Tokens exposing (Tokens)
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
        , px
        , row
        , shrink
        , spacing
        , text
        , width
        )
import Element.Font as Font
import Pages.PairMarket.ListPools as ListPools
import Pages.PairMarket.Tooltip exposing (Tooltip)
import Time exposing (Posix)
import Utility.Color as Color
import Utility.PairInfo as PairInfo


type Page
    = Page
        { pair : Pair
        , tooltip : Maybe Tooltip
        }


init : Pair -> Page
init pair =
    Page
        { pair = pair
        , tooltip = Nothing
        }


fromFragment :
    { model | tokens : Tokens, pools : Pools }
    -> String
    -> Maybe Page
fromFragment { tokens, pools } string =
    string
        |> Pools.fromPairFragment tokens pools
        |> Maybe.map init


getPair : Page -> Pair
getPair (Page { pair }) =
    pair


type Msg
    = OnMouseEnter Tooltip
    | OnMouseLeave


type alias Msgs =
    { onMouseEnter : Tooltip -> Msg
    , onMouseLeave : Msg
    }


update : Msg -> Page -> Page
update msg (Page page) =
    case msg of
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
view ({ device, time, pools } as model) (Page ({ pair } as page)) =
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
        [ title model page
        , pools
            |> Pools.toListSinglePair time pair
            |> ListPools.view msgs model page
        ]


title :
    { model
        | device : Device
        , time : Posix
        , tokenImages : TokenImages
        , pools : Pools
    }
    -> { page | pair : Pair }
    -> Element msg
title ({ tokenImages } as model) ({ pair } as page) =
    row
        [ width fill
        , height shrink
        , spacing 14
        ]
        [ PairInfo.icons tokenImages pair
        , PairInfo.symbols pair
        , pairSize model page
        ]


pairSize :
    { model | device : Device, time : Posix, pools : Pools }
    -> { page | pair : Pair }
    -> Element msg
pairSize { device, time, pools } { pair } =
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
            |> Pools.toListSinglePair time pair
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