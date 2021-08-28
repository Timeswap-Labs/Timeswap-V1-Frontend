module Pages.AllMarket.Main exposing (Msg, Page, init, toUrl, update)

import Data.Chain exposing (Chain(..))
import Data.Device as Device exposing (Device)
import Data.Pair as Pair exposing (Pair)
import Data.Pools as Pools exposing (PoolInfo, Pools)
import Element
    exposing
        ( Element
        , alignLeft
        , alignRight
        , centerX
        , centerY
        , column
        , el
        , fill
        , height
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
import Element.Font as Font
import Element.Input as Input
import Sort.Set as Set exposing (Set)
import Utility.Color as Color


type Page
    = Page (Set Pair)


init : { model | pools : Pools, user : Maybe { user | chain : Chain } } -> Page
init { pools, user } =
    user
        |> Maybe.map .chain
        |> Maybe.withDefault Rinkeby
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


update : Msg -> Page -> ( Page, Cmd Msg )
update msg (Page set) =
    case msg of
        Expand pair ->
            ( set |> Set.insert pair |> Page
            , Cmd.none
            )

        Collapse pair ->
            ( set |> Set.remove pair |> Page
            , Cmd.none
            )


view : { model | device : Device } -> Element Msg
view ({ device } as model) =
    column
        ((if Device.isPhone device then
            [ width <| px 335
            , paddingXY 0 29
            ]

          else if Device.isTablet device then
            [ width <| px 552
            , paddingXY 0 29
            ]

          else
            [ width <| px 1068
            , paddingXY 0 38
            ]
         )
            ++ [ height shrink
               , spacing 30
               , centerX
               ]
        )
        [ title model
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
