module Aside exposing (fromFragment, toUrl, view)

import Data.Address as Address
import Data.Chain exposing (Chain)
import Data.Device as Device exposing (Device)
import Data.Pair as Pair exposing (Pair)
import Data.Pools as Pools exposing (Pools)
import Element
    exposing
        ( Element
        , alignLeft
        , alignRight
        , centerY
        , column
        , el
        , fill
        , height
        , htmlAttribute
        , link
        , paddingEach
        , paddingXY
        , px
        , row
        , scrollbarY
        , shrink
        , spacing
        , text
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Keyed as Keyed
import Element.Region as Region
import Html.Attributes
import Page exposing (Page)
import Time exposing (Posix)
import Utility.Color as Color
import Utility.Image as Image
import Utility.PairInfo as PairInfo
import Utility.Router as Router


fromFragment : { model | device : Device } -> String -> Maybe ()
fromFragment { device } string =
    case string of
        "aside" ->
            if device |> Device.isPhoneOrTablet then
                Just ()

            else
                Nothing

        _ ->
            Nothing


toUrl : String
toUrl =
    "#aside"


view :
    { model
        | device : Device
        , time : Posix
        , pools : Pools
        , user : Maybe { user | chain : Chain }
        , page : Page
    }
    -> Element msg
view ({ device } as model) =
    (if device |> Device.isPhoneOrTablet then
        el
            [ width fill
            , height fill
            , scrollbarY
            , Background.color Color.modal
            , htmlAttribute <| Html.Attributes.id "outside-aside"
            ]

     else
        identity
    )
        (column
            ([ Region.aside
             , width <| px 278
             , height fill
             , alignLeft
             , scrollbarY
             , Border.solid
             , Border.widthEach
                { top = 0
                , right = 1
                , bottom = 0
                , left = 0
                }
             , Border.color Color.transparent100
             ]
                ++ (if device |> Device.isPhoneOrTablet then
                        [ Background.color Color.dark300 ]

                    else
                        []
                   )
            )
            [ title
            , allPairs model
            , listPairs model
            ]
        )


title : Element msg
title =
    row
        [ width fill
        , height <| px 48
        , alignLeft
        , paddingEach
            { top = 20
            , bottom = 12
            , left = 32
            , right = 171
            }
        , Font.color Color.transparent200
        , Font.size 12
        , Font.regular
        ]
        [ text "Trading pairs" ]


allPairs : { model | page : Page } -> Element msg
allPairs { page } =
    row
        [ width fill
        , height <| px 48
        , paddingXY 37 0
        , spacing 14
        ]
        [ Image.allPairs
            [ alignLeft
            , centerY
            ]
        , el
            [ alignLeft
            , centerY
            , Font.color Color.transparent500
            , Font.size 14
            , Font.bold
            ]
            (text "View All Pairs")
        ]
        |> (\element ->
                page
                    |> Page.getPair
                    |> Maybe.map
                        (\_ ->
                            Keyed.el []
                                ( "allPairs"
                                , link
                                    [ width fill
                                    , height shrink
                                    ]
                                    { url = Router.toAllMarket
                                    , label = element
                                    }
                                )
                        )
                    |> Maybe.withDefault
                        (Keyed.el
                            [ width fill
                            , height shrink
                            , Background.color Color.primary100
                            ]
                            ( "allPairs", element )
                        )
           )


listPairs : { model | time : Posix, pools : Pools, page : Page } -> Element msg
listPairs ({ pools } as model) =
    Keyed.column
        [ width fill
        , height fill
        ]
        (pools
            |> Pools.toPairs
            |> List.map
                (\pair ->
                    ( pair |> Pair.toAddress |> Address.toString
                    , pair |> singlePair model
                    )
                )
        )


singlePair : { model | time : Posix, pools : Pools, page : Page } -> Pair -> Element msg
singlePair ({ page } as model) pair =
    row
        [ width fill
        , height <| px 48
        , paddingXY 37 0
        , spacing 8
        ]
        [ PairInfo.iconsAside pair
        , PairInfo.symbolsAside pair
        , size model pair
        ]
        |> (\element ->
                page
                    |> Page.getPair
                    |> Maybe.map
                        (\chosenPair ->
                            if chosenPair == pair then
                                el
                                    [ width fill
                                    , height shrink
                                    , Background.color Color.primary100
                                    ]
                                    element

                            else
                                link
                                    [ width fill
                                    , height shrink
                                    ]
                                    { url = pair |> Router.toPairMarket
                                    , label = element
                                    }
                        )
                    |> Maybe.withDefault
                        (link
                            [ width fill
                            , height shrink
                            ]
                            { url = pair |> Router.toPairMarket
                            , label = element
                            }
                        )
           )


size : { model | time : Posix, pools : Pools } -> Pair -> Element msg
size { time, pools } pair =
    el
        [ width shrink
        , height shrink
        , alignRight
        , centerY
        , Font.family [ Font.typeface "Supreme" ]
        , Font.regular
        , Font.size 14
        , Font.color Color.transparent200
        ]
        (pools
            |> Pools.getSize time pair
            |> String.fromInt
            |> text
        )
