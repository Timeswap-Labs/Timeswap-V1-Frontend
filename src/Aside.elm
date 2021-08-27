module Aside exposing (view)

import Data.Address as Address
import Data.Device as Device exposing (Device)
import Data.Pair as Pair exposing (Pair)
import Data.Pools as Pools exposing (Pools)
import Data.Token as Token
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
import Pages.AllMarket.Main as AllMarket
import Route
import Utility.Color as Color
import Utility.Image as Image
import Utility.TokenImage as TokenImage


view : { model | device : Device, pools : Pools, page : Page } -> Element msg
view ({ device } as model) =
    column
        ([ Region.aside
         , width <| px 278
         , height fill
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
                    [ htmlAttribute <| Html.Attributes.id "aside" ]

                else
                    []
               )
        )
        [ title
        , allPairs model
        , listPairs model
        ]


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
                case page of
                    Page.AllMarket _ ->
                        Keyed.el
                            [ width fill
                            , height shrink
                            , Background.color Color.primary100
                            ]
                            ( "allPairs", element )

                    Page.PairMarket _ ->
                        Keyed.el []
                            ( "allPairs"
                            , link
                                [ width fill
                                , height shrink
                                ]
                                { url = Page.AllMarket AllMarket.init |> Route.Page |> Route.toUrl
                                , label = element
                                }
                            )

                    _ ->
                        Keyed.el
                            [ width fill
                            , height shrink
                            , Background.color Color.primary100
                            ]
                            ( "allPairs", element )
           )


listPairs : { model | pools : Pools, page : Page } -> Element msg
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


singlePair : { model | pools : Pools, page : Page } -> Pair -> Element msg
singlePair ({ page } as model) pair =
    row
        [ width fill
        , height <| px 48
        , paddingXY 37 0
        , spacing 8
        ]
        [ icons pair
        , symbols pair
        , size model pair
        ]
        |> (\element ->
                case page of
                    Page.PairMarket chosenPair ->
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
                                { url = Page.PairMarket pair |> Route.Page |> Route.toUrl
                                , label = element
                                }

                    _ ->
                        link
                            [ width fill
                            , height shrink
                            ]
                            { url = Page.PairMarket pair |> Route.Page |> Route.toUrl
                            , label = element
                            }
           )


icons : Pair -> Element msg
icons pair =
    row
        [ width shrink
        , height shrink
        , alignLeft
        , centerY
        , spacing 4
        ]
        [ pair |> Pair.toAsset |> TokenImage.getIcon [ height <| px 24 ]
        , pair |> Pair.toCollateral |> TokenImage.getIcon [ height <| px 24 ]
        ]


symbols : Pair -> Element msg
symbols pair =
    el
        [ width shrink
        , height shrink
        , alignLeft
        , centerY
        , Font.regular
        , Font.size 14
        , Font.color Color.transparent500
        ]
        ([ pair |> Pair.toAsset |> Token.toSymbol -- short symbol
         , pair |> Pair.toCollateral |> Token.toSymbol -- short symbol
         ]
            |> String.join " - "
            |> text
        )


size : { model | pools : Pools } -> Pair -> Element msg
size { pools } pair =
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
            |> Pools.getSize pair
            |> String.fromInt
            |> text
        )
