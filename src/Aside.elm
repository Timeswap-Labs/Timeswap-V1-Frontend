module Aside exposing (fromFragment, toUrl, view)

import Data.Chain exposing (Chain)
import Data.Device as Device exposing (Device)
import Data.Filter as Filter
import Data.Images exposing (Images)
import Data.Pair as Pair exposing (Pair)
import Data.Pools as Pools exposing (Pools)
import Data.Positions as Positions exposing (Positions)
import Data.Remote exposing (Remote(..))
import Data.TokenImages exposing (TokenImages)
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
        , images : Images
        , tokenImages : TokenImages
        , pools : Pools
        , user : Maybe { user | chain : Chain, positions : Remote Positions }
        , page : Page
    }
    -> Element msg
view ({ device } as model) =
    column
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
        [ title model
        , allPairs model
        , listPairs model
        ]
        |> (\element ->
                if device |> Device.isPhoneOrTablet then
                    el
                        [ width fill
                        , height fill
                        , scrollbarY
                        , Background.color Color.modal
                        , htmlAttribute <| Html.Attributes.id "outside-aside"
                        ]
                        element

                else
                    element
           )


title : { model | page : Page } -> Element msg
title { page } =
    el
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
        ((case page |> Page.toFilter of
            Filter.AllMarket ->
                "Trading pairs"

            Filter.PairMarket _ ->
                "Trading pairs"

            Filter.LendDashboard _ ->
                "Your lend positions by pair"

            Filter.BorrowDashboard _ ->
                "Your borrow positions by pair"

            Filter.LiquidityProvider ->
                "Coming soon"
         )
            |> text
        )


allPairs : { model | images : Images, page : Page } -> Element msg
allPairs { images, page } =
    row
        [ width fill
        , height <| px 48
        , paddingXY 37 0
        , spacing 14
        ]
        [ Image.allPairs images
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
                case page |> Page.toFilter of
                    Filter.AllMarket ->
                        Keyed.el
                            [ width fill
                            , height shrink
                            , Background.color Color.primary100
                            ]
                            ( "allPairs", element )

                    Filter.PairMarket _ ->
                        Keyed.el
                            [ width fill
                            , height shrink
                            ]
                            ( "allPairs"
                            , link
                                [ width fill
                                , height shrink
                                ]
                                { url = Router.toAllMarket
                                , label = element
                                }
                            )

                    Filter.LendDashboard Nothing ->
                        Keyed.el
                            [ width fill
                            , height shrink
                            , Background.color Color.primary100
                            ]
                            ( "allPairs", element )

                    Filter.LendDashboard (Just _) ->
                        Keyed.el
                            [ width fill
                            , height shrink
                            ]
                            ( "allPairs"
                            , link
                                [ width fill
                                , height shrink
                                ]
                                { url = Router.toLendDashboard Nothing
                                , label = element
                                }
                            )

                    Filter.BorrowDashboard Nothing ->
                        Keyed.el
                            [ width fill
                            , height shrink
                            , Background.color Color.primary100
                            ]
                            ( "allPairs", element )

                    Filter.BorrowDashboard (Just _) ->
                        Keyed.el
                            [ width fill
                            , height shrink
                            ]
                            ( "allPairs"
                            , link
                                [ width fill
                                , height shrink
                                ]
                                { url = Router.toBorrowDashboard Nothing
                                , label = element
                                }
                            )

                    Filter.LiquidityProvider ->
                        Keyed.el
                            [ width fill
                            , height shrink
                            , Background.color Color.primary100
                            ]
                            ( "allPairs", element )
           )


listPairs :
    { model
        | time : Posix
        , tokenImages : TokenImages
        , pools : Pools
        , user : Maybe { user | positions : Remote Positions }
        , page : Page
    }
    -> Element msg
listPairs ({ time, pools, user, page } as model) =
    Keyed.column
        [ width fill
        , height fill
        ]
        (case page |> Page.toFilter of
            Filter.AllMarket ->
                pools
                    |> Pools.toList time
                    |> List.map
                        (\( pair, list ) ->
                            ( pair |> Pair.toKey
                            , ( pair
                              , list |> List.length
                              )
                                |> singlePair model
                            )
                        )

            Filter.PairMarket _ ->
                pools
                    |> Pools.toList time
                    |> List.map
                        (\( pair, list ) ->
                            ( pair |> Pair.toKey
                            , ( pair
                              , list |> List.length
                              )
                                |> singlePair model
                            )
                        )

            Filter.LendDashboard _ ->
                user
                    |> Maybe.map
                        (\{ positions } ->
                            case positions of
                                Success successPositions ->
                                    successPositions
                                        |> Positions.toClaimListByPair time
                                        |> List.map
                                            (\( pair, size ) ->
                                                ( pair |> Pair.toKey
                                                , ( pair
                                                  , size
                                                  )
                                                    |> singlePair model
                                                )
                                            )

                                _ ->
                                    []
                        )
                    |> Maybe.withDefault []

            Filter.BorrowDashboard _ ->
                user
                    |> Maybe.map
                        (\{ positions } ->
                            case positions of
                                Success successPositions ->
                                    successPositions
                                        |> Positions.toDueListByPair time
                                        |> List.map
                                            (\( pair, size ) ->
                                                ( pair |> Pair.toKey
                                                , ( pair
                                                  , size
                                                  )
                                                    |> singlePair model
                                                )
                                            )

                                _ ->
                                    []
                        )
                    |> Maybe.withDefault []

            Filter.LiquidityProvider ->
                []
        )


singlePair : { model | tokenImages : TokenImages, page : Page } -> ( Pair, Int ) -> Element msg
singlePair { page, tokenImages } ( pair, size ) =
    row
        [ width fill
        , height <| px 48
        , paddingXY 37 0
        , spacing 8
        ]
        [ PairInfo.iconsAside tokenImages pair
        , PairInfo.symbolsAside pair
        , pairSize size
        ]
        |> (\element ->
                case page |> Page.toFilter of
                    Filter.AllMarket ->
                        link
                            [ width fill
                            , height shrink
                            ]
                            { url = pair |> Router.toPairMarket
                            , label = element
                            }

                    Filter.PairMarket chosenPair ->
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

                    Filter.LendDashboard filter ->
                        filter
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
                                            { url = Just pair |> Router.toLendDashboard
                                            , label = element
                                            }
                                )
                            |> Maybe.withDefault
                                (link
                                    [ width fill
                                    , height shrink
                                    ]
                                    { url = Just pair |> Router.toLendDashboard
                                    , label = element
                                    }
                                )

                    Filter.BorrowDashboard filter ->
                        filter
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
                                            { url = Just pair |> Router.toBorrowDashboard
                                            , label = element
                                            }
                                )
                            |> Maybe.withDefault
                                (link
                                    [ width fill
                                    , height shrink
                                    ]
                                    { url = Just pair |> Router.toBorrowDashboard
                                    , label = element
                                    }
                                )

                    Filter.LiquidityProvider ->
                        element
           )


pairSize : Int -> Element msg
pairSize size =
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
        (size
            |> String.fromInt
            |> text
        )
