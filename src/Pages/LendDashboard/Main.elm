module Pages.LendDashboard.Main exposing (Msg, Page, init, toUrl, update, view)

import Data.Chain exposing (Chain(..))
import Data.Device as Device exposing (Device)
import Data.Pair as Pair exposing (Pair)
import Element
    exposing
        ( Element
        , alignLeft
        , alignTop
        , centerX
        , centerY
        , column
        , el
        , fill
        , height
        , link
        , padding
        , paddingXY
        , px
        , row
        , shrink
        , spacing
        , text
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Sort.Set as Set exposing (Set)
import User
import Utility.Color as Color
import Utility.PairInfo as PairInfo


type Page
    = Page (Set Pair)


init : { model | user : Maybe { user | chain : Chain } } -> Page
init { user } =
    user
        |> User.toChain
        |> (\chain ->
                Pair.sorter chain
                    |> Set.empty
                    |> Page
           )


toUrl : String
toUrl =
    "#dashboard?transaction=lend"


type Msg
    = Msg


update : Msg -> Page -> Page
update msg page =
    page


view : { model | device : Device } -> Page -> Element Msg
view { device } (Page set) =
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
        , spacing 16
        , alignTop
        , centerX
        ]
        [ switch
        , title
        ]


switch : Element msg
switch =
    row
        [ width shrink
        , height shrink
        , alignLeft
        , padding 4
        , Background.color Color.primary100
        , Border.rounded 4
        , Font.size 16
        ]
        [ el
            [ width <| px 95
            , height <| px 44
            , Background.color Color.primary500
            , Border.rounded 4
            ]
            (el
                [ width shrink
                , height shrink
                , centerX
                , centerY
                , Font.bold
                , Font.color Color.transparent500
                ]
                (text "Lend")
            )
        , link
            [ width <| px 95
            , height <| px 44
            ]
            { url = ""
            , label =
                el
                    [ width shrink
                    , height shrink
                    , centerX
                    , centerY
                    , Font.bold
                    , Font.color Color.transparent300
                    ]
                    (text "Borrow")
            }
        ]


title : Element msg
title =
    el
        [ paddingXY 0 4
        , Font.regular
        , Font.size 16
        , Font.color Color.transparent300
        ]
        (text "Your lend positions")


singlePosition { pair } =
    column
        [ width fill
        , height shrink
        ]
        [ Input.button
            [ width fill
            , height shrink
            ]
            { onPress = Nothing
            , label =
                row
                    [ width fill
                    , height <| px 72
                    , paddingXY 24 0
                    , spacing 18
                    ]
                    [ PairInfo.icons { iconSize = 32 } pair
                    , PairInfo.symbols { fontSize = 16, isBold = True } pair
                    ]
            }
        ]
