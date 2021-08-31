module Pages.LendDashboard.Main exposing (Msg, Page, init, update, view)

import Data.Chain exposing (Chain(..))
import Data.Device as Device exposing (Device)
import Data.Maturity as Maturity exposing (Maturity)
import Data.Pair as Pair exposing (Pair)
import Data.Positions as Positions exposing (ActiveClaimInfo, Claim, MaturedClaimInfo, Positions, Return)
import Data.Remote exposing (Remote(..))
import Data.Token as Token
import Element
    exposing
        ( Element
        , alignLeft
        , alignRight
        , alignTop
        , centerX
        , centerY
        , column
        , el
        , fill
        , height
        , link
        , mouseDown
        , mouseOver
        , none
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
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Keyed as Keyed
import Sort
import Sort.Set as Set exposing (Set)
import Time exposing (Posix)
import User
import Utility.Color as Color
import Utility.Glass as Glass
import Utility.Image as Image
import Utility.Loading as Loading
import Utility.PairInfo as PairInfo
import Utility.PositionsInfo as PositionsInfo
import Utility.Router as Router


type Page
    = Page (Set ( Pair, Maturity ))


init : { model | user : Maybe { user | chain : Chain } } -> Page
init { user } =
    user
        |> User.toChain
        |> (\chain ->
                Pair.sorter chain
                    |> Sort.by Tuple.first
                    |> Sort.tiebreaker
                        (Maturity.sorter |> Sort.by Tuple.second)
                    |> Set.empty
                    |> Page
           )


type Msg
    = Expand Pair Maturity
    | Collapse Pair Maturity


update :
    { model | user : Maybe { user | positions : Remote Positions } }
    -> Msg
    -> Page
    -> Page
update { user } msg (Page set) =
    case msg of
        Expand pair maturity ->
            user
                |> Maybe.map
                    (\{ positions } ->
                        case positions of
                            Success successPositions ->
                                successPositions
                                    |> Positions.toClaimPools
                                    |> (\list ->
                                            if list |> List.member ( pair, maturity ) then
                                                set
                                                    |> Set.insert ( pair, maturity )
                                                    |> Page

                                            else
                                                Page set
                                       )

                            _ ->
                                Page set
                    )
                |> Maybe.withDefault (Page set)

        Collapse pair maturity ->
            set |> Set.remove ( pair, maturity ) |> Page


view :
    { model
        | device : Device
        , time : Posix
        , user : Maybe { user | positions : Remote Positions }
    }
    -> Page
    -> Element Msg
view ({ device, time, user } as model) page =
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
        , user
            |> Maybe.map
                (\{ positions } ->
                    case positions of
                        Loading ->
                            PositionsInfo.loading

                        Failure ->
                            none

                        -- fix
                        Success successPositions ->
                            if successPositions |> Positions.isClaimEmpty then
                                PositionsInfo.noLendPosition model

                            else
                                successPositions
                                    |> Positions.toClaimList time
                                    |> allPositions model page
                )
            |> Maybe.withDefault (PositionsInfo.noUser model)
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
            { url = Router.toBorrowDashboard
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


allPositions :
    { model | time : Posix }
    -> Page
    -> ( List ActiveClaimInfo, List MaturedClaimInfo )
    -> Element Msg
allPositions model page ( activeList, maturedList ) =
    Keyed.column
        [ width fill
        , height shrink
        , spacing 18
        ]
        ([ maturedList
            |> List.map
                (\({ pair, maturity } as maturedClaimInfo) ->
                    ( [ pair |> Pair.toKey
                      , maturity |> Maturity.toKey
                      ]
                        |> String.join " "
                    , singleMaturedPosition model page maturedClaimInfo
                    )
                )
         , activeList
            |> List.map
                (\({ pair, maturity } as activeClaimInfo) ->
                    ( [ pair |> Pair.toKey
                      , maturity |> Maturity.toKey
                      ]
                        |> String.join " "
                    , singleActivePosition model page activeClaimInfo
                    )
                )
         ]
            |> List.concat
        )


singleMaturedPosition :
    { model | time : Posix }
    -> Page
    -> MaturedClaimInfo
    -> Element Msg
singleMaturedPosition model ((Page set) as page) ({ pair, maturity } as maturedClaimInfo) =
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
            [ PairInfo.icons pair
            , PairInfo.symbols pair
            , PositionsInfo.duration model maturedClaimInfo
            , maturedButtons maturedClaimInfo
            , discloser page maturedClaimInfo
            ]
        , if ( pair, maturity ) |> Set.memberOf set then
            el
                [ width fill
                , height shrink
                , padding 20
                , Border.width 1
                , Border.color Color.transparent100
                ]
                (maturedBalances maturedClaimInfo)

          else
            none
        ]


singleActivePosition :
    { model | time : Posix }
    -> Page
    -> ActiveClaimInfo
    -> Element Msg
singleActivePosition model ((Page set) as page) ({ pair, maturity } as activeClaimInfo) =
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
            [ PairInfo.icons pair
            , PairInfo.symbols pair
            , PositionsInfo.duration model activeClaimInfo
            , activeButtons activeClaimInfo
            , discloser page activeClaimInfo
            ]
        , if ( pair, maturity ) |> Set.memberOf set then
            el
                [ width fill
                , height shrink
                , padding 20
                , Border.width 1
                , Border.color Color.transparent100
                ]
                (activeBalances activeClaimInfo)

          else
            none
        ]


maturedButtons : { maturedClaimInfo | pair : Pair, maturity : Maturity } -> Element msg
maturedButtons { pair, maturity } =
    link
        [ width <| px 110
        , height <| px 44
        , Background.color Color.primary100
        , Border.rounded 4
        , mouseDown [ Background.color Color.primary400 ]
        , mouseOver [ Background.color Color.primary300 ]
        ]
        { url = Router.toWithdraw pair maturity
        , label =
            el
                [ width shrink
                , height shrink
                , centerX
                , centerY
                , Font.size 16
                , Font.bold
                , Font.color Color.primary500
                ]
                (text "Claim")
        }


activeButtons : { activeClaimInfo | pair : Pair, maturity : Maturity } -> Element msg
activeButtons { pair, maturity } =
    row
        [ width shrink
        , height shrink
        , spacing 18
        , Font.size 16
        ]
        [ link
            [ width <| px 110
            , height <| px 44
            , Border.width 1
            , Border.color Color.primary100
            , Border.rounded 4
            , mouseDown
                [ Background.color Color.primary300
                , Border.color Color.primary300
                ]
            , mouseOver [ Background.color Color.primary100 ]
            ]
            { url = Router.toLend pair maturity
            , label =
                el
                    [ width shrink
                    , height shrink
                    , centerX
                    , centerY
                    , Font.bold
                    , Font.color Color.light100
                    ]
                    (text "Lend more")
            }
        , el
            [ width <| px 110
            , height <| px 44
            , Background.color Color.transparent100
            , Border.rounded 4
            ]
            (el
                [ width shrink
                , height shrink
                , centerX
                , centerY
                , Font.bold
                , Font.color Color.transparent100
                ]
                (text "Claim")
            )
        ]


discloser :
    Page
    -> { positionsInfo | pair : Pair, maturity : Maturity }
    -> Element Msg
discloser (Page set) { pair, maturity } =
    Input.button
        [ width shrink
        , height shrink
        , alignRight
        , centerY
        ]
        { onPress =
            if ( pair, maturity ) |> Set.memberOf set then
                Just (Collapse pair maturity)

            else
                Just (Expand pair maturity)
        , label =
            Image.discloser
                [ width <| px 12
                , if ( pair, maturity ) |> Set.memberOf set then
                    degrees 180 |> rotate

                  else
                    degrees 0 |> rotate
                ]
        }


maturedBalances : { maturedClaimInfo | pair : Pair, return : Maybe Return } -> Element msg
maturedBalances { pair, return } =
    row
        ([ width fill
         , height <| px 56
         , paddingXY 20 0
         , spacing 30
         ]
            ++ Glass.lightWhite 12
        )
        [ el
            [ width shrink
            , height shrink
            , alignLeft
            , centerY
            , Font.regular
            , Font.size 14
            , Font.color Color.transparent300
            ]
            (text "Amount to receive")
        , return
            |> Maybe.map
                (\{ asset, collateral } ->
                    row
                        [ width shrink
                        , height shrink
                        , alignLeft
                        , centerY
                        , spacing 4
                        , Font.size 18
                        ]
                        [ el
                            [ Font.bold
                            , Font.color Color.transparent500
                            ]
                            (text asset)
                        , el
                            [ Font.bold
                            , Font.color Color.transparent300
                            ]
                            (pair
                                |> Pair.toAsset
                                |> Token.toSymbol
                                |> text
                            )
                        , el
                            [ Font.bold
                            , Font.color Color.transparent500
                            ]
                            (text "+")
                        , el
                            [ Font.bold
                            , Font.color Color.transparent500
                            ]
                            (text collateral)
                        , el
                            [ Font.bold
                            , Font.color Color.transparent300
                            ]
                            (pair
                                |> Pair.toCollateral
                                |> Token.toSymbol
                                |> text
                            )
                        ]
                )
            |> Maybe.withDefault
                (el
                    [ width <| px 100
                    , height shrink
                    , alignLeft
                    , centerY
                    ]
                    Loading.view
                )
        ]


activeBalances : { activeClaimInfo | pair : Pair, claim : Claim } -> Element msg
activeBalances { pair, claim } =
    row
        ([ width fill
         , height <| px 56
         , paddingXY 20 0
         , spacing 30
         ]
            ++ Glass.lightWhite 12
        )
        [ el
            [ width shrink
            , height shrink
            , alignLeft
            , centerY
            , Font.regular
            , Font.size 14
            , Font.color Color.transparent300
            ]
            (text "Amount at maturity")
        , row
            [ width shrink
            , height shrink
            , alignLeft
            , centerY
            , spacing 4
            , Font.size 18
            ]
            [ el
                [ Font.bold
                , Font.color Color.transparent500
                ]
                (text claim.bond)
            , el
                [ Font.bold
                , Font.color Color.transparent300
                ]
                (pair
                    |> Pair.toAsset
                    |> Token.toSymbol
                    |> text
                )
            ]
        , line
        , el
            [ width shrink
            , height shrink
            , alignLeft
            , centerY
            , Font.regular
            , Font.size 14
            , Font.color Color.transparent300
            ]
            (text "Insurance in case of default")
        , row
            [ width shrink
            , height shrink
            , alignLeft
            , centerY
            , spacing 4
            , Font.size 18
            ]
            [ el
                [ Font.bold
                , Font.color Color.transparent500
                ]
                (text claim.insurance)
            , el
                [ Font.bold
                , Font.color Color.transparent300
                ]
                (pair
                    |> Pair.toCollateral
                    |> Token.toSymbol
                    |> text
                )
            ]
        ]


line : Element msg
line =
    el
        [ width shrink
        , height <| px 32
        , alignLeft
        , centerY
        , Border.solid
        , Border.widthEach
            { top = 0
            , right = 1
            , bottom = 0
            , left = 0
            }
        , Border.color Color.transparent100
        ]
        none
