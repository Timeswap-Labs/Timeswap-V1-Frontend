module Pages.BorrowDashboard.Main exposing (Msg, Page, init, update, view)

import Data.Chain exposing (Chain(..))
import Data.Device as Device exposing (Device)
import Data.Maturity as Maturity exposing (Maturity)
import Data.Pair as Pair exposing (Pair)
import Data.Positions as Positions exposing (Positions)
import Data.Remote exposing (Remote(..))
import Data.TokenId as TokenId exposing (TokenId)
import Element
    exposing
        ( Element
        , alignLeft
        , alignTop
        , centerX
        , centerY
        , column
        , el
        , height
        , link
        , none
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
import Sort
import Sort.Set as Set exposing (Set)
import Time exposing (Posix)
import User
import Utility.Color as Color
import Utility.PositionsInfo as PositionsInfo
import Utility.Router as Router


type Page
    = Page
        { expandedSet : Set ( Pair, Maturity )
        , chosenPool : Maybe ( Pair, Maturity )
        , chosenTokenIds : Set TokenId
        }


init : { model | user : Maybe { user | chain : Chain } } -> Page
init { user } =
    user
        |> User.toChain
        |> (\chain ->
                { expandedSet =
                    Pair.sorter chain
                        |> Sort.by Tuple.first
                        |> Sort.tiebreaker
                            (Maturity.sorter |> Sort.by Tuple.second)
                        |> Set.empty
                , chosenPool = Nothing
                , chosenTokenIds = Set.empty TokenId.sorter
                }
                    |> Page
           )


type Msg
    = Expand Pair Maturity
    | Collapse Pair Maturity
    | Check Pair Maturity TokenId
    | Uncheck Pair Maturity TokenId


update :
    { model | user : Maybe { user | positions : Remote Positions } }
    -> Msg
    -> Page
    -> Page
update { user } msg (Page page) =
    case msg of
        Expand pair maturity ->
            user
                |> Maybe.map
                    (\{ positions } ->
                        case positions of
                            Success successPositions ->
                                successPositions
                                    |> Positions.toDuePools
                                    |> (\list ->
                                            if list |> List.member ( pair, maturity ) then
                                                { page
                                                    | expandedSet =
                                                        page.expandedSet
                                                            |> Set.insert ( pair, maturity )
                                                }
                                                    |> Page

                                            else
                                                Page page
                                       )

                            _ ->
                                Page page
                    )
                |> Maybe.withDefault (Page page)

        Collapse pair maturity ->
            page.chosenPool
                |> Maybe.map
                    (\( chosenPair, chosenMaturity ) ->
                        if pair == chosenPair && maturity == chosenMaturity then
                            { expandedSet =
                                page.expandedSet
                                    |> Set.remove ( pair, maturity )
                            , chosenPool = Nothing
                            , chosenTokenIds = Set.empty TokenId.sorter
                            }
                                |> Page

                        else
                            { page
                                | expandedSet =
                                    page.expandedSet
                                        |> Set.remove ( pair, maturity )
                            }
                                |> Page
                    )
                |> Maybe.withDefault
                    ({ page
                        | expandedSet =
                            page.expandedSet
                                |> Set.remove ( pair, maturity )
                     }
                        |> Page
                    )

        Check pair maturity tokenId ->
            if ( pair, maturity ) |> Set.memberOf page.expandedSet then
                page.chosenPool
                    |> Maybe.map
                        (\( chosenPair, chosenMaturity ) ->
                            if pair == chosenPair && maturity == chosenMaturity then
                                { page
                                    | chosenTokenIds =
                                        page.chosenTokenIds
                                            |> Set.insert tokenId
                                }
                                    |> Page

                            else
                                { page
                                    | chosenPool = Just ( pair, maturity )
                                    , chosenTokenIds = Set.singleton TokenId.sorter tokenId
                                }
                                    |> Page
                        )
                    |> Maybe.withDefault
                        ({ page
                            | chosenPool = Just ( pair, maturity )
                            , chosenTokenIds = Set.singleton TokenId.sorter tokenId
                         }
                            |> Page
                        )

            else
                Page page

        Uncheck pair maturity tokenId ->
            if ( pair, maturity ) |> Set.memberOf page.expandedSet then
                page.chosenPool
                    |> Maybe.map
                        (\( chosenPair, chosenMaturity ) ->
                            if pair == chosenPair && maturity == chosenMaturity then
                                page.chosenTokenIds
                                    |> Set.remove tokenId
                                    |> (\set ->
                                            { page
                                                | chosenPool =
                                                    if set |> Set.isEmpty then
                                                        Nothing

                                                    else
                                                        page.chosenPool
                                                , chosenTokenIds = set
                                            }
                                                |> Page
                                       )

                            else
                                Page page
                        )
                    |> Maybe.withDefault
                        (Page page)

            else
                Page page


view :
    { model
        | device : Device
        , time : Posix
        , user : Maybe { user | positions : Remote Positions }
    }
    -> Page
    -> Element Msg
view ({ device, user } as model) (Page set) =
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

        -- , user
        --     |> Maybe.map
        --         (\{ positions } ->
        --             case positions of
        --                 Loading ->
        --                     PositionsInfo.loading
        --                 Failure ->
        --                     none
        --                 -- fix
        --                 Success successPositions ->
        --                     successPositions
        --                         |> Positions.toClaimList
        --                         |> (\list ->
        --                                 if list |> List.isEmpty then
        --                                     PositionsInfo.noLendPosition model
        --                                 else
        --                                     none
        --                             -- allPositions model set list
        --                            )
        --         )
        --     |> Maybe.withDefault (PositionsInfo.noUser model)
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
        [ link
            [ width <| px 95
            , height <| px 44
            ]
            { url = Router.toLendDashboard
            , label =
                el
                    [ width shrink
                    , height shrink
                    , centerX
                    , centerY
                    , Font.bold
                    , Font.color Color.transparent300
                    ]
                    (text "Lend")
            }
        , el
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
                (text "Borrow")
            )
        ]


title : Element msg
title =
    el
        [ paddingXY 0 4
        , Font.regular
        , Font.size 16
        , Font.color Color.transparent300
        ]
        (text "Your borrow positions")
