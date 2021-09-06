module Pages.LendDashboard.Main exposing (Msg, Page, fromFragment, getFilter, init, update, view)

import Data.Chain exposing (Chain(..))
import Data.Device as Device exposing (Device)
import Data.Images exposing (Images)
import Data.Pair as Pair exposing (Pair)
import Data.Pool as Pool exposing (Pool)
import Data.Pools as Pools exposing (Pools)
import Data.Positions as Positions exposing (ActiveClaimInfo, Claim, MaturedClaimInfo, Positions, Return)
import Data.Remote exposing (Remote(..))
import Data.Token as Token
import Data.TokenImages exposing (TokenImages)
import Data.Tokens exposing (Tokens)
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
        , inFront
        , link
        , mouseDown
        , mouseOver
        , moveLeft
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
import Sort.Set as Set exposing (Set)
import Time exposing (Posix)
import Utility.Color as Color
import Utility.Glass as Glass
import Utility.Image as Image
import Utility.Loading as Loading
import Utility.PairInfo as PairInfo
import Utility.PositionsInfo as PositionsInfo
import Utility.Router as Router


type Page
    = Page
        { filter : Maybe Pair
        , expandedSet : Set Pool
        }


init :
    { model | time : Posix, user : Maybe { user | positions : Remote Positions } }
    -> Maybe Pair
    -> Page
init { time, user } filter =
    { filter = filter
    , expandedSet =
        user
            |> Maybe.andThen
                (\{ positions } ->
                    case positions of
                        Loading ->
                            Set.empty Pool.sorter |> Just

                        Failure ->
                            Set.empty Pool.sorter |> Just

                        Success successPositions ->
                            successPositions
                                |> Positions.getFirstClaim time filter
                                |> Maybe.map (Set.singleton Pool.sorter)
                )
            |> Maybe.withDefault (Set.empty Pool.sorter)
    }
        |> Page


fromFragment :
    { model
        | time : Posix
        , tokens : Tokens
        , pools : Pools
        , user : Maybe { user | positions : Remote Positions }
    }
    -> String
    -> Page
fromFragment ({ tokens, pools } as model) string =
    string
        |> Pools.fromPairFragment tokens pools
        |> init model


getFilter : Page -> Maybe Pair
getFilter (Page { filter }) =
    filter


type Msg
    = Expand Pool
    | Collapse Pool


update :
    { model | user : Maybe { user | positions : Remote Positions } }
    -> Msg
    -> Page
    -> Page
update { user } msg (Page page) =
    case msg of
        Expand pool ->
            user
                |> Maybe.map
                    (\{ positions } ->
                        case positions of
                            Success successPositions ->
                                successPositions
                                    |> Positions.toClaimPools
                                    |> (\list ->
                                            if list |> List.member pool then
                                                { page
                                                    | expandedSet =
                                                        page.expandedSet
                                                            |> Set.insert pool
                                                }
                                                    |> Page

                                            else
                                                Page page
                                       )

                            _ ->
                                Page page
                    )
                |> Maybe.withDefault (Page page)

        Collapse pool ->
            { page
                | expandedSet = page.expandedSet |> Set.remove pool
            }
                |> Page


view :
    { model
        | device : Device
        , time : Posix
        , images : Images
        , tokenImages : TokenImages
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
            { url = Router.toBorrowDashboard Nothing
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
    { model | time : Posix, images : Images, tokenImages : TokenImages }
    -> Page
    -> ( List ActiveClaimInfo, List MaturedClaimInfo )
    -> Element Msg
allPositions model ((Page { filter }) as page) ( activeList, maturedList ) =
    Keyed.column
        [ width fill
        , height shrink
        , spacing 18
        ]
        ([ maturedList
            |> List.filter
                (\{ pool } ->
                    filter
                        |> Maybe.map ((==) pool.pair)
                        |> Maybe.withDefault True
                )
            |> List.map
                (\({ pool } as maturedClaimInfo) ->
                    ( pool |> Pool.toKey
                    , singleMaturedPosition model page maturedClaimInfo
                    )
                )
         , activeList
            |> List.filter
                (\{ pool } ->
                    filter
                        |> Maybe.map ((==) pool.pair)
                        |> Maybe.withDefault True
                )
            |> List.map
                (\({ pool } as activeClaimInfo) ->
                    ( pool |> Pool.toKey
                    , singleActivePosition model page activeClaimInfo
                    )
                )
         ]
            |> List.concat
        )


singleMaturedPosition :
    { model | time : Posix, images : Images, tokenImages : TokenImages }
    -> Page
    -> MaturedClaimInfo
    -> Element Msg
singleMaturedPosition ({ tokenImages } as model) ((Page { expandedSet }) as page) ({ pool, return } as maturedClaimInfo) =
    column
        [ width fill
        , height shrink
        ]
        [ el
            [ width fill
            , height shrink
            , (return
                |> Maybe.map (\_ -> claimButton maturedClaimInfo)
                |> Maybe.withDefault claimButtonOff
              )
                |> inFront
            ]
            (Input.button
                [ width fill
                , height shrink
                ]
                { onPress =
                    if pool |> Set.memberOf expandedSet then
                        Just (Collapse pool)

                    else
                        Just (Expand pool)
                , label =
                    row
                        ([ width fill
                         , height <| px 72
                         , paddingXY 24 0
                         , spacing 18
                         ]
                            ++ Glass.lightPrimary 1
                        )
                        [ PairInfo.icons tokenImages pool.pair
                        , PairInfo.symbols pool.pair
                        , PositionsInfo.duration model pool.maturity
                        , el
                            [ width <| px 110
                            , alignRight
                            ]
                            none
                        , discloser model page maturedClaimInfo
                        ]
                }
            )
        , if pool |> Set.memberOf expandedSet then
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
    { model | time : Posix, images : Images, tokenImages : TokenImages }
    -> Page
    -> ActiveClaimInfo
    -> Element Msg
singleActivePosition ({ tokenImages } as model) ((Page { expandedSet }) as page) ({ pool } as activeClaimInfo) =
    column
        [ width fill
        , height shrink
        ]
        [ el
            [ width fill
            , height shrink
            , lendMoreButton activeClaimInfo |> inFront
            , claimButtonOff |> inFront
            ]
            (Input.button
                [ width fill
                , height shrink
                ]
                { onPress =
                    if pool |> Set.memberOf expandedSet then
                        Just (Collapse pool)

                    else
                        Just (Expand pool)
                , label =
                    row
                        ([ width fill
                         , height <| px 72
                         , paddingXY 24 0
                         , spacing 18
                         ]
                            ++ Glass.lightPrimary 1
                        )
                        [ PairInfo.icons tokenImages pool.pair
                        , PairInfo.symbols pool.pair
                        , PositionsInfo.duration model pool.maturity
                        , el
                            [ width <| px 238
                            , alignRight
                            ]
                            none
                        , discloser model page activeClaimInfo
                        ]
                }
            )
        , if pool |> Set.memberOf expandedSet then
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


claimButton : { maturedClaimInfo | pool : Pool } -> Element msg
claimButton { pool } =
    link
        [ width <| px 110
        , height <| px 44
        , alignRight
        , centerY
        , moveLeft 54
        , Background.color Color.primary100
        , Border.rounded 4
        , mouseDown [ Background.color Color.primary400 ]
        , mouseOver [ Background.color Color.primary300 ]
        ]
        { url = Router.toWithdraw pool
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


lendMoreButton : { activeClaimInfo | pool : Pool } -> Element msg
lendMoreButton { pool } =
    link
        [ width <| px 110
        , height <| px 44
        , alignRight
        , centerY
        , moveLeft 182
        , Border.width 1
        , Border.color Color.primary100
        , Border.rounded 4
        , mouseDown
            [ Background.color Color.primary300
            , Border.color Color.primary300
            ]
        , mouseOver [ Background.color Color.primary100 ]
        ]
        { url = Router.toLend pool
        , label =
            el
                [ width shrink
                , height shrink
                , centerX
                , centerY
                , Font.bold
                , Font.size 16
                , Font.color Color.light100
                ]
                (text "Lend more")
        }


claimButtonOff : Element msg
claimButtonOff =
    el
        [ width <| px 110
        , height <| px 44
        , alignRight
        , centerY
        , moveLeft 54
        , Background.color Color.transparent100
        , Border.rounded 4
        ]
        (el
            [ width shrink
            , height shrink
            , centerX
            , centerY
            , Font.bold
            , Font.size 16
            , Font.color Color.transparent100
            ]
            (text "Claim")
        )


discloser :
    { model | images : Images }
    -> Page
    -> { positionsInfo | pool : Pool }
    -> Element Msg
discloser { images } (Page { expandedSet }) { pool } =
    Image.discloser images
        [ width <| px 12
        , alignRight
        , centerY
        , if pool |> Set.memberOf expandedSet then
            degrees 180 |> rotate

          else
            degrees 0 |> rotate
        ]


maturedBalances :
    { maturedClaimInfo | pool : { pool | pair : Pair }, return : Maybe Return }
    -> Element msg
maturedBalances { pool, return } =
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
                            (pool.pair
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
                            (pool.pair
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


activeBalances :
    { activeClaimInfo | pool : { pool | pair : Pair }, claim : Claim }
    -> Element msg
activeBalances { pool, claim } =
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
                (pool.pair
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
                (pool.pair
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
