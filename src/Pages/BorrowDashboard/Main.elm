module Pages.BorrowDashboard.Main exposing (Msg, Page, fromFragment, getFilter, init, update, view)

import Data.Chain exposing (Chain(..))
import Data.Device as Device exposing (Device)
import Data.Images exposing (Images)
import Data.Pair as Pair exposing (Pair)
import Data.Pool as Pool exposing (Pool)
import Data.Pools as Pools exposing (Pools)
import Data.Positions as Positions exposing (DueInfo, DuesInfo, Positions)
import Data.Remote exposing (Remote(..))
import Data.Token as Token
import Data.TokenId as TokenId exposing (TokenId)
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
import Utility.PairInfo as PairInfo
import Utility.PositionsInfo as PositionsInfo
import Utility.Router as Router


type Page
    = Page
        { filter : Maybe Pair
        , expandedSet : Set Pool
        , chosenPool : Maybe Pool
        , chosenTokenIds : Set TokenId
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
                                |> Positions.getFirstDue time filter
                                |> Maybe.map (Set.singleton Pool.sorter)
                )
            |> Maybe.withDefault (Set.empty Pool.sorter)
    , chosenPool = Nothing
    , chosenTokenIds = Set.empty TokenId.sorter
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
    | Check Pool TokenId Bool


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
                                    |> Positions.toDuePools
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
            page.chosenPool
                |> Maybe.map
                    (\chosenPool ->
                        if pool == chosenPool then
                            { page
                                | expandedSet =
                                    page.expandedSet
                                        |> Set.remove pool
                                , chosenPool = Nothing
                                , chosenTokenIds = Set.empty TokenId.sorter
                            }
                                |> Page

                        else
                            { page
                                | expandedSet =
                                    page.expandedSet
                                        |> Set.remove pool
                            }
                                |> Page
                    )
                |> Maybe.withDefault
                    ({ page
                        | expandedSet =
                            page.expandedSet
                                |> Set.remove pool
                     }
                        |> Page
                    )

        Check pool tokenId checked ->
            if pool |> Set.memberOf page.expandedSet then
                if checked then
                    page.chosenPool
                        |> Maybe.map
                            (\chosenPool ->
                                if pool == chosenPool then
                                    { page
                                        | chosenTokenIds =
                                            page.chosenTokenIds
                                                |> Set.insert tokenId
                                    }
                                        |> Page

                                else
                                    { page
                                        | chosenPool = Just pool
                                        , chosenTokenIds = Set.singleton TokenId.sorter tokenId
                                    }
                                        |> Page
                            )
                        |> Maybe.withDefault
                            ({ page
                                | chosenPool = Just pool
                                , chosenTokenIds = Set.singleton TokenId.sorter tokenId
                             }
                                |> Page
                            )

                else
                    page.chosenPool
                        |> Maybe.map
                            (\chosenPool ->
                                if pool == chosenPool then
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
                        |> Maybe.withDefault (Page page)

            else
                Page page


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
                            if successPositions |> Positions.isDuesEmpty then
                                PositionsInfo.noBorrowPosition model

                            else
                                successPositions
                                    |> Positions.toDueList time
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
        [ link
            [ width <| px 95
            , height <| px 44
            ]
            { url = Router.toLendDashboard Nothing
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


allPositions :
    { model | time : Posix, images : Images, tokenImages : TokenImages }
    -> Page
    -> ( List DuesInfo, List DuesInfo )
    -> Element Msg
allPositions model page ( activeList, maturedList ) =
    Keyed.column
        [ width fill
        , height shrink
        , spacing 18
        ]
        ([ activeList
            |> List.map
                (\({ pool } as duesInfo) ->
                    ( pool |> Pool.toKey
                    , singleActivePosition model page duesInfo
                    )
                )
         , maturedList
            |> List.map
                (\({ pool } as duesInfo) ->
                    ( pool |> Pool.toKey
                    , singleMaturedPosition model page duesInfo
                    )
                )
         ]
            |> List.concat
        )


singleActivePosition :
    { model | time : Posix, images : Images, tokenImages : TokenImages }
    -> Page
    -> DuesInfo
    -> Element Msg
singleActivePosition ({ tokenImages } as model) ((Page { expandedSet }) as page) ({ pool, listDue } as duesInfo) =
    column
        [ width fill
        , height shrink
        ]
        [ el
            [ width fill
            , height shrink
            , borrowMoreButton duesInfo |> inFront
            , repayButton page duesInfo |> inFront
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
                            [ width <| px 298
                            , alignRight
                            ]
                            none
                        , discloser model page duesInfo
                        ]
                }
            )
        , if pool |> Set.memberOf expandedSet then
            column
                [ width fill
                , height shrink
                , padding 20
                , spacing 16
                , Border.width 1
                , Border.color Color.transparent100
                ]
                (listDue
                    |> List.map (activeBalances model page duesInfo)
                )

          else
            none
        ]


singleMaturedPosition :
    { model | time : Posix, images : Images, tokenImages : TokenImages }
    -> Page
    -> DuesInfo
    -> Element Msg
singleMaturedPosition ({ tokenImages } as model) ((Page { expandedSet }) as page) ({ pool, listDue } as duesInfo) =
    column
        [ width fill
        , height shrink
        ]
        [ Input.button
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
                    , discloser model page duesInfo
                    ]
            }
        , if pool |> Set.memberOf expandedSet then
            column
                [ width fill
                , height shrink
                , padding 20
                , spacing 16
                , Border.width 1
                , Border.color Color.transparent100
                ]
                (listDue
                    |> List.map (maturedBalances duesInfo)
                )

          else
            none
        ]


borrowMoreButton : { duesInfo | pool : Pool } -> Element msg
borrowMoreButton { pool } =
    link
        [ width <| px 140
        , height <| px 44
        , alignRight
        , centerY
        , moveLeft 212
        , Border.width 1
        , Border.color Color.primary100
        , Border.rounded 4
        , mouseDown
            [ Background.color Color.primary300
            , Border.color Color.primary300
            ]
        , mouseOver [ Background.color Color.primary100 ]
        ]
        { url = Router.toBorrow pool
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
                (text "Borrow more")
        }


repayButton :
    Page
    -> DuesInfo
    -> Element msg
repayButton (Page { chosenPool, chosenTokenIds }) { pool, listDue } =
    link
        [ width <| px 140
        , height <| px 44
        , alignRight
        , centerY
        , moveLeft 54
        , Background.color Color.primary100
        , Border.rounded 4
        , mouseDown [ Background.color Color.primary400 ]
        , mouseOver [ Background.color Color.primary300 ]
        ]
        { url =
            chosenPool
                |> Maybe.map
                    (\chosenPoolJust ->
                        if chosenPoolJust == pool then
                            Router.toPay pool chosenTokenIds

                        else
                            listDue
                                |> List.map .tokenId
                                |> Set.fromList TokenId.sorter
                                |> Router.toPay pool
                    )
                |> Maybe.withDefault
                    (listDue
                        |> List.map .tokenId
                        |> Set.fromList TokenId.sorter
                        |> Router.toPay pool
                    )
        , label =
            el
                [ width shrink
                , height shrink
                , centerX
                , centerY
                , Font.bold
                , Font.size 16
                , Font.color Color.primary500
                ]
                ((chosenPool
                    |> Maybe.map
                        (\chosenPoolJust ->
                            if chosenPoolJust == pool then
                                "Repay selected"

                            else
                                "Repay all"
                        )
                    |> Maybe.withDefault "Repay all"
                 )
                    |> text
                )
        }


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


activeBalances :
    { model | images : Images }
    -> Page
    -> { duesInfo | pool : Pool }
    -> DueInfo
    -> Element Msg
activeBalances { images } (Page { chosenPool, chosenTokenIds }) { pool } { tokenId, debt, collateral } =
    row
        ([ width fill
         , height <| px 56
         , paddingXY 20 0
         , spacing 30
         ]
            ++ Glass.lightWhite 12
        )
        [ Input.checkbox
            [ width shrink
            , height shrink
            , alignLeft
            , centerY
            ]
            { onChange = Check pool tokenId
            , icon =
                \checked ->
                    (if checked then
                        Image.checkboxSelected images

                     else
                        Image.checkbox images
                    )
                        [ height <| px 24 ]
            , checked =
                chosenPool
                    |> Maybe.map
                        (\chosenPoolJust ->
                            if chosenPoolJust == pool then
                                tokenId |> Set.memberOf chosenTokenIds

                            else
                                False
                        )
                    |> Maybe.withDefault False
            , label = Input.labelHidden "Checkbox"
            }
        , el
            [ width shrink
            , height shrink
            , alignLeft
            , centerY
            , Font.regular
            , Font.size 14
            , Font.color Color.transparent300
            ]
            (text "Amount to repay")
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
                (text debt)
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
            (text "Collateral locked")
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
        ]


maturedBalances :
    { duesInfo | pool : { pool | pair : Pair } }
    -> DueInfo
    -> Element msg
maturedBalances { pool } { debt, collateral } =
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
            (text "Amount defaulted")
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
                (text debt)
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
            (text "Collateral forfeited")
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
