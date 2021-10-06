module Pages.BorrowDashboard.Main exposing
    ( Msg
    , Page
    , fromFragment
    , getFilter
    , init
    , update
    , updateDashboard
    , view
    )

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
        , alignBottom
        , alignLeft
        , alignRight
        , alignTop
        , below
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
        , moveUp
        , newTabLink
        , none
        , onRight
        , padding
        , paddingEach
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
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Keyed as Keyed
import Pages.BorrowDashboard.Tooltip as Tooltip exposing (Tooltip)
import Sort.Set as Set exposing (Set)
import Time exposing (Posix)
import Utility.Color as Color
import Utility.Glass as Glass
import Utility.Image as Image
import Utility.OpenSea as OpenSea
import Utility.PairInfo as PairInfo
import Utility.PositionsInfo as PositionsInfo
import Utility.Router as Router
import Utility.Truncate as Truncate


type Page
    = Page
        { filter : Maybe Pair
        , expandedSet : Set Pool
        , chosenPool : Maybe Pool
        , chosenTokenIds : Set TokenId
        , tooltip : Maybe Tooltip
        }


init :
    { model | time : Posix, user : Remote error { user | positions : Remote () Positions } }
    -> Maybe Pair
    -> Page
init { time, user } filter =
    { filter = filter
    , expandedSet =
        case user of
            Success { positions } ->
                case positions of
                    Loading ->
                        Set.empty Pool.sorter

                    Failure _ ->
                        Set.empty Pool.sorter

                    Success successPositions ->
                        successPositions
                            |> Positions.getFirstDue time filter
                            |> Maybe.map (Set.singleton Pool.sorter)
                            |> Maybe.withDefault (Set.empty Pool.sorter)

            _ ->
                Set.empty Pool.sorter
    , chosenPool = Nothing
    , chosenTokenIds = Set.empty TokenId.sorter
    , tooltip = Nothing
    }
        |> Page


fromFragment :
    { model
        | time : Posix
        , tokens : Tokens
        , pools : Pools
        , user : Remote error { user | positions : Remote () Positions }
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
    | OnMouseEnter Tooltip
    | OnMouseLeave


update :
    { model | user : Remote error { user | positions : Remote () Positions } }
    -> Msg
    -> Page
    -> Page
update { user } msg (Page page) =
    case msg of
        Expand pool ->
            case user of
                Success { positions } ->
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

                _ ->
                    Page page

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

        OnMouseEnter tooltip ->
            { page | tooltip = Just tooltip }
                |> Page

        OnMouseLeave ->
            { page | tooltip = Nothing }
                |> Page


updateDashboard :
    Remote () Positions
    -> { model | time : Posix, user : Remote error { user | positions : Remote () Positions } }
    -> Page
    -> Page
updateDashboard oldPositions ({ time } as model) ((Page { filter }) as page) =
    (case oldPositions of
        Success successPositions ->
            successPositions
                |> Positions.getFirstDue time filter
                |> Maybe.map (\_ -> True)
                |> Maybe.withDefault False

        _ ->
            False
    )
        |> (\hasClaims ->
                if hasClaims then
                    page

                else
                    init model filter
           )


view :
    { model
        | device : Device
        , time : Posix
        , images : Images
        , tokenImages : TokenImages
        , pools : Pools
        , user : Remote userError { user | chain : Chain, positions : Remote () Positions }
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
        , case user of
            Success { chain, positions } ->
                case positions of
                    Loading ->
                        PositionsInfo.loading

                    Failure _ ->
                        none

                    Success successPositions ->
                        if successPositions |> Positions.isDuesEmpty then
                            PositionsInfo.noBorrowPosition model

                        else
                            successPositions
                                |> Positions.toDueList time
                                |> allPositions model page chain

            _ ->
                PositionsInfo.noUser model
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
    { model
        | device : Device
        , time : Posix
        , images : Images
        , tokenImages : TokenImages
        , pools : Pools
    }
    -> Page
    -> Chain
    -> ( List DuesInfo, List DuesInfo )
    -> Element Msg
allPositions model ((Page { filter }) as page) chain ( activeList, maturedList ) =
    Keyed.column
        [ width fill
        , height shrink
        , spacing 18
        ]
        ([ activeList
            |> List.filter
                (\{ pool } ->
                    filter
                        |> Maybe.map ((==) pool.pair)
                        |> Maybe.withDefault True
                )
            |> List.map
                (\({ pool } as duesInfo) ->
                    ( pool |> Pool.toKey
                    , singleActivePosition model page chain duesInfo
                    )
                )
         , maturedList
            |> List.filter
                (\{ pool } ->
                    filter
                        |> Maybe.map ((==) pool.pair)
                        |> Maybe.withDefault True
                )
            |> List.map
                (\({ pool } as duesInfo) ->
                    ( pool |> Pool.toKey
                    , singleMaturedPosition model page chain duesInfo
                    )
                )
         ]
            |> List.concat
        )


singleActivePosition :
    { model
        | device : Device
        , time : Posix
        , images : Images
        , tokenImages : TokenImages
        , pools : Pools
    }
    -> Page
    -> Chain
    -> DuesInfo
    -> Element Msg
singleActivePosition ({ device, tokenImages } as model) (Page ({ expandedSet } as page)) chain ({ pool, listDue } as duesInfo) =
    column
        [ width fill
        , height shrink
        ]
        [ el
            [ width fill
            , height shrink
            , borrowMoreButton model duesInfo |> inFront
            , repayButton model page duesInfo |> inFront
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
                    if Device.isPhoneOrTablet device then
                        column
                            ([ width fill
                             , (if device |> Device.isPhone then
                                    240

                                else
                                    184
                               )
                                |> px
                                |> height
                             , spacing 12
                             ]
                                ++ Glass.lightPrimary 1
                            )
                            [ row
                                [ width fill
                                , height shrink
                                , paddingXY 20 0
                                , spacing 18
                                , centerY
                                ]
                                [ PairInfo.icons tokenImages pool.pair
                                , PairInfo.symbols pool.pair
                                , discloser model page duesInfo
                                ]
                            , el
                                [ width fill
                                , height shrink
                                , paddingXY 20 0
                                , centerY
                                ]
                                (PositionsInfo.duration model pool.maturity)
                            , el
                                [ width fill
                                , (if device |> Device.isPhone then
                                    100

                                   else
                                    44
                                  )
                                    |> px
                                    |> height
                                , paddingXY 20 0
                                , centerY
                                ]
                                none
                            ]

                    else
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
                    |> List.map (activeBalances model page chain duesInfo)
                )

          else
            none
        ]


singleMaturedPosition :
    { model
        | device : Device
        , time : Posix
        , images : Images
        , tokenImages : TokenImages
        , pools : Pools
    }
    -> Page
    -> Chain
    -> DuesInfo
    -> Element Msg
singleMaturedPosition ({ device, tokenImages } as model) (Page ({ expandedSet } as page)) chain ({ pool, listDue } as duesInfo) =
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
                if Device.isPhoneOrTablet device then
                    column
                        ([ width fill
                         , height <| px 128
                         , spacing 12
                         ]
                            ++ Glass.lightPrimary 1
                        )
                        [ row
                            [ width fill
                            , height shrink
                            , paddingXY 20 0
                            , spacing 18
                            , centerY
                            ]
                            [ PairInfo.icons tokenImages pool.pair
                            , PairInfo.symbols pool.pair
                            , discloser model page duesInfo
                            ]
                        , el
                            [ width fill
                            , height shrink
                            , paddingXY 20 0
                            , centerY
                            ]
                            (PositionsInfo.duration model pool.maturity)
                        ]

                else
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
                    |> List.map (maturedBalances model page chain duesInfo)
                )

          else
            none
        ]


borrowMoreButton : { model | device : Device } -> { duesInfo | pool : Pool } -> Element msg
borrowMoreButton { device } { pool } =
    link
        ([ height <| px 44
         , Border.width 1
         , Border.color Color.primary100
         , Border.rounded 4
         , mouseDown
            [ Background.color Color.primary300
            , Border.color Color.primary300
            ]
         , mouseOver [ Background.color Color.primary100 ]
         ]
            ++ (if device |> Device.isPhone then
                    [ width <| px 295
                    , centerX
                    , alignBottom
                    , moveUp 76
                    ]

                else if device |> Device.isTablet then
                    [ width <| px 247
                    , alignRight
                    , alignBottom
                    , moveLeft 285
                    , moveUp 20
                    ]

                else
                    [ width <| px 140
                    , alignRight
                    , centerY
                    , moveLeft 212
                    ]
               )
        )
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
    { model | device : Device }
    ->
        { page
            | chosenPool : Maybe Pool
            , chosenTokenIds : Set TokenId
        }
    -> DuesInfo
    -> Element msg
repayButton { device } { chosenPool, chosenTokenIds } { pool, listDue } =
    link
        ([ height <| px 44
         , Background.color Color.primary100
         , Border.rounded 4
         , mouseDown [ Background.color Color.primary400 ]
         , mouseOver [ Background.color Color.primary300 ]
         ]
            ++ (if device |> Device.isPhone then
                    [ width <| px 295
                    , centerX
                    , alignBottom
                    , moveUp 20
                    ]

                else if device |> Device.isTablet then
                    [ width <| px 247
                    , alignRight
                    , alignBottom
                    , moveLeft 20
                    , moveUp 20
                    ]

                else
                    [ width <| px 140
                    , alignRight
                    , centerY
                    , moveLeft 54
                    ]
               )
        )
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
    -> { page | expandedSet : Set Pool }
    -> { positionsInfo | pool : Pool }
    -> Element Msg
discloser { images } { expandedSet } { pool } =
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
    { model | device : Device, images : Images, pools : Pools }
    ->
        { page
            | chosenPool : Maybe Pool
            , chosenTokenIds : Set TokenId
            , tooltip : Maybe Tooltip
        }
    -> Chain
    -> { duesInfo | pool : Pool }
    -> DueInfo
    -> Element Msg
activeBalances ({ device, images, pools } as model) ({ chosenPool, chosenTokenIds } as page) chain { pool } { tokenId, debt, collateral } =
    (\checkbox assetElement collateralElement openSeaLink ->
        if device |> Device.isPhoneOrTablet then
            column
                ([ width fill
                 , (if device |> Device.isPhone then
                        144

                    else
                        96
                   )
                    |> px
                    |> height
                 , paddingXY 20 0
                 , spacing 16
                 ]
                    ++ Glass.lightWhite 12
                )
                [ row
                    [ width fill
                    , height shrink
                    , spacing 30
                    , centerY
                    ]
                    [ checkbox
                    , assetElement
                    , openSeaLink
                    ]
                , row
                    [ width fill
                    , height shrink
                    , spacing 30
                    , centerY
                    ]
                    [ el
                        [ width <| px 24
                        , centerY
                        ]
                        none
                    , collateralElement
                    ]
                ]

        else
            row
                ([ width fill
                 , height <| px 56
                 , paddingXY 20 0
                 , spacing 30
                 ]
                    ++ Glass.lightWhite 12
                )
                [ checkbox
                , assetElement
                , line
                , collateralElement
                , openSeaLink
                ]
    )
        (Input.checkbox
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
        )
        ((if device |> Device.isPhone then
            column

          else
            row
         )
            [ width shrink
            , alignLeft
            , centerY
            , (if device |> Device.isPhone then
                8

               else
                30
              )
                |> spacing
            ]
            [ el
                [ width shrink
                , height shrink
                , alignLeft
                , centerY
                , Font.regular
                , Font.size 14
                , Font.color Color.transparent300
                ]
                (text "Amount to repay")
            , assetBalance model page pool tokenId debt
            ]
        )
        ((if device |> Device.isPhone then
            column

          else
            row
         )
            [ width shrink
            , alignLeft
            , centerY
            , (if device |> Device.isPhone then
                8

               else
                30
              )
                |> spacing
            ]
            [ el
                [ width shrink
                , height shrink
                , alignLeft
                , centerY
                , Font.regular
                , Font.size 14
                , Font.color Color.transparent300
                ]
                (text "Collateral locked")
            , collateralBalance model page pool tokenId collateral
            ]
        )
        (pools
            |> Pools.getCollateralizedDebt pool
            |> Maybe.map
                (\collateralizedDebt ->
                    newTabLink
                        [ width shrink
                        , height shrink
                        , alignRight
                        , centerY
                        ]
                        { url =
                            OpenSea.url chain collateralizedDebt tokenId
                        , label =
                            Image.openSea images
                                [ width <| px 20 ]
                        }
                )
            |> Maybe.withDefault none
        )


maturedBalances :
    { model | device : Device, images : Images, pools : Pools }
    -> { page | tooltip : Maybe Tooltip }
    -> Chain
    -> { duesInfo | pool : Pool }
    -> DueInfo
    -> Element Msg
maturedBalances ({ device, images, pools } as model) page chain { pool } { tokenId, debt, collateral } =
    (\assetElement collateralElement ->
        if device |> Device.isPhoneOrTablet then
            column
                ([ width fill
                 , (if device |> Device.isPhone then
                        144

                    else
                        96
                   )
                    |> px
                    |> height
                 , paddingXY 20 0
                 , spacing 16
                 ]
                    ++ Glass.lightWhite 12
                )
                [ row
                    [ width fill
                    , height shrink
                    , centerY
                    ]
                    [ assetElement
                    , pools
                        |> Pools.getCollateralizedDebt pool
                        |> Maybe.map
                            (\collateralizedDebt ->
                                newTabLink
                                    [ width shrink
                                    , height shrink
                                    , alignRight
                                    , centerY
                                    ]
                                    { url =
                                        OpenSea.url chain collateralizedDebt tokenId
                                    , label =
                                        Image.openSea images
                                            [ width <| px 20 ]
                                    }
                            )
                        |> Maybe.withDefault none
                    ]
                , collateralElement
                ]

        else
            row
                ([ width fill
                 , height <| px 56
                 , paddingXY 20 0
                 , spacing 30
                 ]
                    ++ Glass.lightWhite 12
                )
                [ assetElement
                , line
                , collateralElement
                , pools
                    |> Pools.getCollateralizedDebt pool
                    |> Maybe.map
                        (\collateralizedDebt ->
                            newTabLink
                                [ width shrink
                                , height shrink
                                , alignRight
                                , centerY
                                ]
                                { url =
                                    OpenSea.url chain collateralizedDebt tokenId
                                , label =
                                    Image.openSea images
                                        [ width <| px 20 ]
                                }
                        )
                    |> Maybe.withDefault none
                ]
    )
        ((if device |> Device.isPhone then
            column

          else
            row
         )
            [ width shrink
            , centerY
            , (if device |> Device.isPhone then
                8

               else
                30
              )
                |> spacing
            ]
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
            , assetBalance model page pool tokenId debt
            ]
        )
        ((if device |> Device.isPhone then
            column

          else
            row
         )
            [ width shrink
            , centerY
            , (if device |> Device.isPhone then
                8

               else
                30
              )
                |> spacing
            ]
            [ el
                [ width shrink
                , height shrink
                , alignLeft
                , centerY
                , Font.regular
                , Font.size 14
                , Font.color Color.transparent300
                ]
                (text "Collateral forfeited")
            , collateralBalance model page pool tokenId collateral
            ]
        )


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


assetBalance :
    { model | device : Device }
    -> { page | tooltip : Maybe Tooltip }
    -> Pool
    -> TokenId
    -> String
    -> Element Msg
assetBalance { device } { tooltip } pool tokenId asset =
    asset
        |> Truncate.amount
        |> (\{ full, truncated } ->
                truncated
                    |> Maybe.map
                        (\short ->
                            row
                                [ centerY
                                , alignLeft
                                , paddingEach
                                    { top = 3
                                    , right = 0
                                    , bottom = 2
                                    , left = 0
                                    }
                                , spacing 4
                                , Border.widthEach
                                    { top = 0
                                    , right = 0
                                    , bottom = 1
                                    , left = 0
                                    }
                                , Border.dashed
                                , Border.color Color.transparent200
                                , Font.size 18
                                , Events.onMouseEnter (OnMouseEnter (Tooltip.Debt pool tokenId))
                                , Events.onMouseLeave OnMouseLeave
                                , (case tooltip of
                                    Just (Tooltip.Debt chosenPool chosenTokenId) ->
                                        if chosenPool == pool && chosenTokenId == tokenId then
                                            [ full
                                            , pool.pair
                                                |> Pair.toAsset
                                                |> Token.toSymbol
                                            ]
                                                |> String.join " "
                                                |> Tooltip.amount device

                                        else
                                            none

                                    _ ->
                                        none
                                  )
                                    |> (if device |> Device.isPhoneOrTablet then
                                            below

                                        else
                                            onRight
                                       )
                                ]
                                [ el
                                    [ Font.bold
                                    , Font.color Color.transparent500
                                    ]
                                    (text short)
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
                        )
                    |> Maybe.withDefault
                        (row
                            [ width shrink
                            , height shrink
                            , alignLeft
                            , centerY
                            , spacing 4
                            , Font.size 18
                            , Font.color Color.transparent300
                            ]
                            [ el
                                [ Font.bold
                                , Font.color Color.transparent500
                                ]
                                (text full)
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
                        )
           )


collateralBalance :
    { model | device : Device }
    -> { page | tooltip : Maybe Tooltip }
    -> Pool
    -> TokenId
    -> String
    -> Element Msg
collateralBalance { device } { tooltip } pool tokenId collateral =
    collateral
        |> Truncate.amount
        |> (\{ full, truncated } ->
                truncated
                    |> Maybe.map
                        (\short ->
                            row
                                [ centerY
                                , alignLeft
                                , paddingEach
                                    { top = 3
                                    , right = 0
                                    , bottom = 2
                                    , left = 0
                                    }
                                , spacing 4
                                , Font.regular
                                , Border.widthEach
                                    { top = 0
                                    , right = 0
                                    , bottom = 1
                                    , left = 0
                                    }
                                , Border.dashed
                                , Border.color Color.transparent200
                                , Font.size 18
                                , Events.onMouseEnter (OnMouseEnter (Tooltip.Collateral pool tokenId))
                                , Events.onMouseLeave OnMouseLeave
                                , (case tooltip of
                                    Just (Tooltip.Collateral chosenPool chosenTokenId) ->
                                        if chosenPool == pool && chosenTokenId == tokenId then
                                            [ full
                                            , pool.pair
                                                |> Pair.toCollateral
                                                |> Token.toSymbol
                                            ]
                                                |> String.join " "
                                                |> Tooltip.amount device

                                        else
                                            none

                                    _ ->
                                        none
                                  )
                                    |> (if device |> Device.isPhoneOrTablet then
                                            below

                                        else
                                            onRight
                                       )
                                ]
                                [ el
                                    [ Font.bold
                                    , Font.color Color.transparent500
                                    ]
                                    (text short)
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
                        (row
                            [ width shrink
                            , height shrink
                            , alignLeft
                            , centerY
                            , spacing 4
                            , Font.size 18
                            , Font.color Color.transparent300
                            ]
                            [ el
                                [ Font.bold
                                , Font.color Color.transparent500
                                ]
                                (text full)
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
           )
