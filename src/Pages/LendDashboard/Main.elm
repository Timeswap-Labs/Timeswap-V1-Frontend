module Pages.LendDashboard.Main exposing
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
import Data.Positions as Positions exposing (ActiveClaimInfo, Claim, MaturedClaimInfo, Positions, Return)
import Data.Remote exposing (Remote(..))
import Data.Token as Token
import Data.TokenImages exposing (TokenImages)
import Data.Tokens exposing (Tokens)
import Element
    exposing
        ( Element
        , alignBottom
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
        , moveUp
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
import Pages.LendDashboard.Tooltip as Tooltip exposing (Tooltip)
import Sort.Set as Set exposing (Set)
import Time exposing (Posix)
import Utility.Color as Color
import Utility.Glass as Glass
import Utility.Image as Image
import Utility.Loading as Loading
import Utility.PairInfo as PairInfo
import Utility.PositionsInfo as PositionsInfo
import Utility.Router as Router
import Utility.Truncate as Truncate


type Page
    = Page
        { filter : Maybe Pair
        , expandedSet : Set Pool
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
                            |> Positions.getFirstClaim time filter
                            |> Maybe.map (Set.singleton Pool.sorter)
                            |> Maybe.withDefault (Set.empty Pool.sorter)

            _ ->
                Set.empty Pool.sorter
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

                _ ->
                    Page page

        Collapse pool ->
            { page
                | expandedSet = page.expandedSet |> Set.remove pool
            }
                |> Page

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
                |> Positions.getFirstClaim time filter
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
        , user : Remote error { user | positions : Remote () Positions }
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
            Success { positions } ->
                case positions of
                    Loading ->
                        PositionsInfo.loading

                    Failure _ ->
                        none

                    Success successPositions ->
                        if successPositions |> Positions.isClaimEmpty then
                            PositionsInfo.noLendPosition model

                        else
                            successPositions
                                |> Positions.toClaimList time
                                |> allPositions model page

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
    { model
        | device : Device
        , time : Posix
        , images : Images
        , tokenImages : TokenImages
    }
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
    { model | device : Device, time : Posix, images : Images, tokenImages : TokenImages }
    -> Page
    -> MaturedClaimInfo
    -> Element Msg
singleMaturedPosition ({ device, tokenImages } as model) (Page ({ expandedSet } as page)) ({ pool, return } as maturedClaimInfo) =
    column
        [ width fill
        , height shrink
        ]
        [ el
            [ width fill
            , height shrink
            , (return
                |> Maybe.map (\_ -> claimButton model maturedClaimInfo)
                |> Maybe.withDefault (claimButtonOff model)
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
                    if Device.isPhoneOrTablet device then
                        column
                            ([ width fill
                             , height <| px 184
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
                                , discloser model page maturedClaimInfo
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
                                , height <| px 44
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
                (maturedBalances model page maturedClaimInfo)

          else
            none
        ]


singleActivePosition :
    { model | device : Device, time : Posix, images : Images, tokenImages : TokenImages }
    -> Page
    -> ActiveClaimInfo
    -> Element Msg
singleActivePosition ({ device, tokenImages } as model) (Page ({ expandedSet } as page)) ({ pool } as activeClaimInfo) =
    column
        [ width fill
        , height shrink
        ]
        [ el
            [ width fill
            , height shrink
            , lendMoreButton model activeClaimInfo |> inFront
            , claimButtonOff model |> inFront
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
                                    248

                                else
                                    192
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
                                , discloser model page activeClaimInfo
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
                (activeBalances model page activeClaimInfo)

          else
            none
        ]


claimButton : { model | device : Device } -> { maturedClaimInfo | pool : Pool } -> Element msg
claimButton { device } { pool } =
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
                    [ width <| px 110
                    , alignRight
                    , centerY
                    , moveLeft 54
                    ]
               )
        )
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


lendMoreButton : { model | device : Device } -> { activeClaimInfo | pool : Pool } -> Element msg
lendMoreButton { device } { pool } =
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
                    [ width <| px 110
                    , alignRight
                    , centerY
                    , moveLeft 182
                    ]
               )
        )
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


claimButtonOff : { model | device : Device } -> Element msg
claimButtonOff { device } =
    el
        ([ height <| px 44
         , alignRight
         , Background.color Color.transparent100
         , Border.rounded 4
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
                    [ width <| px 110
                    , alignRight
                    , centerY
                    , moveLeft 54
                    ]
               )
        )
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


maturedBalances :
    { model | device : Device }
    -> { page | tooltip : Maybe Tooltip }
    ->
        { maturedClaimInfo
            | pool : Pool
            , return : Maybe Return
        }
    -> Element Msg
maturedBalances { device } page { pool, return } =
    (if device |> Device.isPhone then
        column

     else
        row
    )
        ([ width fill
         , (if device |> Device.isPhone then
                80

            else
                56
           )
            |> px
            |> height
         , paddingXY 20 0
         , (if device |> Device.isPhone then
                8

            else
                30
           )
            |> spacing
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
                        [ assetBalance page pool asset
                        , el
                            [ Font.bold
                            , Font.color Color.transparent500
                            ]
                            (text "+")
                        , collateralBalance page pool collateral
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
    { model | device : Device }
    -> { page | tooltip : Maybe Tooltip }
    -> { activeClaimInfo | pool : Pool, claim : Claim }
    -> Element Msg
activeBalances { device } page { pool, claim } =
    (\assetElement collateralElement ->
        if device |> Device.isPhoneOrTablet then
            column
                ([ width fill
                 , (if device |> Device.isPhone then
                        136

                    else
                        88
                   )
                    |> px
                    |> height
                 , paddingXY 20 0
                 , spacing 16
                 ]
                    ++ Glass.lightWhite 12
                )
                [ assetElement
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
                (text "Amount at maturity")
            , assetBalance page pool claim.bond
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
                (text "Insurance in case of 100% default")
            , collateralBalance page pool claim.insurance
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


assetBalance : { page | tooltip : Maybe Tooltip } -> Pool -> String -> Element Msg
assetBalance { tooltip } pool asset =
    asset
        |> Truncate.amount
        |> (\{ full, truncated } ->
                truncated
                    |> Maybe.map
                        (\short ->
                            row
                                [ alignLeft
                                , centerY
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
                                , Events.onMouseEnter (OnMouseEnter (Tooltip.Bond pool))
                                , Events.onMouseLeave OnMouseLeave
                                , (case tooltip of
                                    Just (Tooltip.Bond chosenPool) ->
                                        if chosenPool == pool then
                                            [ full
                                            , pool.pair
                                                |> Pair.toAsset
                                                |> Token.toSymbol
                                            ]
                                                |> String.join " "
                                                |> Tooltip.amount

                                        else
                                            none

                                    _ ->
                                        none
                                  )
                                    |> onRight
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


collateralBalance : { page | tooltip : Maybe Tooltip } -> Pool -> String -> Element Msg
collateralBalance { tooltip } pool collateral =
    collateral
        |> Truncate.amount
        |> (\{ full, truncated } ->
                truncated
                    |> Maybe.map
                        (\short ->
                            row
                                [ alignLeft
                                , centerY
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
                                , Events.onMouseEnter (OnMouseEnter (Tooltip.Insurance pool))
                                , Events.onMouseLeave OnMouseLeave
                                , (case tooltip of
                                    Just (Tooltip.Insurance chosenPool) ->
                                        if chosenPool == pool then
                                            [ full
                                            , pool.pair
                                                |> Pair.toCollateral
                                                |> Token.toSymbol
                                            ]
                                                |> String.join " "
                                                |> Tooltip.amount

                                        else
                                            none

                                    _ ->
                                        none
                                  )
                                    |> onRight
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
