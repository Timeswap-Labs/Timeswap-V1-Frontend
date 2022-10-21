module Page.Positions.Dues.Main exposing
    ( Effect(..)
    , Msg
    , Positions
    , init
    , noDues
    , update
    , view
    )

import Blockchain.User.Due exposing (Due)
import Blockchain.User.Dues as Dues exposing (Dues)
import Blockchain.User.Main as User exposing (User)
import Blockchain.User.TokenId exposing (TokenId)
import Data.Backdrop exposing (Backdrop)
import Data.Chain exposing (Chain)
import Data.Chains as Chains exposing (Chains)
import Data.ChosenZone exposing (ChosenZone)
import Data.Device exposing (Device(..))
import Data.ERC20 as ERC20 exposing (ERC20)
import Data.Images exposing (Images)
import Data.Maturity as Maturity
import Data.Offset exposing (Offset)
import Data.Pair as Pair
import Data.Pool as Pool exposing (Pool)
import Data.Remote exposing (Remote(..))
import Data.Theme as Theme exposing (Theme)
import Data.Token as Token
import Element
    exposing
        ( Element
        , alignRight
        , centerX
        , centerY
        , column
        , el
        , fill
        , height
        , inFront
        , padding
        , paddingEach
        , paddingXY
        , paragraph
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
import Element.Region as Region
import Page.Position.Claim.Main exposing (errorHandlerNativesFetch)
import Page.Positions.Dues.Tooltip as Tooltip exposing (Tooltip)
import Sort.Dict as Dict exposing (Dict)
import Time exposing (Posix)
import Utility.Color as Color
import Utility.Duration as Duration
import Utility.Glass as Glass
import Utility.Id as Id
import Utility.Image as Image
import Utility.PairImage as PairImage
import Utility.ThemeColor as ThemeColor
import Utility.Truncate as Truncate


type Positions
    = Positions (Maybe Tooltip)


type Msg
    = ClickDue Pool
    | OnMouseEnter Tooltip
    | OnMouseLeave


type Effect
    = OpenDue Pool


init : Positions
init =
    Positions Nothing


update : Msg -> Positions -> ( Positions, Maybe Effect )
update msg positions =
    case msg of
        ClickDue pool ->
            ( positions
            , OpenDue pool
                |> Just
            )

        OnMouseEnter tooltip ->
            ( Just tooltip |> Positions
            , Nothing
            )

        OnMouseLeave ->
            ( Nothing |> Positions
            , Nothing
            )


view :
    { model
        | time : Posix
        , offset : Offset
        , chosenZone : ChosenZone
        , theme : Theme
        , device : Device
        , backdrop : Backdrop
        , images : Images
        , chains : Chains
    }
    -> User
    -> Chain
    -> Positions
    -> Element Msg
view ({ device, backdrop, theme } as model) user chain (Positions tooltip) =
    el
        ([ Region.description "borrow positions"
         , (case device of
                Desktop ->
                    758

                _ ->
                    375
           )
            |> px
            |> width
         , height shrink
         , (case device of
                Desktop ->
                    24

                _ ->
                    16
           )
            |> padding
         , Border.rounded 8
         , Border.width 1
         , theme |> ThemeColor.border |> Border.color
         , Id.is "positions"
         ]
            ++ Glass.background backdrop theme
        )
        (case user |> User.getDues of
            Loading _ ->
                loading model

            Failure _ ->
                errorHandlerNativesFetch

            -- |> Debug.log "error view"
            Success dues ->
                dues
                    |> Dues.filterEmptyDues
                    |> (\filteredDues ->
                            if filteredDues |> Dict.isEmpty then
                                noDues model

                            else
                                viewDues model tooltip chain filteredDues
                       )
        )


loading : { model | images : Images, theme : Theme } -> Element msg
loading { images, theme } =
    column
        [ width fill
        , height shrink
        , spacing 1
        ]
        [ row
            [ width fill
            , height shrink
            , spacing 0
            ]
            [ el
                [ width shrink
                , height shrink
                , paddingEach
                    { top = 0
                    , right = 0
                    , bottom = 22
                    , left = 0
                    }
                , Font.size 16
                , theme |> ThemeColor.text |> Font.color
                , Font.bold
                ]
                (text "Your Borrow Positions")
            ]
        , row
            [ width shrink
            , height shrink
            , centerX
            , centerY
            , spacing 8
            , inFront
                (column
                    [ centerX
                    , centerY
                    ]
                    [ row
                        [ width shrink
                        , height shrink
                        , centerX
                        , centerY
                        , spacing 12
                        ]
                        [ el
                            []
                            (images
                                |> Image.loadingAnimation
                                    [ width <| px 30
                                    , height <| px 30
                                    , centerX
                                    , centerY
                                    ]
                            )
                        ]
                    , row
                        [ centerX
                        , centerY
                        ]
                        [ paragraph
                            [ width shrink
                            , height shrink
                            , centerX
                            , centerY
                            , Font.size 14
                            , paddingXY 0 8
                            , theme |> ThemeColor.textLight |> Font.color
                            ]
                            [ text "Fetching your Borrow Positions..." ]
                        ]
                    ]
                )
            ]
            [ images
                |> (case theme of
                        Theme.Dark ->
                            Image.loadingPositionsDark

                        -- Image
                        Theme.Light ->
                            Image.loadingPositions
                   )
                    [ height <| px 137
                    , width <| px 710
                    , centerX
                    , paddingEach
                        { top = 0
                        , right = 0
                        , bottom = 26
                        , left = 0
                        }
                    ]
            ]
        ]


noDues : { model | device : Device, images : Images, theme : Theme } -> Element msg
noDues { images, theme } =
    column
        [ width fill
        , height shrink
        , spacing 1
        ]
        [ row
            [ width fill
            , height shrink
            , spacing 0
            ]
            [ el
                [ width shrink
                , height shrink
                , paddingEach
                    { top = 0
                    , right = 0
                    , bottom = 22
                    , left = 0
                    }
                , Font.size 16
                , theme |> ThemeColor.text |> Font.color
                , Font.bold
                ]
                (text "Your Borrow Positions")
            ]
        , row
            [ width shrink
            , height shrink
            , centerX
            , centerY
            , spacing 8
            , inFront
                (column
                    [ centerX
                    , centerY
                    ]
                    [ row
                        [ centerX
                        , centerY
                        ]
                        [ images
                            |> (case theme of
                                    Theme.Dark ->
                                        Image.borrowloadingPositionsIconDark

                                    Theme.Light ->
                                        Image.borrowloadingPositionsIcon
                               )
                                [ width <| px 36
                                , height <| px 36
                                , centerX
                                , paddingEach
                                    { top = 0
                                    , right = 0
                                    , bottom = 14
                                    , left = 0
                                    }
                                ]
                        ]
                    , row
                        [ centerX
                        , centerY
                        ]
                        [ paragraph
                            [ width shrink
                            , height shrink
                            , centerX
                            , centerY
                            , Font.size 14
                            , paddingXY 0 8
                            , theme |> ThemeColor.textLight |> Font.color
                            ]
                            [ text "You don’t have any open Borrow positions" ]
                        ]
                    ]
                )
            ]
            [ images
                |> (case theme of
                        Theme.Dark ->
                            Image.loadingPositionsDark

                        -- Image
                        Theme.Light ->
                            Image.loadingPositions
                   )
                    [ height <| px 137
                    , width <| px 710
                    , centerX
                    , paddingEach
                        { top = 0
                        , right = 0
                        , bottom = 26
                        , left = 0
                        }
                    ]
            ]
        ]


viewDues :
    { model
        | time : Posix
        , offset : Offset
        , chosenZone : ChosenZone
        , theme : Theme
        , images : Images
        , chains : Chains
    }
    -> Maybe Tooltip
    -> Chain
    -> Dues
    -> Element Msg
viewDues ({ time, theme, chains } as model) tooltip chain dues =
    column
        [ width fill
        , height shrink
        , spacing 20
        ]
        [ title theme dues
        , Keyed.column
            [ width fill
            , height shrink
            , spacing 12
            ]
            (dues
                |> Dues.toList time
                |> List.map
                    (\(( pool, _ ) as tuple) ->
                        ( pool |> Pool.toString
                        , (tuple |> poolTransform chains chain) |> viewDue model tooltip
                        )
                    )
            )
        ]


poolTransform : Chains -> Chain -> ( Pool, Dict TokenId Due ) -> ( Pool, Dict TokenId Due )
poolTransform chains chain poolDueTuple =
    poolDueTuple
        |> (\( pool, dict ) ->
                pool.pair
                    |> Pair.toAsset
                    |> (\token ->
                            (token |> Token.toERC20)
                                |> Maybe.map (\erc20 -> tokenTransform chains chain erc20 |> Token.ERC20)
                                |> Maybe.withDefault token
                       )
                    |> (\assetToken ->
                            Pair.init assetToken
                                (pool.pair
                                    |> Pair.toCollateral
                                    |> (\token ->
                                            (token |> Token.toERC20)
                                                |> Maybe.map (\erc20 -> tokenTransform chains chain erc20 |> Token.ERC20)
                                                |> Maybe.withDefault token
                                       )
                                )
                       )
                    |> Maybe.map (\pair -> pair)
                    |> Maybe.withDefault pool.pair
                    |> (\pair -> { pair = pair, maturity = pool.maturity })
                    |> (\newPool -> Tuple.pair newPool dict)
           )


tokenTransform : Chains -> Chain -> ERC20 -> ERC20
tokenTransform chains chain erc20 =
    (chains |> Chains.toERC20List chain)
        |> List.filter (\chainERC20 -> (chainERC20 |> ERC20.toAddress) == (erc20 |> ERC20.toAddress))
        |> List.head
        |> Maybe.map (\chainERC20 -> chainERC20)
        |> Maybe.withDefault erc20


title : Theme -> Dues -> Element msg
title theme dues =
    el
        [ width shrink
        , height shrink
        , Font.size 16
        , Font.bold
        , paddingXY 0 2
        , theme |> ThemeColor.text |> Font.color
        ]
        ([ "Your Borrow Positions "
         , "("
         , dues
            |> Dict.size
            |> String.fromInt
         , ")"
         ]
            |> String.concat
            |> text
        )


viewDue :
    { model
        | time : Posix
        , offset : Offset
        , chosenZone : ChosenZone
        , theme : Theme
        , images : Images
    }
    -> Maybe Tooltip
    -> ( Pool, Dict TokenId Due )
    -> Element Msg
viewDue { time, offset, chosenZone, theme, images } tooltip ( pool, due ) =
    Input.button
        [ width fill
        , height <| px 56
        ]
        { onPress = ClickDue pool |> Just
        , label =
            row
                [ width fill
                , height fill
                , paddingXY 20 0
                , spacing 12
                , theme |> ThemeColor.positionBG |> Background.color
                , Border.width 1
                , Border.rounded 8
                , theme |> ThemeColor.border |> Border.color
                ]
                [ el
                    [ width shrink
                    , height shrink
                    , centerY
                    ]
                    (images
                        |> PairImage.view
                            { pair = pool.pair
                            , length = 24
                            }
                    )
                , el
                    [ width shrink
                    , height shrink
                    , centerY
                    ]
                    (Truncate.viewPairSymbol
                        { onMouseEnter = OnMouseEnter
                        , onMouseLeave = OnMouseLeave
                        , tooltip = Tooltip.Symbol pool
                        , opened = tooltip
                        , pair = pool.pair
                        , fontSize = 14
                        , fontPadding = 3
                        , theme = theme
                        }
                    )
                , el
                    [ width shrink
                    , height shrink
                    , alignRight
                    , centerY
                    ]
                    (Duration.viewMaturity
                        { onMouseEnter = OnMouseEnter
                        , onMouseLeave = OnMouseLeave
                        , tooltip = Tooltip.Maturity pool
                        , opened = tooltip
                        , time = time
                        , offset = offset
                        , chosenZone = chosenZone
                        , maturity = pool.maturity
                        , theme = theme
                        }
                    )
                , if pool.maturity |> Maturity.isActive time then
                    el
                        [ width shrink
                        , height <| px 24
                        , paddingXY 10 2
                        , Background.color Color.positive100
                        , Border.rounded 999
                        ]
                        (el
                            [ width shrink
                            , height shrink
                            , centerX
                            , centerY
                            , Font.size 14
                            , Font.bold
                            , Font.color Color.positive400
                            ]
                            ([ due
                                |> Dict.size
                                |> String.fromInt
                             , "Active"
                             ]
                                |> String.join " "
                                |> text
                            )
                        )

                  else
                    el
                        [ width shrink
                        , height <| px 24
                        , paddingXY 10 2
                        , Background.color Color.negative100
                        , Border.rounded 999
                        ]
                        (el
                            [ width shrink
                            , height shrink
                            , centerX
                            , centerY
                            , Font.size 14
                            , Font.bold
                            , Font.color Color.negative400
                            ]
                            (text "Matured")
                        )
                , images
                    |> (case theme of
                            Theme.Dark ->
                                Image.discloser

                            Theme.Light ->
                                Image.arrowDownDark
                       )
                        [ width <| px 11
                        , height <| px 7
                        , alignRight
                        , centerY
                        , (pi / 2)
                            |> negate
                            |> rotate
                        ]
                ]
        }
