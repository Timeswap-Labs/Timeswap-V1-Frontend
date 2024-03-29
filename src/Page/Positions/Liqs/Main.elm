module Page.Positions.Liqs.Main exposing
    ( Effect(..)
    , Msg
    , Positions
    , init
    , update
    , view
    )

import Blockchain.User.Liq exposing (Liq)
import Blockchain.User.Liqs as Liqs exposing (Liqs)
import Blockchain.User.Main as User exposing (User)
import Data.Address exposing (Address)
import Data.Backdrop exposing (Backdrop)
import Data.ChosenZone exposing (ChosenZone)
import Data.Device exposing (Device(..))
import Data.Images exposing (Images)
import Data.Maturity as Maturity
import Data.Offset exposing (Offset)
import Data.Pool as Pool exposing (Pool)
import Data.Remote exposing (Remote(..))
import Data.Theme as Theme exposing (Theme)
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
import Page.Positions.Liqs.Tooltip as Tooltip exposing (Tooltip)
import Sort.Dict as Dict
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
    = ClickLiq Address Pool
    | OnMouseEnter Tooltip
    | OnMouseLeave


type Effect
    = OpenLiq Address Pool


init : Positions
init =
    Positions Nothing


update : Msg -> Positions -> ( Positions, Maybe Effect )
update msg positions =
    case msg of
        ClickLiq convAddress pool ->
            ( positions
            , OpenLiq convAddress pool
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
    }
    -> User
    -> Positions
    -> Element Msg
view ({ device, backdrop, theme } as model) user (Positions tooltip) =
    el
        ([ Region.description "liquidity positions"
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
        (case user |> User.getLiqs of
            Loading _ ->
                loading model

            Failure _ ->
                errorHandlerNativesFetch

            -- |> Debug.log "error view"
            Success liqs ->
                liqs
                    |> Liqs.filterEmptyLiqs
                    |> (\filteredLiqs ->
                            if filteredLiqs |> Dict.isEmpty then
                                noLiqs model

                            else
                                viewLiqs model tooltip filteredLiqs
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
                (text "Your Liquidity Positions")
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
                            [ text "Fetching your Liquidity Positions..." ]
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


noLiqs : { model | device : Device, images : Images, theme : Theme } -> Element msg
noLiqs { images, theme } =
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
                (text "Your Liquidity Positions")
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
                            [ text "You don't have any open Liquidity positions" ]
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


viewLiqs :
    { model
        | time : Posix
        , offset : Offset
        , chosenZone : ChosenZone
        , theme : Theme
        , images : Images
    }
    -> Maybe Tooltip
    -> Liqs
    -> Element Msg
viewLiqs ({ theme, time } as model) tooltip liqs =
    column
        [ width fill
        , height shrink
        , spacing 20
        ]
        [ title theme liqs
        , Keyed.column
            [ width fill
            , height shrink
            , spacing 12
            ]
            (liqs
                |> Liqs.toList time
                |> List.map
                    (\( convAddress, poolLiqTuple ) ->
                        ( poolLiqTuple |> Tuple.first |> Pool.toString
                        , poolLiqTuple |> viewLiq model tooltip convAddress
                        )
                    )
            )
        ]


title : Theme -> Liqs -> Element msg
title theme liqs =
    el
        [ width shrink
        , height shrink
        , Font.size 16
        , Font.bold
        , paddingXY 0 2
        , theme |> ThemeColor.text |> Font.color
        ]
        ([ "Your Liquidity Positions "
         , "("
         , liqs
            |> Liqs.getPositionCount
            |> String.fromInt
         , ")"
         ]
            |> String.concat
            |> text
        )


viewLiq :
    { model
        | time : Posix
        , offset : Offset
        , chosenZone : ChosenZone
        , theme : Theme
        , images : Images
    }
    -> Maybe Tooltip
    -> Address
    -> ( Pool, Liq )
    -> Element Msg
viewLiq { time, offset, chosenZone, theme, images } tooltip convAddress ( pool, claim ) =
    Input.button
        [ width fill
        , height <| px 56
        ]
        { onPress = ClickLiq convAddress pool |> Just
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
                , row
                    [ width shrink
                    , height shrink
                    , alignRight
                    , centerY
                    , spacing 10
                    ]
                    [ images
                        |> (case theme of
                                Theme.Dark ->
                                    Image.hourglassPrimary

                                Theme.Light ->
                                    Image.hourglassDark
                           )
                            [ width <| px 16
                            , height <| px 16
                            ]
                    , Duration.viewMaturity
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
                    ]
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
                            (text "Active")
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
                            , Font.color Color.negative400
                            , Font.bold
                            ]
                            (text "Matured")
                        )
                , images
                    |> Image.discloser
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
