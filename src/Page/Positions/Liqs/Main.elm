module Page.Positions.Liqs.Main exposing
    ( Effect(..)
    , Msg
    , Positions
    , init
    , update
    , view
    )

import Animator exposing (Timeline)
import Blockchain.User.Liq exposing (Liq)
import Blockchain.User.Liqs as Liqs exposing (Liqs)
import Blockchain.User.Main as User exposing (User)
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
        , alignTop
        , centerX
        , centerY
        , column
        , el
        , fill
        , height
        , none
        , padding
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
import Page.Positions.Liqs.Tooltip as Tooltip exposing (Tooltip)
import Sort.Dict as Dict
import Time exposing (Posix)
import Utility.Color as Color
import Utility.Duration as Duration
import Utility.Glass as Glass
import Utility.Id as Id
import Utility.Image as Image
import Utility.Loading as Loading
import Utility.PairImage as PairImage
import Utility.Truncate as Truncate


type Positions
    = Positions (Maybe Tooltip)


type Msg
    = ClickLiq Pool
    | OnMouseEnter Tooltip
    | OnMouseLeave


type Effect
    = OpenLiq Pool


init : Positions
init =
    Positions Nothing


update : Msg -> Positions -> ( Positions, Maybe Effect )
update msg positions =
    case msg of
        ClickLiq pool ->
            ( positions
            , OpenLiq pool
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
         , Border.color Color.transparent100
         , Id.is "positions"
         ]
            ++ Glass.background backdrop theme
        )
        (case user |> User.getLiqs of
            Loading timeline ->
                loading model timeline

            Failure error ->
                none

            -- |> Debug.log "error view"
            Success liqs ->
                if liqs |> Dict.isEmpty then
                    noLiqs model

                else
                    viewLiqs model tooltip liqs
        )


loading : { model | images : Images, theme : Theme } -> Timeline () -> Element msg
loading { images, theme } timeline =
    row
        [ width shrink
        , height shrink
        , centerX
        , centerY
        , spacing 12
        ]
        [ images
            |> (case theme of
                    Theme.Dark ->
                        Image.info

                    Theme.Light ->
                        Image.infoDark
               )
                [ width <| px 20
                , height <| px 20
                , centerX
                , alignTop
                ]
        , paragraph
            [ width shrink
            , height shrink
            , centerX
            , centerY
            , Font.size 14
            , paddingXY 0 3
            , Font.color Color.transparent300
            ]
            [ text "Fetching your Lend positions..." ]
        , el
            []
            (Loading.view timeline)
        ]


noLiqs : { model | device : Device, images : Images, theme : Theme } -> Element msg
noLiqs { device, images, theme } =
    row
        [ (case device of
            Desktop ->
                shrink

            _ ->
                fill
          )
            |> width
        , height shrink
        , centerX
        , centerY
        , spacing 12
        ]
        [ images
            |> (case theme of
                    Theme.Dark ->
                        Image.info

                    Theme.Light ->
                        Image.infoDark
               )
                [ width <| px 20
                , height <| px 20
                , centerX
                , alignTop
                ]
        , paragraph
            [ width fill
            , height shrink
            , centerX
            , centerY
            , Font.size 14
            , paddingXY 0 3
            , Font.color Color.transparent300
            ]
            [ text "Your Borrow positions from your Liquidity transactions will appear in Borrow section. Your Liquidity positions will appear here..." ]
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
viewLiqs ({ time } as model) tooltip liqs =
    column
        [ width fill
        , height shrink
        , spacing 20
        ]
        [ title liqs
        , Keyed.column
            [ width fill
            , height shrink
            , spacing 12
            ]
            (liqs
                |> Liqs.toList time
                |> List.map
                    (\(( pool, _ ) as tuple) ->
                        ( pool |> Pool.toString
                        , tuple |> viewLiq model tooltip
                        )
                    )
            )
        ]


title : Liqs -> Element msg
title liqs =
    el
        [ width shrink
        , height shrink
        , Font.size 16
        , paddingXY 0 2
        , Font.color Color.transparent500
        ]
        ([ "Your Liquidity Positions"
         , "("
         , liqs
            |> Dict.size
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
    -> ( Pool, Liq )
    -> Element Msg
viewLiq { time, offset, chosenZone, theme, images } tooltip ( pool, claim ) =
    Input.button
        [ width fill
        , height <| px 56
        ]
        { onPress = ClickLiq pool |> Just
        , label =
            row
                [ width fill
                , height fill
                , paddingXY 20 0
                , spacing 12
                , Background.color Color.dark500
                , Border.width 1
                , Border.rounded 8
                , Border.color Color.transparent100
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
                        , Background.color Color.positive100
                        , Border.rounded 999
                        ]
                        (el
                            [ width shrink
                            , height shrink
                            , centerX
                            , centerY
                            , Font.size 14
                            , Font.color Color.positive400
                            ]
                            (text "Active")
                        )

                  else
                    el
                        [ width shrink
                        , height <| px 24
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
