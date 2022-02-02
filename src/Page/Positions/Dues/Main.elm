module Page.Positions.Dues.Main exposing
    ( Effect(..)
    , Msg
    , Positions
    , init
    , noDues
    , update
    , view
    )

import Animator exposing (Timeline)
import Blockchain.User.Due exposing (Due)
import Blockchain.User.Dues as Dues exposing (Dues)
import Blockchain.User.Main as User exposing (User)
import Blockchain.User.TokenId exposing (TokenId)
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
import Page.Positions.Dues.Tooltip as Tooltip exposing (Tooltip)
import Sort.Dict as Dict exposing (Dict)
import Time exposing (Posix)
import Utility.Color as Color
import Utility.Duration as Duration
import Utility.Glass as Glass
import Utility.Id as Id
import Utility.Image as Image
import Utility.Loading as Loading
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
    }
    -> User
    -> Positions
    -> Element Msg
view ({ device, backdrop, theme } as model) user (Positions tooltip) =
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
            Loading timeline ->
                loading model timeline

            Failure error ->
                none

            -- |> Debug.log "error view"
            Success dues ->
                if dues |> Dict.isEmpty then
                    noDues model

                else
                    viewDues model tooltip dues
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
            , theme |> ThemeColor.textLight |> Font.color
            ]
            [ text "Fetching your Borrow positions..." ]
        , el
            []
            (Loading.view timeline)
        ]


noDues : { model | device : Device, images : Images, theme : Theme } -> Element msg
noDues { device, images, theme } =
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
        , spacing 8
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
            , Font.size 14
            , paddingXY 0 3
            , theme |> ThemeColor.textLight |> Font.color
            ]
            [ text "Your Borrow positions including from Liqudity transactions will appear here..." ]
        ]


viewDues :
    { model
        | time : Posix
        , offset : Offset
        , chosenZone : ChosenZone
        , theme : Theme
        , images : Images
    }
    -> Maybe Tooltip
    -> Dues
    -> Element Msg
viewDues ({ time, theme } as model) tooltip dues =
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
                        , tuple |> viewDue model tooltip
                        )
                    )
            )
        ]


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
