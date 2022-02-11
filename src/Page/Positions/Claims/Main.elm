module Page.Positions.Claims.Main exposing
    ( Effect(..)
    , Msg
    , Positions
    , init
    , update
    , view
    )

import Animator exposing (Timeline)
import Blockchain.User.Claim as Claim exposing (Claim)
import Blockchain.User.Claims as Claims exposing (Claims)
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
        , alpha
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
import Page.Positions.Claims.Tooltip as Tooltip exposing (Tooltip)
import Sort.Dict as Dict
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
    = ClickClaim Pool
    | OnMouseEnter Tooltip
    | OnMouseLeave


type Effect
    = OpenClaim Pool


init : Positions
init =
    Positions Nothing


update : Msg -> Positions -> ( Positions, Maybe Effect )
update msg positions =
    case msg of
        ClickClaim pool ->
            ( positions
            , OpenClaim pool
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
view ({ theme, device, backdrop } as model) user (Positions tooltip) =
    el
        ([ Region.description "lend positions"
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
        (case user |> User.getClaims of
            Loading timeline ->
                loading model timeline

            Failure error ->
                none

            -- |> Debug.log "error view"
            Success claims ->
                claims
                    |> Dict.dropIf (\_ claim -> claim |> Claim.isZero)
                    |> (\filteredClaims ->
                            if filteredClaims |> Dict.isEmpty then
                                noClaims model

                            else
                                viewClaims model tooltip filteredClaims
                       )
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
            [ text "Fetching your Lend positions..." ]
        , el
            []
            (Loading.view timeline theme)
        ]


noClaims : { model | images : Images, theme : Theme } -> Element msg
noClaims { images, theme } =
    row
        [ width shrink
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
            [ width shrink
            , height shrink
            , centerX
            , centerY
            , Font.size 14
            , paddingXY 0 3
            , theme |> ThemeColor.textLight |> Font.color
            ]
            [ text "Your Lend positions will appear here..." ]
        ]


viewClaims :
    { model
        | time : Posix
        , offset : Offset
        , chosenZone : ChosenZone
        , theme : Theme
        , images : Images
    }
    -> Maybe Tooltip
    -> Claims
    -> Element Msg
viewClaims ({ time, theme } as model) tooltip claims =
    column
        [ width fill
        , height shrink
        , spacing 20
        ]
        [ title theme claims
        , Keyed.column
            [ width fill
            , height shrink
            , spacing 12
            ]
            (claims
                |> Claims.toList time
                |> List.map
                    (\(( pool, _ ) as tuple) ->
                        ( pool |> Pool.toString
                        , tuple |> viewClaim model tooltip
                        )
                    )
            )
        ]


title : Theme -> Claims -> Element msg
title theme claims =
    el
        [ width shrink
        , height shrink
        , Font.size 16
        , Font.bold
        , paddingXY 0 2
        , theme |> ThemeColor.text |> Font.color
        ]
        ([ "Your Lend Positions "
         , "("
         , claims
            |> Dict.size
            |> String.fromInt
         , ")"
         ]
            |> String.concat
            |> text
        )


viewClaim :
    { model
        | time : Posix
        , offset : Offset
        , chosenZone : ChosenZone
        , theme : Theme
        , images : Images
    }
    -> Maybe Tooltip
    -> ( Pool, Claim )
    -> Element Msg
viewClaim { time, offset, chosenZone, theme, images } tooltip ( pool, claim ) =
    Input.button
        [ width fill
        , height <| px 56
        ]
        { onPress = ClickClaim pool |> Just
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
                        , height <| px 8
                        , alignRight
                        , centerY
                        , alpha 0.6
                        , (pi / 2)
                            |> negate
                            |> rotate
                        ]
                ]
        }
