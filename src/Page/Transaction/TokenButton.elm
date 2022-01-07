module Page.Transaction.TokenButton exposing (view)

import Data.Images exposing (Images)
import Data.Theme as Theme exposing (Theme)
import Data.Token exposing (Token)
import Data.TokenParam as TokenParam exposing (TokenParam)
import Element
    exposing
        ( Element
        , alignLeft
        , alignRight
        , centerY
        , el
        , fill
        , height
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
import Element.Input as Input
import Element.Region as Region
import Utility.Color as Color
import Utility.Image as Image
import Utility.ThemeColor as ThemeColor
import Utility.Truncate as Truncate


view :
    { model | images : Images, theme : Theme }
    ->
        { onPress : TokenParam -> msg
        , onMouseEnter : tooltip -> msg
        , onMouseLeave : msg
        , tooltip : TokenParam -> tooltip
        , opened : Maybe tooltip
        , tokenParam : TokenParam
        , token : Maybe Token
        }
    -> Element msg
view { images, theme } param =
    param.token
        |> Maybe.map
            (\token ->
                Input.button
                    [ Region.description
                        (case param.tokenParam of
                            TokenParam.Asset ->
                                "asset button"

                            TokenParam.Collateral ->
                                "collateral button"
                        )
                    , width fill
                    , height <| px 44
                    , theme |> ThemeColor.btnBackground |> Background.color
                    , Border.width 1
                    , theme |> ThemeColor.border |> Border.color
                    , Border.rounded 8
                    ]
                    { onPress = param.onPress param.tokenParam |> Just
                    , label =
                        row
                            [ width fill
                            , height fill
                            , paddingXY 12 0
                            , spacing 6
                            ]
                            [ images
                                |> Image.viewToken
                                    [ width <| px 24
                                    , alignLeft
                                    , centerY
                                    ]
                                    token
                            , Truncate.viewSymbol
                                { onMouseEnter = param.onMouseEnter
                                , onMouseLeave = param.onMouseLeave
                                , tooltip = param.tooltip param.tokenParam
                                , opened = param.opened
                                , token = token
                                , theme = theme
                                }
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
                                    ]
                            ]
                    }
            )
        |> Maybe.withDefault
            (Input.button
                [ Region.description
                    (case param.tokenParam of
                        TokenParam.Asset ->
                            "asset button"

                        TokenParam.Collateral ->
                            "collateral button"
                    )
                , width fill
                , height <| px 44
                , theme |> ThemeColor.primaryBtn |> Background.color
                , Border.rounded 8
                ]
                { onPress = param.onPress param.tokenParam |> Just
                , label =
                    row
                        [ width fill
                        , height fill
                        , paddingXY 12 0
                        ]
                        [ el
                            [ width shrink
                            , height shrink
                            , alignLeft
                            , centerY
                            , Font.size 14
                            , Color.light100 |> Font.color
                            ]
                            (text "Select Token")
                        , images
                            |> Image.discloser
                                [ width <| px 9
                                , alignRight
                                , centerY
                                ]
                        ]
                }
            )
