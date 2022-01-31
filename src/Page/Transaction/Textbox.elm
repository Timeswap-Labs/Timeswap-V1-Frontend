module Page.Transaction.Textbox exposing (disabled, empty, view)

import Data.Images exposing (Images)
import Data.Or exposing (Or(..))
import Data.Theme exposing (Theme)
import Data.Token exposing (Token)
import Data.Uint as Uint exposing (Uint)
import Element
    exposing
        ( Element
        , alignLeft
        , alignRight
        , behindContent
        , centerY
        , el
        , fill
        , height
        , none
        , padding
        , paddingEach
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
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Utility.Color as Color
import Utility.Fade as Fade
import Utility.Image as Image
import Utility.ThemeColor as ThemeColor
import Utility.Truncate as Truncate


view :
    { model | images : Images, theme : Theme }
    ->
        { onMouseEnter : tooltip -> msg
        , onMouseLeave : msg
        , tooltip : tooltip
        , opened : Maybe tooltip
        , token : Token
        , onClick : Maybe msg
        , onChange : String -> msg
        , text : Or String Uint
        , description : String
        }
    -> Element msg
view { images, theme } param =
    el
        [ Region.description param.description
        , width fill
        , height <| px 44
        , paddingXY 12 0
        , Border.width 1
        , theme |> ThemeColor.textboxBorder |> Border.color
        , Border.rounded 8
        ]
        (Input.text
            ([ width fill
             , height shrink
             , centerY
             , Font.alignRight
             , spacing 12
             , Background.color Color.none
             , Border.width 0
             , Font.size 18
             , paddingXY 0 4
             , theme |> ThemeColor.text |> Font.color
             ]
                ++ (param.onClick
                        |> Maybe.map Events.onClick
                        |> Maybe.map List.singleton
                        |> Maybe.withDefault []
                   )
            )
            { onChange = param.onChange
            , text =
                case param.text of
                    Left string ->
                        string

                    Right _ ->
                        ""
            , placeholder =
                (case param.text of
                    Left _ ->
                        Input.placeholder
                            [ Font.size 16
                            , centerY
                            , Font.alignRight
                            , theme |> ThemeColor.placeholder |> Font.color
                            ]
                            (text "0.0")

                    Right uint ->
                        Input.placeholder
                            [ width fill
                            , height fill
                            , centerY
                            , Font.alignRight
                            ]
                            (Fade.view theme param.token uint)
                )
                    |> Just
            , label =
                Input.labelLeft
                    [ width shrink
                    , height fill
                    ]
                    (row
                        [ width <| px 80
                        , height shrink
                        , spacing 6
                        , centerY
                        ]
                        [ images
                            |> Image.viewToken
                                [ width <| px 24
                                , height <| px 24
                                ]
                                param.token
                        , Truncate.viewSymbol
                            { onMouseEnter = param.onMouseEnter
                            , onMouseLeave = param.onMouseLeave
                            , tooltip = param.tooltip
                            , opened = param.opened
                            , token = param.token
                            , theme = theme
                            , customStyles = []
                            }
                        ]
                    )
            }
        )


disabled :
    { model | images : Images, theme : Theme }
    ->
        { token : Token
        , text : String
        , description : String
        }
    -> Element Never
disabled { images, theme } param =
    row
        [ Region.description param.description
        , width fill
        , height <| px 44
        , paddingXY 12 0
        , spacing 12
        , Border.width 1
        , Border.color Color.primary100
        , Border.rounded 8
        ]
        [ row
            [ width <| px 100
            , height fill
            , paddingXY 0 12
            , spacing 6
            , alignLeft
            , centerY
            ]
            [ images
                |> Image.viewToken
                    [ width <| px 24
                    , height <| px 24
                    ]
                    param.token
            , Truncate.disabledSymbol param.token theme
            ]
        , el
            [ width fill
            , height fill
            , centerY
            , Font.size 16
            , paddingXY 0 4
            ]
            (text param.text)
        ]


empty : String -> Element Never
empty description =
    el
        [ Region.description description
        , width fill
        , height <| px 44
        , paddingXY 12 0
        , Border.width 1
        , Border.color Color.primary100
        , Border.rounded 8
        ]
        none
