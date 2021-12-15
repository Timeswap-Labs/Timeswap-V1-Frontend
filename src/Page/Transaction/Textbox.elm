module Page.Transaction.Textbox exposing (disabled, empty, view)

import Data.Images exposing (Images)
import Data.Or exposing (Or(..))
import Data.Token exposing (Token)
import Data.Uint as Uint exposing (Uint)
import Element
    exposing
        ( Element
        , alignLeft
        , behindContent
        , centerY
        , el
        , fill
        , height
        , moveUp
        , none
        , padding
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
import Utility.Image as Image
import Utility.Truncate as Truncate


view :
    { model | images : Images }
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
view { images } param =
    el
        [ Region.description param.description
        , width fill
        , height <| px 44
        , paddingXY 12 0
        , Border.width 1
        , Border.color Color.primary100
        , Border.rounded 8
        ]
        (Input.text
            ([ width fill
             , height shrink
             , centerY
             , paddingXY 0 4
             , spacing 12
             , Background.color Color.none
             , Border.width 0
             , Font.size 16
             , Font.color Color.light100
             , (case param.text of
                    Left _ ->
                        none

                    Right uint ->
                        el
                            [ width fill
                            , height fill
                            , paddingXY 0 4
                            , Font.size 16
                            , Font.color Color.light100
                            ]
                            (uint
                                |> Uint.toAmount param.token
                                |> text
                            )
               )
                |> behindContent
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
            , placeholder = Nothing
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
                            }
                        ]
                    )
            }
        )


disabled :
    { model | images : Images }
    ->
        { token : Token
        , text : String
        , description : String
        }
    -> Element Never
disabled { images } param =
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
            , paddingXY 0 14
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
            , Truncate.disabledSymbol param.token
            ]
        , el
            [ width fill
            , height fill
            , padding 14
            , Font.size 16
            ]
            (text param.text)
        ]


empty : String -> Element Never
empty description =
    el
        [ Region.description description
        , width fill
        , height <| px 44
        , spacing 6
        , paddingXY 12 0
        , Border.width 1
        , Border.color Color.primary100
        , Border.rounded 8
        ]
        none
