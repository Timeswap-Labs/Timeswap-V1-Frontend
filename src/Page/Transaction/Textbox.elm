module Page.Transaction.Textbox exposing (disabled, empty, emptyNoInput, view)

import Data.Images exposing (Images)
import Data.Token exposing (Token)
import Element
    exposing
        ( Element
        , alignLeft
        , centerY
        , el
        , fill
        , height
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
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Utility.Color as Color
import Utility.Direction exposing (Direction)
import Utility.FontStyle as FontStyle
import Utility.Image as Image
import Utility.Truncate as Truncate


view :
    { model | images : Images }
    ->
        { tooltip :
            { align : Direction ()
            , move : Direction Int
            , onMouseEnterMsg : tooltip -> msg
            , onMouseLeaveMsg : msg
            , given : tooltip
            , opened : Maybe tooltip
            }
        , main :
            { onChange : String -> msg
            , token : Token
            , text : String
            , description : String
            }
        }
    -> Element msg
view { images } { tooltip, main } =
    el
        [ Region.description main.description
        , width fill
        , height <| px 44
        , paddingXY 12 0
        , Background.color Color.primary100
        , Border.rounded 8
        ]
        (Input.text
            [ width fill
            , height fill
            , paddingXY 0 14
            , spacing 12
            , Font.size 16
            ]
            { onChange = main.onChange
            , text = main.text
            , placeholder = Nothing
            , label =
                Input.labelLeft
                    [ width shrink
                    , height fill
                    ]
                    (row
                        [ width <| px 100
                        , height shrink
                        , spacing 6
                        , centerY
                        ]
                        [ images
                            |> Image.viewToken
                                [ width <| px 24
                                , height <| px 24
                                ]
                                main.token
                        , el
                            [ width shrink
                            , height shrink
                            ]
                            (Truncate.view
                                { tooltip = tooltip
                                , main =
                                    { fontSize = 18
                                    , fontPadding = 3
                                    , fontColor = Color.light100
                                    , texts =
                                        main.token
                                            |> Truncate.fromSymbol
                                    }
                                }
                            )
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
        , Background.color Color.primary100
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
            , Truncate.disabled
                { fontSize = 18
                , fontPadding = 3
                , fontColor = Color.light100
                , texts =
                    param.token
                        |> Truncate.fromSymbol
                }
            ]
        , el
            [ width fill
            , height fill
            , padding 14
            , Font.size 16
            ]
            (text param.text)
        ]


empty :
    { model | images : Images }
    ->
        { token : Maybe Token
        , description : String
        }
    -> Element Never
empty { images } param =
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
        (param.token
            |> Maybe.map
                (\token ->
                    [ row
                        [ width <| px 100
                        , height shrink
                        , spacing 6
                        , centerY
                        ]
                        [ images
                            |> Image.viewToken
                                [ width <| px 24
                                , height <| px 24
                                ]
                                token
                        , Truncate.disabled
                            { fontSize = 18
                            , fontPadding = 3
                            , fontColor = Color.light100
                            , texts =
                                token
                                    |> Truncate.fromSymbol
                            }
                        ]
                    , el
                        [ width fill
                        , height fill
                        , paddingXY 0 14
                        , Font.size 16
                        , Font.color Color.transparent100
                        ]
                        (text "0.0")
                    ]
                )
            |> Maybe.withDefault
                [ el
                    [ width shrink
                    , height shrink
                    , alignLeft
                    , centerY
                    ]
                    none
                ]
        )


emptyNoInput :
    { model | images : Images }
    ->
        { token : Maybe Token
        , description : String
        }
    -> Element Never
emptyNoInput { images } param =
    row
        [ Region.description param.description
        , width fill
        , height <| px 44
        , spacing 6
        , paddingXY 12 0
        , Background.color Color.primary100
        , Border.rounded 8
        ]
        (param.token
            |> Maybe.map
                (\token ->
                    [ images
                        |> Image.viewToken
                            [ width <| px 24
                            , height <| px 24
                            ]
                            token
                    , Truncate.disabled
                        { fontSize = 18
                        , fontPadding = 3
                        , fontColor = Color.light100
                        , texts =
                            token
                                |> Truncate.fromSymbol
                        }
                    ]
                )
            |> Maybe.withDefault
                [ el
                    [ width shrink
                    , height shrink
                    , alignLeft
                    , centerY
                    ]
                    none
                ]
        )
