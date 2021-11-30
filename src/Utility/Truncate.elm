module Utility.Truncate exposing
    ( disabled
    , fromBalance
    , fromSymbol
    , view
    )

import Data.Token as Token exposing (Token)
import Data.Uint as Uint exposing (Uint)
import Element
    exposing
        ( Color
        , Element
        , below
        , el
        , height
        , maximum
        , none
        , paddingEach
        , shrink
        , text
        , width
        )
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Utility.Color as Color
import Utility.Direction exposing (Direction)
import Utility.FontStyle as FontStyle exposing (FontStyle)
import Utility.Tooltip as Tooltip


type alias Truncated =
    { full : String
    , truncated : Maybe String
    }


view :
    { tooltip :
        { align : Direction ()
        , move : Direction Int
        , onMouseEnterMsg : tooltip -> msg
        , onMouseLeaveMsg : msg
        , given : tooltip
        , opened : Maybe tooltip
        }
    , main :
        { fontSize : Int
        , fontPadding : Int
        , fontColor : Color
        , texts : Truncated
        }
    }
    -> Element msg
view { tooltip, main } =
    construct
        { tooltip = Just tooltip
        , main = main
        }


disabled :
    { fontSize : Int
    , fontPadding : Int
    , fontColor : Color
    , texts : Truncated
    }
    -> Element Never
disabled param =
    construct
        { tooltip = Nothing
        , main = param
        }


construct :
    { tooltip :
        Maybe
            { align : Direction ()
            , move : Direction Int
            , onMouseEnterMsg : tooltip -> msg
            , onMouseLeaveMsg : msg
            , given : tooltip
            , opened : Maybe tooltip
            }
    , main :
        { fontSize : Int
        , fontPadding : Int
        , fontColor : Color
        , texts : Truncated
        }
    }
    -> Element msg
construct { tooltip, main } =
    el
        ([ width shrink
         , height shrink
         , paddingEach
            { top = main.fontPadding
            , right = 0
            , bottom =
                main.texts.truncated
                    |> Maybe.map
                        (\_ ->
                            main.fontPadding - 1
                        )
                    |> Maybe.withDefault
                        main.fontPadding
            , left = 0
            }
         , Font.regular
         , Font.size main.fontSize
         , Font.color Color.light100
         ]
            ++ (main.texts.truncated
                    |> Maybe.map
                        (\_ ->
                            [ Border.widthEach
                                { top = 0
                                , right = 0
                                , bottom = 1
                                , left = 0
                                }
                            , Border.dashed
                            , Border.color Color.transparent200
                            ]
                        )
                    |> Maybe.withDefault []
               )
            ++ (Maybe.map2
                    (\_ param ->
                        [ Events.onMouseEnter
                            (param.onMouseEnterMsg param.given)
                        , Events.onMouseLeave
                            param.onMouseLeaveMsg
                        , param.opened
                            |> Maybe.map
                                (\openedTooltip ->
                                    if
                                        openedTooltip
                                            == param.given
                                    then
                                        Tooltip.below
                                            { width =
                                                shrink
                                                    |> maximum 335
                                            , align = param.align
                                            , move = param.move
                                            , text = main.texts.full
                                            }

                                    else
                                        none
                                )
                            |> Maybe.withDefault none
                            |> below
                        ]
                    )
                    main.texts.truncated
                    tooltip
                    |> Maybe.withDefault []
               )
        )
        (main.texts.truncated
            |> Maybe.withDefault main.texts.full
            |> text
        )


fromBalance : Token -> Uint -> Truncated
fromBalance token uint =
    ( fromAmount token uint
    , fromSymbol token
    )
        |> (\( amount, symbol ) ->
                { full =
                    [ amount.full
                    , symbol.full
                    ]
                        |> String.join " "
                , truncated =
                    case ( amount.truncated, symbol.truncated ) of
                        ( Just truncatedAmount, Just truncatedSymbol ) ->
                            [ truncatedAmount
                            , truncatedSymbol
                            ]
                                |> String.join " "
                                |> Just

                        ( Just truncatedAmount, Nothing ) ->
                            [ truncatedAmount
                            , symbol.full
                            ]
                                |> String.join " "
                                |> Just

                        ( Nothing, Just truncatedSymbol ) ->
                            [ amount.full
                            , truncatedSymbol
                            ]
                                |> String.join " "
                                |> Just

                        _ ->
                            Nothing
                }
           )


fromAmount : Token -> Uint -> Truncated
fromAmount token uint =
    uint
        |> Uint.toAmount token
        |> (\full ->
                full
                    |> String.split "."
                    |> (\list ->
                            case list of
                                whole :: fraction :: _ ->
                                    if (whole |> String.length) > 15 then
                                        [ [ whole |> String.dropRight 15
                                          , whole
                                                |> String.dropRight 13
                                                |> String.right 2
                                          ]
                                            |> String.join "."
                                        , "Q"
                                        ]
                                            |> String.concat
                                            |> Just
                                            |> Truncated full

                                    else if (whole |> String.length) > 12 then
                                        [ [ whole |> String.dropRight 12
                                          , whole
                                                |> String.dropRight 10
                                                |> String.right 2
                                          ]
                                            |> String.join "."
                                        , "T"
                                        ]
                                            |> String.concat
                                            |> Just
                                            |> Truncated full

                                    else if (whole |> String.length) > 9 then
                                        [ [ whole |> String.dropRight 9
                                          , whole
                                                |> String.dropRight 7
                                                |> String.right 2
                                          ]
                                            |> String.join "."
                                        , "B"
                                        ]
                                            |> String.concat
                                            |> Just
                                            |> Truncated full

                                    else if (whole |> String.length) > 6 then
                                        [ [ whole |> String.dropRight 6
                                          , whole
                                                |> String.dropRight 4
                                                |> String.right 2
                                          ]
                                            |> String.join "."
                                        , "M"
                                        ]
                                            |> String.concat
                                            |> Just
                                            |> Truncated full

                                    else if (whole |> String.length) > 3 then
                                        [ whole |> String.dropRight 3
                                        , whole |> String.right 3
                                        ]
                                            |> String.join ","
                                            |> Just
                                            |> Truncated full

                                    else if (fraction |> String.length) <= 3 then
                                        Truncated full Nothing

                                    else
                                        [ whole
                                        , fraction |> String.left 3
                                        ]
                                            |> String.join "."
                                            |> Just
                                            |> Truncated full

                                whole :: _ ->
                                    if (whole |> String.length) > 15 then
                                        [ [ whole |> String.dropRight 15
                                          , whole
                                                |> String.dropRight 13
                                                |> String.right 2
                                          ]
                                            |> String.join "."
                                        , "Q"
                                        ]
                                            |> String.concat
                                            |> Just
                                            |> Truncated full

                                    else if (whole |> String.length) > 12 then
                                        [ [ whole |> String.dropRight 12
                                          , whole
                                                |> String.dropRight 10
                                                |> String.right 2
                                          ]
                                            |> String.join "."
                                        , "T"
                                        ]
                                            |> String.concat
                                            |> Just
                                            |> Truncated full

                                    else if (whole |> String.length) > 9 then
                                        [ [ whole |> String.dropRight 9
                                          , whole
                                                |> String.dropRight 7
                                                |> String.right 2
                                          ]
                                            |> String.join "."
                                        , "B"
                                        ]
                                            |> String.concat
                                            |> Just
                                            |> Truncated full

                                    else if (whole |> String.length) > 6 then
                                        [ [ whole |> String.dropRight 6
                                          , whole
                                                |> String.dropRight 4
                                                |> String.right 2
                                          ]
                                            |> String.join "."
                                        , "M"
                                        ]
                                            |> String.concat
                                            |> Just
                                            |> Truncated full

                                    else if (whole |> String.length) > 3 then
                                        [ whole |> String.dropRight 3
                                        , whole |> String.right 3
                                        ]
                                            |> String.join ","
                                            |> (\formattedFull -> Truncated formattedFull Nothing)

                                    else
                                        Truncated full Nothing

                                _ ->
                                    Truncated full Nothing
                       )
           )


fromSymbol : Token -> Truncated
fromSymbol token =
    token
        |> Token.toSymbol
        |> (\string ->
                { full = string
                , truncated =
                    if (string |> String.length) > 5 then
                        string
                            |> String.left 5
                            |> Just

                    else
                        Nothing
                }
           )
