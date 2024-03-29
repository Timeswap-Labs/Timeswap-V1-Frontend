module Utility.Truncate exposing
    ( disabledBalance
    , disabledSymbol
    , viewAmount
    , viewBalance
    , viewCDP
    , viewCDPSymbol
    , viewName
    , viewPairSymbol
    , viewSymbol
    )

import Data.Pair as Pair exposing (Pair)
import Data.Theme exposing (Theme)
import Data.Token as Token exposing (Token)
import Data.Uint as Uint exposing (Uint)
import Element
    exposing
        ( Attribute
        , Element
        , below
        , centerY
        , el
        , height
        , none
        , paddingEach
        , paddingXY
        , shrink
        , text
        , width
        )
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Utility.Color as Color
import Utility.ThemeColor as ThemeColor
import Utility.Tooltip as Tooltip


viewPairSymbol :
    { onMouseEnter : tooltip -> msg
    , onMouseLeave : msg
    , tooltip : tooltip
    , opened : Maybe tooltip
    , pair : Pair
    , fontSize : Int
    , fontPadding : Int
    , theme : Theme
    }
    -> Element msg
viewPairSymbol param =
    case
        ( fromPairSymbol param.pair
        , param.opened
        )
    of
        ( ( full, Just short ), Just opened ) ->
            el
                [ width shrink
                , height shrink
                , paddingEach
                    { top = param.fontPadding
                    , right = 0
                    , bottom = param.fontPadding - 1
                    , left = 0
                    }
                , Border.widthEach
                    { top = 0
                    , right = 0
                    , bottom = 1
                    , left = 0
                    }
                , Border.dashed
                , param.theme |> ThemeColor.placeholder |> Border.color
                , Events.onMouseEnter
                    (param.onMouseEnter param.tooltip)
                , Events.onMouseLeave param.onMouseLeave
                , (if opened == param.tooltip then
                    el
                        [ Font.size param.fontSize
                        , param.theme |> ThemeColor.textLight |> Font.color
                        ]
                        (text full)
                        |> Tooltip.belowAlignLeft param.theme

                   else
                    none
                  )
                    |> below
                , Font.size param.fontSize
                , param.theme |> ThemeColor.text |> Font.color
                ]
                (text short)

        ( ( _, Just short ), Nothing ) ->
            el
                [ width shrink
                , height shrink
                , paddingEach
                    { top = param.fontPadding
                    , right = 0
                    , bottom = param.fontPadding - 1
                    , left = 0
                    }
                , Border.widthEach
                    { top = 0
                    , right = 0
                    , bottom = 1
                    , left = 0
                    }
                , Border.dashed
                , param.theme |> ThemeColor.placeholder |> Border.color
                , Events.onMouseEnter
                    (param.onMouseEnter param.tooltip)
                , Events.onMouseLeave param.onMouseLeave
                , Font.size param.fontSize
                , param.theme |> ThemeColor.text |> Font.color
                ]
                (text short)

        ( ( full, Nothing ), _ ) ->
            el
                [ width shrink
                , height shrink
                , paddingXY 0 param.fontPadding
                , Font.size param.fontSize
                , param.theme |> ThemeColor.text |> Font.color
                ]
                (text full)


viewSymbol :
    { onMouseEnter : tooltip -> msg
    , onMouseLeave : msg
    , tooltip : tooltip
    , opened : Maybe tooltip
    , token : Token
    , theme : Theme
    , customStyles : List (Attribute msg)
    }
    -> Element msg
viewSymbol param =
    case
        ( fromSymbol param.token
        , param.opened
        )
    of
        ( ( full, Just short ), Just opened ) ->
            el
                ([ width shrink
                 , height shrink
                 , paddingEach
                    { top = 4
                    , right = 0
                    , bottom = 3
                    , left = 0
                    }
                 , Border.widthEach
                    { top = 0
                    , right = 0
                    , bottom = 1
                    , left = 0
                    }
                 , Border.dashed
                 , param.theme |> ThemeColor.placeholder |> Border.color
                 , Events.onMouseEnter
                    (param.onMouseEnter param.tooltip)
                 , Events.onMouseLeave param.onMouseLeave
                 , (if opened == param.tooltip then
                        el
                            [ Font.size 14
                            , param.theme |> ThemeColor.textLight |> Font.color
                            ]
                            (text full)
                            |> Tooltip.belowAlignLeft param.theme

                    else
                        none
                   )
                    |> below
                 , Font.size 16
                 , param.theme |> ThemeColor.text |> Font.color
                 ]
                    ++ param.customStyles
                )
                (text short)

        ( ( _, Just short ), Nothing ) ->
            el
                ([ width shrink
                 , height shrink
                 , paddingEach
                    { top = 4
                    , right = 0
                    , bottom = 3
                    , left = 0
                    }
                 , Border.widthEach
                    { top = 0
                    , right = 0
                    , bottom = 1
                    , left = 0
                    }
                 , Border.dashed
                 , param.theme |> ThemeColor.placeholder |> Border.color
                 , Events.onMouseEnter
                    (param.onMouseEnter param.tooltip)
                 , Events.onMouseLeave param.onMouseLeave
                 , Font.size 16
                 , param.theme |> ThemeColor.text |> Font.color
                 ]
                    ++ param.customStyles
                )
                (text short)

        ( ( full, Nothing ), _ ) ->
            el
                ([ width shrink
                 , height shrink
                 , paddingXY 0 4
                 , Font.size 16
                 , param.theme |> ThemeColor.text |> Font.color
                 ]
                    ++ param.customStyles
                )
                (text full)


viewName :
    { onMouseEnter : tooltip -> msg
    , onMouseLeave : msg
    , tooltip : tooltip
    , opened : Maybe tooltip
    , token : Token
    , theme : Theme
    }
    -> Element msg
viewName param =
    case
        ( fromName param.token
        , param.opened
        )
    of
        ( ( full, Just short ), Just opened ) ->
            el
                [ width shrink
                , height shrink
                , paddingEach
                    { top = 2
                    , right = 0
                    , bottom = 1
                    , left = 0
                    }
                , Border.widthEach
                    { top = 0
                    , right = 0
                    , bottom = 1
                    , left = 0
                    }
                , Border.dashed
                , param.theme |> ThemeColor.placeholder |> Border.color
                , Events.onMouseEnter
                    (param.onMouseEnter param.tooltip)
                , Events.onMouseLeave param.onMouseLeave
                , (if opened == param.tooltip then
                    el
                        [ Font.size 12
                        , param.theme |> ThemeColor.textLight |> Font.color
                        ]
                        (text full)
                        |> Tooltip.belowAlignLeft param.theme

                   else
                    none
                  )
                    |> below
                , Font.size 12
                , param.theme |> ThemeColor.textLight |> Font.color
                ]
                (text short)

        ( ( _, Just short ), Nothing ) ->
            el
                [ width shrink
                , height shrink
                , paddingEach
                    { top = 2
                    , right = 0
                    , bottom = 1
                    , left = 0
                    }
                , Border.widthEach
                    { top = 0
                    , right = 0
                    , bottom = 1
                    , left = 0
                    }
                , Border.dashed
                , param.theme |> ThemeColor.placeholder |> Border.color
                , Events.onMouseEnter
                    (param.onMouseEnter param.tooltip)
                , Events.onMouseLeave param.onMouseLeave
                , Font.size 12
                , param.theme |> ThemeColor.textLight |> Font.color
                ]
                (text short)

        ( ( full, Nothing ), _ ) ->
            el
                [ width shrink
                , height shrink
                , paddingXY 0 2
                , Font.size 12
                , param.theme |> ThemeColor.textLight |> Font.color
                ]
                (text full)


viewCDPSymbol :
    { onMouseEnter : tooltip -> msg
    , onMouseLeave : msg
    , tooltip : tooltip
    , opened : Maybe tooltip
    , pair : Pair
    , theme : Theme
    }
    -> Element msg
viewCDPSymbol param =
    case
        ( fromCDPSymbol param.pair
        , param.opened
        )
    of
        ( ( full, Just short ), Just opened ) ->
            el
                [ width shrink
                , height shrink
                , centerY
                , paddingEach
                    { top = 2
                    , right = 0
                    , bottom = 1
                    , left = 0
                    }
                , Border.widthEach
                    { top = 0
                    , right = 0
                    , bottom = 1
                    , left = 0
                    }
                , Border.dashed
                , param.theme |> ThemeColor.placeholder |> Border.color
                , Events.onMouseEnter
                    (param.onMouseEnter param.tooltip)
                , Events.onMouseLeave param.onMouseLeave
                , (if opened == param.tooltip then
                    el
                        [ Font.size 14
                        , param.theme |> ThemeColor.textLight |> Font.color
                        ]
                        (text full)
                        |> Tooltip.belowAlignLeft param.theme

                   else
                    none
                  )
                    |> below
                , Font.size 12
                , param.theme |> ThemeColor.textLight |> Font.color
                ]
                (text short)

        ( ( _, Just short ), Nothing ) ->
            el
                [ width shrink
                , height shrink
                , centerY
                , paddingEach
                    { top = 2
                    , right = 0
                    , bottom = 1
                    , left = 0
                    }
                , Border.widthEach
                    { top = 0
                    , right = 0
                    , bottom = 1
                    , left = 0
                    }
                , Border.dashed
                , param.theme |> ThemeColor.placeholder |> Border.color
                , Events.onMouseEnter
                    (param.onMouseEnter param.tooltip)
                , Events.onMouseLeave param.onMouseLeave
                , Font.size 12
                , param.theme |> ThemeColor.textLight |> Font.color
                ]
                (text short)

        ( ( full, Nothing ), _ ) ->
            el
                [ width shrink
                , height shrink
                , centerY
                , paddingXY 0 2
                , Font.size 12
                , param.theme |> ThemeColor.textLight |> Font.color
                ]
                (text full)


viewCDP :
    { onMouseEnter : tooltip -> msg
    , onMouseLeave : msg
    , tooltip : tooltip
    , opened : Maybe tooltip
    , pair : Pair
    , cdp : Uint
    , theme : Theme
    , styles : List (Attribute msg)
    }
    -> Element msg
viewCDP param =
    case
        ( param.cdp
            |> fromAmount
                (param.pair |> Pair.toCollateral)
        , param.opened
        )
    of
        ( ( full, Just short ), Just opened ) ->
            el
                ([ width shrink
                 , height shrink
                 , paddingEach
                    { top = 3
                    , right = 0
                    , bottom = 2
                    , left = 0
                    }
                 , Border.widthEach
                    { top = 0
                    , right = 0
                    , bottom = 1
                    , left = 0
                    }
                 , Border.dashed
                 , param.theme |> ThemeColor.placeholder |> Border.color
                 , Events.onMouseEnter
                    (param.onMouseEnter param.tooltip)
                 , Events.onMouseLeave param.onMouseLeave
                 , (if opened == param.tooltip then
                        el
                            [ Font.size 12
                            , param.theme |> ThemeColor.textLight |> Font.color
                            ]
                            (text full)
                            |> Tooltip.belowAlignRight param.theme

                    else
                        none
                   )
                    |> below
                 , Font.size 18
                 , (if param.cdp |> Uint.isZero then
                        Color.transparent200

                    else
                        Color.warning500
                   )
                    |> Font.color
                 ]
                    ++ param.styles
                )
                (text short)

        ( ( _, Just short ), Nothing ) ->
            el
                ([ width shrink
                 , height shrink
                 , paddingEach
                    { top = 3
                    , right = 0
                    , bottom = 2
                    , left = 0
                    }
                 , Border.widthEach
                    { top = 0
                    , right = 0
                    , bottom = 1
                    , left = 0
                    }
                 , Border.dashed
                 , param.theme |> ThemeColor.placeholder |> Border.color
                 , Events.onMouseEnter
                    (param.onMouseEnter param.tooltip)
                 , Events.onMouseLeave param.onMouseLeave
                 , Font.size 18
                 , Font.color Color.warning500
                 ]
                    ++ param.styles
                )
                (text short)

        ( ( full, Nothing ), _ ) ->
            el
                ([ width shrink
                 , height shrink
                 , paddingXY 0 3
                 , Font.size 18
                 , Font.color Color.warning500
                 ]
                    ++ param.styles
                )
                (text full)


viewAmount :
    { onMouseEnter : tooltip -> msg
    , onMouseLeave : msg
    , tooltip : tooltip
    , opened : Maybe tooltip
    , token : Token
    , amount : Uint
    , theme : Theme
    , customStyles : List (Attribute msg)
    }
    -> Element msg
viewAmount param =
    case
        ( param.amount |> fromAmount param.token
        , param.opened
        )
    of
        ( ( full, Just short ), Just opened ) ->
            el
                ([ width shrink
                 , height shrink
                 , paddingEach
                    { top = 4
                    , right = 0
                    , bottom = 3
                    , left = 0
                    }
                 , Border.widthEach
                    { top = 0
                    , right = 0
                    , bottom = 1
                    , left = 0
                    }
                 , Border.dashed
                 , param.theme |> ThemeColor.placeholder |> Border.color
                 , Events.onMouseEnter
                    (param.onMouseEnter param.tooltip)
                 , Events.onMouseLeave param.onMouseLeave
                 , (if opened == param.tooltip then
                        el
                            [ Font.size 12
                            , param.theme |> ThemeColor.textLight |> Font.color
                            ]
                            (text full)
                            |> Tooltip.belowAlignRight param.theme

                    else
                        none
                   )
                    |> below
                 , Font.size 16
                 , param.theme |> ThemeColor.text |> Font.color
                 ]
                    ++ param.customStyles
                )
                (text short)

        ( ( _, Just short ), Nothing ) ->
            el
                ([ width shrink
                 , height shrink
                 , paddingEach
                    { top = 4
                    , right = 0
                    , bottom = 3
                    , left = 0
                    }
                 , Border.widthEach
                    { top = 0
                    , right = 0
                    , bottom = 1
                    , left = 0
                    }
                 , Border.dashed
                 , param.theme |> ThemeColor.placeholder |> Border.color
                 , Events.onMouseEnter
                    (param.onMouseEnter param.tooltip)
                 , Events.onMouseLeave param.onMouseLeave
                 , Font.size 16
                 , param.theme |> ThemeColor.text |> Font.color
                 ]
                    ++ param.customStyles
                )
                (text short)

        ( ( full, Nothing ), _ ) ->
            el
                ([ width shrink
                 , height shrink
                 , paddingXY 0 4
                 , Font.size 16
                 , param.theme |> ThemeColor.text |> Font.color
                 ]
                    ++ param.customStyles
                )
                (text full)


viewBalance :
    { onMouseEnter : tooltip -> msg
    , onMouseLeave : msg
    , tooltip : tooltip
    , opened : Maybe tooltip
    , token : Token
    , balance : Uint
    , theme : Theme
    }
    -> Element msg
viewBalance param =
    case
        ( param.balance |> fromBalance param.token
        , param.opened
        )
    of
        ( ( full, Just short ), Just opened ) ->
            el
                [ width shrink
                , height shrink
                , paddingEach
                    { top = 2
                    , right = 0
                    , bottom = 1
                    , left = 0
                    }
                , Border.widthEach
                    { top = 0
                    , right = 0
                    , bottom = 1
                    , left = 0
                    }
                , Border.dashed
                , param.theme |> ThemeColor.placeholder |> Border.color
                , Events.onMouseEnter
                    (param.onMouseEnter param.tooltip)
                , Events.onMouseLeave param.onMouseLeave
                , (if opened == param.tooltip then
                    el
                        [ Font.size 12
                        , param.theme |> ThemeColor.textLight |> Font.color
                        ]
                        (text full)
                        |> Tooltip.belowAlignRight param.theme

                   else
                    none
                  )
                    |> below
                , Font.size 12
                , param.theme |> ThemeColor.textLight |> Font.color
                ]
                (text short)

        ( ( _, Just short ), Nothing ) ->
            el
                [ width shrink
                , height shrink
                , paddingEach
                    { top = 2
                    , right = 0
                    , bottom = 1
                    , left = 0
                    }
                , Border.widthEach
                    { top = 0
                    , right = 0
                    , bottom = 1
                    , left = 0
                    }
                , Border.dashed
                , param.theme |> ThemeColor.placeholder |> Border.color
                , Events.onMouseEnter
                    (param.onMouseEnter param.tooltip)
                , Events.onMouseLeave param.onMouseLeave
                , Font.size 12
                , param.theme |> ThemeColor.textLight |> Font.color
                ]
                (text short)

        ( ( full, Nothing ), _ ) ->
            el
                [ width shrink
                , height shrink
                , paddingXY 0 4
                , Font.size 12
                , param.theme |> ThemeColor.textLight |> Font.color
                ]
                (text full)


disabledSymbol : Token -> Theme -> Element Never
disabledSymbol token theme =
    case token |> fromSymbol of
        ( _, Just short ) ->
            el
                [ width shrink
                , height shrink
                , paddingEach
                    { top = 4
                    , right = 0
                    , bottom = 3
                    , left = 0
                    }
                , Border.widthEach
                    { top = 0
                    , right = 0
                    , bottom = 1
                    , left = 0
                    }
                , Border.dashed
                , theme |> ThemeColor.placeholder |> Border.color
                , Font.size 16
                , theme |> ThemeColor.text |> Font.color
                ]
                (text short)

        ( full, Nothing ) ->
            el
                [ width shrink
                , height shrink
                , paddingXY 0 4
                , Font.size 16
                , theme |> ThemeColor.text |> Font.color
                ]
                (text full)


disabledBalance :
    { token : Token
    , balance : Uint
    , theme : Theme
    }
    -> Element Never
disabledBalance param =
    case param.balance |> fromBalance param.token of
        ( _, Just short ) ->
            el
                [ width shrink
                , height shrink
                , paddingEach
                    { top = 2
                    , right = 0
                    , bottom = 1
                    , left = 0
                    }
                , Border.widthEach
                    { top = 0
                    , right = 0
                    , bottom = 1
                    , left = 0
                    }
                , Border.dashed
                , param.theme |> ThemeColor.placeholder |> Border.color
                , Font.size 12
                , param.theme |> ThemeColor.textLight |> Font.color
                ]
                (text short)

        ( full, Nothing ) ->
            el
                [ width shrink
                , height shrink
                , paddingXY 0 4
                , Font.size 12
                , param.theme |> ThemeColor.textLight |> Font.color
                ]
                (text full)


fromBalance : Token -> Uint -> ( String, Maybe String )
fromBalance token uint =
    ( fromAmount token uint
    , fromSymbol token
    )
        |> (\( amount, symbol ) ->
                ( [ amount |> Tuple.first
                  , symbol |> Tuple.first
                  ]
                    |> String.join " "
                , case ( amount |> Tuple.second, symbol |> Tuple.second ) of
                    ( Just shortAmount, Just shortSymbol ) ->
                        [ shortAmount
                        , shortSymbol
                        ]
                            |> String.join " "
                            |> Just

                    ( Just shortAmount, Nothing ) ->
                        [ shortAmount
                        , symbol |> Tuple.first
                        ]
                            |> String.join " "
                            |> Just

                    ( Nothing, Just shortSymbol ) ->
                        [ amount |> Tuple.first
                        , shortSymbol
                        ]
                            |> String.join " "
                            |> Just

                    _ ->
                        Nothing
                )
           )


fromAmount : Token -> Uint -> ( String, Maybe String )
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
                                            |> Tuple.pair full

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
                                            |> Tuple.pair full

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
                                            |> Tuple.pair full

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
                                            |> Tuple.pair full

                                    else if (whole |> String.length) > 3 then
                                        [ whole |> String.dropRight 3
                                        , whole |> String.right 3
                                        ]
                                            |> String.join ","
                                            |> Just
                                            |> Tuple.pair full

                                    else if (fraction |> String.length) <= 3 then
                                        Tuple.pair full Nothing

                                    else
                                        [ whole
                                        , fraction |> String.left 3
                                        ]
                                            |> String.join "."
                                            |> Just
                                            |> Tuple.pair full

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
                                            |> Tuple.pair full

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
                                            |> Tuple.pair full

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
                                            |> Tuple.pair full

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
                                            |> Tuple.pair full

                                    else if (whole |> String.length) > 3 then
                                        [ whole |> String.dropRight 3
                                        , whole |> String.right 3
                                        ]
                                            |> String.join ","
                                            |> (\formattedFull -> Tuple.pair formattedFull Nothing)

                                    else
                                        Tuple.pair full Nothing

                                _ ->
                                    Tuple.pair full Nothing
                       )
           )


fromSymbol : Token -> ( String, Maybe String )
fromSymbol token =
    token
        |> Token.toSymbol
        |> (\string ->
                ( string |> String.toUpper
                , if (string |> String.length) > 6 then
                    string
                        |> String.left 6
                        |> String.toUpper
                        |> Just

                  else
                    Nothing
                )
           )


fromCDPSymbol : Pair -> ( String, Maybe String )
fromCDPSymbol pair =
    (\collateral asset ->
        ( [ collateral
          , "Per"
          , asset
          ]
            |> String.join " "
        , if
            (collateral |> String.length)
                > 6
                || (asset |> String.length)
                > 6
          then
            [ collateral |> String.left 6
            , "Per"
            , asset |> String.left 6
            ]
                |> String.join " "
                |> Just

          else
            Nothing
        )
    )
        (pair |> Pair.toCollateral |> Token.toSymbol)
        (pair |> Pair.toAsset |> Token.toSymbol)


fromPairSymbol : Pair -> ( String, Maybe String )
fromPairSymbol pair =
    (\collateral asset ->
        ( [ asset
          , collateral
          ]
            |> String.join " - "
        , if
            (collateral |> String.length)
                > 5
                || (asset |> String.length)
                > 5
          then
            [ asset |> String.left 6
            , collateral |> String.left 6
            ]
                |> String.join " - "
                |> Just

          else
            Nothing
        )
    )
        (pair |> Pair.toCollateral |> Token.toSymbol)
        (pair |> Pair.toAsset |> Token.toSymbol)


fromName : Token -> ( String, Maybe String )
fromName token =
    token
        |> Token.toName
        |> (\string ->
                ( string
                , if (string |> String.length) > 20 then
                    string
                        |> String.left 20
                        |> Just

                  else
                    Nothing
                )
           )
