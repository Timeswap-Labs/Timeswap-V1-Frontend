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
        ( Element
        , below
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
                , Border.color Color.transparent200
                , Events.onMouseEnter
                    (param.onMouseEnter param.tooltip)
                , Events.onMouseLeave param.onMouseLeave
                , (if opened == param.tooltip then
                    el
                        [ Font.size param.fontSize
                        , Font.color Color.transparent300
                        ]
                        (text full)
                        |> Tooltip.belowAlignLeft

                   else
                    none
                  )
                    |> below
                , Font.size 14
                , Font.color Color.transparent500
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
                , Border.color Color.transparent200
                , Events.onMouseEnter
                    (param.onMouseEnter param.tooltip)
                , Events.onMouseLeave param.onMouseLeave
                , Font.size param.fontSize
                , Font.color Color.transparent500
                ]
                (text short)

        ( ( full, Nothing ), _ ) ->
            el
                [ width shrink
                , height shrink
                , paddingXY 0 param.fontPadding
                , Font.size param.fontSize
                , Font.color Color.transparent500
                ]
                (text full)


viewSymbol :
    { onMouseEnter : tooltip -> msg
    , onMouseLeave : msg
    , tooltip : tooltip
    , opened : Maybe tooltip
    , token : Token
    , theme : Theme
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
                , Border.color Color.transparent200
                , Events.onMouseEnter
                    (param.onMouseEnter param.tooltip)
                , Events.onMouseLeave param.onMouseLeave
                , (if opened == param.tooltip then
                    el
                        [ Font.size 14
                        , Font.color Color.transparent300
                        ]
                        (text full)
                        |> Tooltip.belowAlignLeft

                   else
                    none
                  )
                    |> below
                , Font.size 16
                , param.theme |> ThemeColor.text |> Font.color
                ]
                (text short)

        ( ( _, Just short ), Nothing ) ->
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
                , Border.color Color.transparent200
                , Events.onMouseEnter
                    (param.onMouseEnter param.tooltip)
                , Events.onMouseLeave param.onMouseLeave
                , Font.size 16
                , param.theme |> ThemeColor.text |> Font.color
                ]
                (text short)

        ( ( full, Nothing ), _ ) ->
            el
                [ width shrink
                , height shrink
                , paddingXY 0 4
                , Font.size 16
                , param.theme |> ThemeColor.text |> Font.color
                ]
                (text full)


viewName :
    { onMouseEnter : tooltip -> msg
    , onMouseLeave : msg
    , tooltip : tooltip
    , opened : Maybe tooltip
    , token : Token
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
                , Border.color Color.transparent200
                , Events.onMouseEnter
                    (param.onMouseEnter param.tooltip)
                , Events.onMouseLeave param.onMouseLeave
                , (if opened == param.tooltip then
                    el
                        [ Font.size 12
                        , Font.color Color.transparent300
                        ]
                        (text full)
                        |> Tooltip.belowAlignLeft

                   else
                    none
                  )
                    |> below
                , Font.size 12
                , Font.color Color.transparent300
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
                , Border.color Color.transparent200
                , Events.onMouseEnter
                    (param.onMouseEnter param.tooltip)
                , Events.onMouseLeave param.onMouseLeave
                , Font.size 12
                , Font.color Color.transparent300
                ]
                (text short)

        ( ( full, Nothing ), _ ) ->
            el
                [ width shrink
                , height shrink
                , paddingXY 0 2
                , Font.size 12
                , Font.color Color.transparent300
                ]
                (text full)


viewCDPSymbol :
    { onMouseEnter : tooltip -> msg
    , onMouseLeave : msg
    , tooltip : tooltip
    , opened : Maybe tooltip
    , pair : Pair
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
                , Border.color Color.transparent200
                , Events.onMouseEnter
                    (param.onMouseEnter param.tooltip)
                , Events.onMouseLeave param.onMouseLeave
                , (if opened == param.tooltip then
                    el
                        [ Font.size 14
                        , Font.color Color.transparent300
                        ]
                        (text full)
                        |> Tooltip.belowAlignLeft

                   else
                    none
                  )
                    |> below
                , Font.size 12
                , Font.color Color.transparent300
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
                , Border.color Color.transparent200
                , Events.onMouseEnter
                    (param.onMouseEnter param.tooltip)
                , Events.onMouseLeave param.onMouseLeave
                , Font.size 12
                , Font.color Color.transparent300
                ]
                (text short)

        ( ( full, Nothing ), _ ) ->
            el
                [ width shrink
                , height shrink
                , paddingXY 0 2
                , Font.size 12
                , Font.color Color.transparent300
                ]
                (text full)


viewCDP :
    { onMouseEnter : tooltip -> msg
    , onMouseLeave : msg
    , tooltip : tooltip
    , opened : Maybe tooltip
    , pair : Pair
    , cdp : Uint
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
                [ width shrink
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
                , Border.color Color.transparent200
                , Events.onMouseEnter
                    (param.onMouseEnter param.tooltip)
                , Events.onMouseLeave param.onMouseLeave
                , (if opened == param.tooltip then
                    el
                        [ Font.size 12
                        , Font.color Color.transparent300
                        ]
                        (text full)
                        |> Tooltip.belowAlignRight

                   else
                    none
                  )
                    |> below
                , Font.size 18
                , (if param.cdp |> Uint.isZero then
                    Color.transparent200

                   else
                    Color.warning400
                  )
                    |> Font.color
                ]
                (text short)

        ( ( _, Just short ), Nothing ) ->
            el
                [ width shrink
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
                , Border.color Color.transparent200
                , Events.onMouseEnter
                    (param.onMouseEnter param.tooltip)
                , Events.onMouseLeave param.onMouseLeave
                , Font.size 18
                , Font.color Color.warning400
                ]
                (text short)

        ( ( full, Nothing ), _ ) ->
            el
                [ width shrink
                , height shrink
                , paddingXY 0 3
                , Font.size 18
                , Font.color Color.warning400
                ]
                (text full)


viewAmount :
    { onMouseEnter : tooltip -> msg
    , onMouseLeave : msg
    , tooltip : tooltip
    , opened : Maybe tooltip
    , token : Token
    , amount : Uint
    , theme : Theme
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
                , Border.color Color.transparent200
                , Events.onMouseEnter
                    (param.onMouseEnter param.tooltip)
                , Events.onMouseLeave param.onMouseLeave
                , (if opened == param.tooltip then
                    el
                        [ Font.size 12
                        , Font.color Color.transparent300
                        ]
                        (text full)
                        |> Tooltip.belowAlignRight

                   else
                    none
                  )
                    |> below
                , Font.size 16
                , param.theme |> ThemeColor.text |> Font.color
                ]
                (text short)

        ( ( _, Just short ), Nothing ) ->
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
                , Border.color Color.transparent200
                , Events.onMouseEnter
                    (param.onMouseEnter param.tooltip)
                , Events.onMouseLeave param.onMouseLeave
                , Font.size 16
                , param.theme |> ThemeColor.text |> Font.color
                ]
                (text short)

        ( ( full, Nothing ), _ ) ->
            el
                [ width shrink
                , height shrink
                , paddingXY 0 4
                , Font.size 16
                , param.theme |> ThemeColor.text |> Font.color
                ]
                (text full)


viewBalance :
    { onMouseEnter : tooltip -> msg
    , onMouseLeave : msg
    , tooltip : tooltip
    , opened : Maybe tooltip
    , token : Token
    , balance : Uint
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
                , Border.color Color.transparent200
                , Events.onMouseEnter
                    (param.onMouseEnter param.tooltip)
                , Events.onMouseLeave param.onMouseLeave
                , (if opened == param.tooltip then
                    el
                        [ Font.size 12
                        , Font.color Color.transparent300
                        ]
                        (text full)
                        |> Tooltip.belowAlignRight

                   else
                    none
                  )
                    |> below
                , Font.size 12
                , Font.color Color.transparent300
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
                , Border.color Color.transparent200
                , Events.onMouseEnter
                    (param.onMouseEnter param.tooltip)
                , Events.onMouseLeave param.onMouseLeave
                , Font.size 12
                , Font.color Color.transparent300
                ]
                (text short)

        ( ( full, Nothing ), _ ) ->
            el
                [ width shrink
                , height shrink
                , paddingXY 0 4
                , Font.size 12
                , Font.color Color.transparent300
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
                , Border.color Color.transparent200
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
                , Border.color Color.transparent200
                , Font.size 12
                , Font.color Color.transparent300
                ]
                (text short)

        ( full, Nothing ) ->
            el
                [ width shrink
                , height shrink
                , paddingXY 0 4
                , Font.size 12
                , Font.color Color.transparent300
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
                ( string
                , if (string |> String.length) > 5 then
                    string
                        |> String.left 5
                        |> Just

                  else
                    Nothing
                )
           )


fromCDPSymbol : Pair -> ( String, Maybe String )
fromCDPSymbol pair =
    (\collateral asset ->
        ( [ collateral
          , "per"
          , asset
          ]
            |> String.join " "
        , if
            (collateral |> String.length)
                > 5
                || (asset |> String.length)
                > 5
          then
            [ collateral |> String.left 5
            , "per"
            , asset |> String.left 5
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
        ( [ collateral
          , asset
          ]
            |> String.join " - "
        , if
            (collateral |> String.length)
                > 5
                || (asset |> String.length)
                > 5
          then
            [ collateral |> String.left 5
            , asset |> String.left 5
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
