module Page.Transaction.MaxButton exposing (disabled, view)

import Data.Remote as Remote exposing (Remote(..))
import Data.Theme exposing (Theme)
import Data.Token exposing (Token)
import Data.Uint exposing (Uint)
import Data.Web exposing (Web)
import Element
    exposing
        ( Element
        , alignRight
        , centerX
        , centerY
        , el
        , height
        , none
        , paddingXY
        , row
        , shrink
        , spacing
        , text
        , width
        )
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Utility.Color as Color
import Utility.Loading as Loading
import Utility.ThemeColor as ThemeColor
import Utility.Truncate as Truncate


view :
    { onPress : msg
    , onMouseEnter : tooltip -> msg
    , onMouseLeave : msg
    , tooltip : tooltip
    , opened : Maybe tooltip
    , token : Token
    , balance : Web Uint
    , theme : Theme
    }
    -> Element msg
view param =
    row
        [ width shrink
        , height shrink
        , spacing 6
        , alignRight
        , centerY
        ]
        [ userBalance param
        , param.balance
            |> Remote.map (\_ -> maxButton param)
            |> Remote.withDefault none
        ]


userBalance :
    { param
        | onMouseEnter : tooltip -> msg
        , onMouseLeave : msg
        , tooltip : tooltip
        , opened : Maybe tooltip
        , token : Token
        , balance : Web Uint
        , theme : Theme
    }
    -> Element msg
userBalance param =
    row
        [ width shrink
        , height shrink
        , alignRight
        , centerY
        ]
        [ el
            [ width shrink
            , height shrink
            , Font.size 12
            , paddingXY 0 2
            , param.theme |> ThemeColor.textLight |> Font.color
            ]
            (text "Bal: ")
        , case param.balance of
            Success balance ->
                Truncate.viewBalance
                    { onMouseEnter = param.onMouseEnter
                    , onMouseLeave = param.onMouseLeave
                    , tooltip = param.tooltip
                    , opened = param.opened
                    , token = param.token
                    , balance = balance
                    , theme = param.theme
                    }

            Loading timeline ->
                el
                    [ width shrink
                    , height shrink
                    , centerY
                    ]
                    (Loading.view timeline param.theme)

            Failure _ ->
                el
                    [ width shrink
                    , height shrink
                    , Font.size 12
                    , paddingXY 0 2
                    , Font.color Color.transparent300
                    ]
                    (text "error")
        ]


maxButton :
    { param | onPress : msg }
    -> Element msg
maxButton param =
    Input.button
        [ Region.description "max asset lend"
        , width shrink
        , height shrink
        , alignRight
        , centerY
        , Font.size 12
        , paddingXY 0 2
        , Font.color Color.warning400
        , Font.bold
        ]
        { onPress = Just param.onPress
        , label = text "MAX"
        }


disabled :
    { token : Token
    , balance : Web Uint
    , theme : Theme
    }
    -> Element Never
disabled param =
    row
        [ width shrink
        , height shrink
        , spacing 6
        , alignRight
        , centerY
        ]
        [ disabledUserBalance param
        , param.balance
            |> Remote.map (\_ -> disabledMaxButton)
            |> Remote.withDefault none
        ]


disabledUserBalance :
    { param
        | token : Token
        , balance : Web Uint
        , theme : Theme
    }
    -> Element Never
disabledUserBalance param =
    row
        [ width shrink
        , height shrink
        , alignRight
        , centerY
        ]
        [ el
            [ width shrink
            , height shrink
            , Font.size 12
            , paddingXY 0 2
            , param.theme |> ThemeColor.textLight |> Font.color
            ]
            (text "Bal: ")
        , case param.balance of
            Success balance ->
                Truncate.disabledBalance
                    { token = param.token
                    , balance = balance
                    , theme = param.theme
                    }

            Loading timeline ->
                el
                    [ width shrink
                    , height shrink
                    , centerX
                    , centerY
                    ]
                    (Loading.view timeline param.theme)

            Failure _ ->
                el
                    [ width shrink
                    , height shrink
                    , Font.size 12
                    , paddingXY 0 2
                    , Font.color Color.transparent300
                    ]
                    (text "error")
        ]


disabledMaxButton : Element Never
disabledMaxButton =
    el
        [ Region.description "max asset lend"
        , width shrink
        , height shrink
        , alignRight
        , centerY
        , Font.size 12
        , paddingXY 0 2
        , Font.color Color.warning400
        , Font.bold
        ]
        (text "MAX")
