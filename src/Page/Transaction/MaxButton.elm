module Page.Transaction.MaxButton exposing (disabled, view)

import Data.Token exposing (Token)
import Data.Uint exposing (Uint)
import Element
    exposing
        ( Element
        , alignRight
        , centerY
        , el
        , height
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
import Utility.Truncate as Truncate


view :
    { onPress : msg
    , onMouseEnter : tooltip -> msg
    , onMouseLeave : msg
    , tooltip : tooltip
    , opened : Maybe tooltip
    , token : Token
    , balance : Uint
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
        , maxButton param
        ]


userBalance :
    { param
        | onMouseEnter : tooltip -> msg
        , onMouseLeave : msg
        , tooltip : tooltip
        , opened : Maybe tooltip
        , token : Token
        , balance : Uint
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
            , Font.color Color.transparent300
            ]
            (text "Bal: ")
        , Truncate.viewBalance
            { onMouseEnter = param.onMouseEnter
            , onMouseLeave = param.onMouseLeave
            , tooltip = param.tooltip
            , opened = param.opened
            , token = param.token
            , balance = param.balance
            }
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
    , balance : Uint
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
        , disabledMaxButton
        ]


disabledUserBalance :
    { param
        | token : Token
        , balance : Uint
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
            , Font.color Color.transparent300
            ]
            (text "Bal: ")
        , Truncate.disabledBalance
            { token = param.token
            , balance = param.balance
            }
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
