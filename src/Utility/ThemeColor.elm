module Utility.ThemeColor exposing
    ( actionElemLabel
    , background
    , border
    , btnBackground
    , btnHoverBG
    , btnPressBG
    , dropdownBG
    , modalBackground
    , modalOutside
    , none
    , outside
    , placeholder
    , placeholder2
    , positionBG
    , primaryBtn
    , sectionBackground
    , switchBG
    , tableHeaderBG
    , text
    , textDisabled
    , textError
    , textLight
    , textboxBorder
    , tooltipBG
    , warning
    )

import Data.Theme as Theme exposing (Theme)
import Element exposing (Color, rgba255)
import Utility.Color as Color


none : Color
none =
    rgba255 0 0 0 0


outside : Color
outside =
    rgba255 15 20 38 0.6


background : Theme -> Color
background theme =
    case theme of
        Theme.Dark ->
            Color.background

        Theme.Light ->
            Color.primaryLight


modalBackground : Theme -> Color
modalBackground theme =
    case theme of
        Theme.Dark ->
            Color.background

        Theme.Light ->
            Color.primary100


modalOutside : Theme -> Color
modalOutside theme =
    case theme of
        Theme.Dark ->
            Color.outside

        Theme.Light ->
            Color.transparent200


text : Theme -> Color
text theme =
    case theme of
        Theme.Dark ->
            Color.light100

        Theme.Light ->
            Color.dark500


textLight : Theme -> Color
textLight theme =
    case theme of
        Theme.Dark ->
            Color.transparent300

        Theme.Light ->
            Color.dark100


textDisabled : Theme -> Color
textDisabled theme =
    case theme of
        Theme.Dark ->
            Color.transparent100

        Theme.Light ->
            Color.dark100Transparent


textError : Theme -> Color
textError theme =
    case theme of
        Theme.Dark ->
            Color.transparent500

        Theme.Light ->
            Color.negative400


placeholder : Theme -> Color
placeholder theme =
    case theme of
        Theme.Dark ->
            Color.transparent200

        Theme.Light ->
            Color.dark100


placeholder2 : Theme -> Color
placeholder2 theme =
    case theme of
        Theme.Dark ->
            Color.transparent100

        Theme.Light ->
            Color.light500


sectionBackground : Theme -> Color
sectionBackground theme =
    case theme of
        Theme.Dark ->
            Color.primary100

        Theme.Light ->
            Color.transparent200


btnBackground : Theme -> Color
btnBackground theme =
    case theme of
        Theme.Dark ->
            Color.primary100

        Theme.Light ->
            Color.secondary100


btnHoverBG : Theme -> Color
btnHoverBG theme =
    case theme of
        Theme.Dark ->
            Color.primary200

        Theme.Light ->
            Color.secondary200


btnPressBG : Theme -> Color
btnPressBG theme =
    case theme of
        Theme.Dark ->
            Color.primary300

        Theme.Light ->
            Color.secondary300


border : Theme -> Color
border theme =
    case theme of
        Theme.Dark ->
            Color.transparent100

        Theme.Light ->
            Color.secondary100


textboxBorder : Theme -> Color
textboxBorder theme =
    case theme of
        Theme.Dark ->
            Color.transparent100

        Theme.Light ->
            Color.secondary200


actionElemLabel : Theme -> Color
actionElemLabel theme =
    case theme of
        Theme.Dark ->
            Color.primary400

        Theme.Light ->
            Color.secondary500


primaryBtn : Theme -> Color
primaryBtn theme =
    case theme of
        Theme.Dark ->
            Color.primary500

        Theme.Light ->
            Color.secondary500


switchBG : Theme -> Color
switchBG theme =
    case theme of
        Theme.Dark ->
            Color.transparent200

        Theme.Light ->
            Color.transparent400


tableHeaderBG : Theme -> Color
tableHeaderBG theme =
    case theme of
        Theme.Dark ->
            Color.list

        Theme.Light ->
            Color.secondary100


positionBG : Theme -> Color
positionBG theme =
    case theme of
        Theme.Dark ->
            Color.dark500

        Theme.Light ->
            Color.transparent200


dropdownBG : Theme -> Color
dropdownBG theme =
    case theme of
        Theme.Dark ->
            Color.dark300

        Theme.Light ->
            rgba255 222 224 245 1


tooltipBG : Theme -> Color
tooltipBG theme =
    case theme of
        Theme.Dark ->
            Color.dark500

        Theme.Light ->
            Color.light300


warning : Theme -> Color
warning theme =
    case theme of
        Theme.Dark ->
            Color.warning400

        Theme.Light ->
            Color.warning500
