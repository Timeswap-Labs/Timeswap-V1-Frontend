module Utility.ThemeColor exposing
    ( actionElemLabel
    , background
    , border
    , btnBackground
    , btnHoverBG
    , btnPressBG
    , completelyTransparent
    , modalBackground
    , modalOutside
    , negative100
    , negative200
    , negative300
    , negative400
    , negative500
    , none
    , outside
    , placeholder
    , placeholder2
    , positive100
    , positive200
    , positive300
    , positive400
    , positive500
    , primaryBtn
    , secondary100
    , secondary200
    , secondary300
    , secondary400
    , secondary500
    , sectionBackground
    , switchBG
    , tableHeaderBG
    , text
    , textDisabled
    , textError
    , textLight
    , textboxBorder
    , transparentVery
    , warning100
    , warning200
    , warning300
    , warning400
    , warning500
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


secondary100 : Color
secondary100 =
    rgba255 120 117 235 0.12


secondary200 : Color
secondary200 =
    rgba255 120 117 235 0.44


secondary300 : Color
secondary300 =
    rgba255 120 117 235 0.64


secondary400 : Color
secondary400 =
    rgba255 120 117 235 1


secondary500 : Color
secondary500 =
    rgba255 84 82 204 1


negative100 : Color
negative100 =
    rgba255 233 85 103 0.12


negative200 : Color
negative200 =
    rgba255 233 85 103 0.12


negative300 : Color
negative300 =
    rgba255 233 85 103 0.44


negative400 : Color
negative400 =
    rgba255 233 85 103 0.64


negative500 : Color
negative500 =
    rgba255 204 51 69 1


positive100 : Color
positive100 =
    rgba255 82 204 184 0.12


positive200 : Color
positive200 =
    rgba255 82 204 184 0.44


positive300 : Color
positive300 =
    rgba255 82 204 184 0.64


positive400 : Color
positive400 =
    rgba255 82 204 184 1


positive500 : Color
positive500 =
    rgba255 36 178 155 1


warning100 : Color
warning100 =
    rgba255 242 189 84 0.12


warning200 : Color
warning200 =
    rgba255 242 189 84 0.42


warning300 : Color
warning300 =
    rgba255 242 189 84 0.64


warning400 : Color
warning400 =
    rgba255 242 189 84 1


warning500 : Color
warning500 =
    rgba255 217 163 54 1


completelyTransparent : Color
completelyTransparent =
    rgba255 0 0 0 0


transparentVery : Color
transparentVery =
    rgba255 255 255 255 0.02
