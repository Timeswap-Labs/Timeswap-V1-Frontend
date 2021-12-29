module Utility.ThemeColor exposing
    ( background
    , completelyTransparent
    , dark100
    , dark200
    , dark300
    , dark400
    , dark500
    , darkModal
    , light100
    , light200
    , light300
    , light400
    , light500
    , lightModal
    , list
    , modal
    , negative100
    , negative200
    , negative300
    , negative400
    , negative500
    , none
    , outside
    , positive100
    , positive200
    , positive300
    , positive400
    , positive500
    , primary100
    , primary200
    , primary300
    , primary400
    , primary500
    , secondary100
    , secondary200
    , secondary300
    , secondary400
    , secondary500
    , solid
    , transparent100
    , transparent200
    , transparent300
    , transparent400
    , transparent500
    , transparentVery
    , warning100
    , warning200
    , warning300
    , warning400
    , warning500
    )

import Data.Theme as Theme exposing (Theme)
import Element exposing (Color, rgba255)


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
            rgba255 28 45 59 0.3

        Theme.Light ->
            rgba255 118 175 204 0.12


solid : Color
solid =
    rgba255 28 45 59 1


list : Color
list =
    rgba255 0 0 0 0.21


modal : Color
modal =
    rgba255 0 0 0 0.4


dark100 : Color
dark100 =
    rgba255 67 72 89 1


dark200 : Color
dark200 =
    rgba255 54 59 77 1


dark300 : Color
dark300 =
    rgba255 38 44 64 1


dark400 : Color
dark400 =
    rgba255 26 31 51 1


dark500 : Color
dark500 =
    rgba255 15 20 38 1


darkModal : Color
darkModal =
    rgba255 26 31 51 1


light100 : Theme -> Color
light100 theme =
    case theme of
        Theme.Dark ->
            rgba255 255 255 255 1

        Theme.Light ->
            rgba255 15 20 38 1


light200 : Color
light200 =
    rgba255 242 243 247 1


light300 : Color
light300 =
    rgba255 230 232 239 1


light400 : Color
light400 =
    rgba255 219 221 230 1


light500 : Color
light500 =
    rgba255 208 211 221 1


lightModal : Color
lightModal =
    rgba255 219 221 230 1


transparent100 : Color
transparent100 =
    rgba255 255 255 255 0.12


transparent200 : Color
transparent200 =
    rgba255 255 255 255 0.32


transparent300 : Color
transparent300 =
    rgba255 255 255 255 0.64


transparent400 : Color
transparent400 =
    rgba255 255 255 255 0.88


transparent500 : Color
transparent500 =
    rgba255 255 255 255 1


primary100 : Theme -> Color
primary100 theme =
    case theme of
        Theme.Dark ->
            rgba255 160 210 235 0.12

        Theme.Light ->
            rgba255 255 255 255 0.32


primary200 : Color
primary200 =
    rgba255 160 210 235 0.44


primary300 : Color
primary300 =
    rgba255 160 210 235 0.64


primary400 : Theme -> Color
primary400 theme =
    case theme of
        Theme.Dark ->
            rgba255 160 210 235 1

        Theme.Light ->
            rgba255 84 82 204 1


primary500 : Theme -> Color
primary500 theme =
    case theme of
        Theme.Dark ->
            rgba255 118 175 204 1

        Theme.Light ->
            rgba255 84 82 204 1


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
