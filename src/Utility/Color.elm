module Utility.Color exposing
    ( dark400
    , dark500
    , light100
    , primary100
    , primary300
    , primary400
    , primary500
    , transparent100
    , transparent200
    , transparent300
    , transparent500
    , warning400
    )

import Element exposing (Color, rgba255)


dark400 : Color
dark400 =
    rgba255 26 31 51 1


dark500 : Color
dark500 =
    rgba255 15 20 38 1


light100 : Color
light100 =
    rgba255 255 255 255 1


transparent100 : Color
transparent100 =
    rgba255 255 255 255 0.12


transparent200 : Color
transparent200 =
    rgba255 255 255 255 0.32


transparent300 : Color
transparent300 =
    rgba255 255 255 255 0.64


transparent500 : Color
transparent500 =
    rgba255 255 255 255 1


primary100 : Color
primary100 =
    rgba255 160 210 235 0.12


primary300 : Color
primary300 =
    rgba255 160 210 235 0.64


primary400 : Color
primary400 =
    rgba255 160 210 235 1


primary500 : Color
primary500 =
    rgba255 118 175 204 1


warning400 : Color
warning400 =
    rgba255 242 189 84 1
