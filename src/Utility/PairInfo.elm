module Utility.PairInfo exposing (icons, symbols)

import Data.Pair as Pair exposing (Pair)
import Data.Token as Token
import Element
    exposing
        ( Element
        , alignLeft
        , centerY
        , el
        , height
        , px
        , row
        , shrink
        , spacing
        , text
        , width
        )
import Element.Font as Font
import Utility.Color as Color
import Utility.TokenImage as TokenImage


icons : { iconSize : Int } -> Pair -> Element msg
icons { iconSize } pair =
    row
        [ width shrink
        , height shrink
        , spacing 4
        , alignLeft
        , centerY
        ]
        [ pair |> Pair.toAsset |> TokenImage.getIcon [ height <| px iconSize ]
        , pair |> Pair.toCollateral |> TokenImage.getIcon [ height <| px iconSize ]
        ]


symbols : { fontSize : Int, isBold : Bool } -> Pair -> Element msg
symbols { fontSize, isBold } pair =
    el
        [ width shrink
        , height shrink
        , alignLeft
        , centerY
        , if isBold then
            Font.bold

          else
            Font.regular
        , Font.size fontSize
        , Font.color Color.transparent500
        ]
        ([ pair |> Pair.toAsset |> Token.toSymbol -- short symbol
         , pair |> Pair.toCollateral |> Token.toSymbol -- short symbol
         ]
            |> String.join " - "
            |> text
        )
