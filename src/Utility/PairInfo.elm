module Utility.PairInfo exposing (icons, iconsAside, symbols, symbolsAside)

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


icons : Pair -> Element msg
icons pair =
    row
        [ width shrink
        , height shrink
        , spacing 4
        , alignLeft
        , centerY
        ]
        [ pair |> Pair.toAsset |> TokenImage.getIcon [ height <| px 32 ]
        , pair |> Pair.toCollateral |> TokenImage.getIcon [ height <| px 32 ]
        ]


iconsAside : Pair -> Element msg
iconsAside pair =
    row
        [ width shrink
        , height shrink
        , spacing 4
        , alignLeft
        , centerY
        ]
        [ pair |> Pair.toAsset |> TokenImage.getIcon [ height <| px 24 ]
        , pair |> Pair.toCollateral |> TokenImage.getIcon [ height <| px 24 ]
        ]


symbols : Pair -> Element msg
symbols pair =
    el
        [ width shrink
        , height shrink
        , alignLeft
        , centerY
        , Font.bold
        , Font.size 18
        , Font.color Color.transparent500
        ]
        ([ pair |> Pair.toAsset |> Token.toSymbol -- short symbol
         , pair |> Pair.toCollateral |> Token.toSymbol -- short symbol
         ]
            |> String.join " - "
            |> text
        )


symbolsAside : Pair -> Element msg
symbolsAside pair =
    el
        [ width shrink
        , height shrink
        , alignLeft
        , centerY
        , Font.regular
        , Font.size 14
        , Font.color Color.transparent500
        ]
        ([ pair |> Pair.toAsset |> Token.toSymbol -- short symbol
         , pair |> Pair.toCollateral |> Token.toSymbol -- short symbol
         ]
            |> String.join " - "
            |> text
        )
