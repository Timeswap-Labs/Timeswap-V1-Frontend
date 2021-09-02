module Utility.PairInfo exposing (icons, iconsAside, symbols, symbolsAside)

import Data.Pair as Pair exposing (Pair)
import Data.Token as Token
import Data.TokenImages exposing (TokenImages)
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


icons : TokenImages -> Pair -> Element msg
icons tokenImages pair =
    row
        [ width shrink
        , height shrink
        , spacing 4
        , alignLeft
        , centerY
        ]
        [ pair
            |> Pair.toAsset
            |> TokenImage.icon tokenImages [ height <| px 32 ]
        , pair
            |> Pair.toCollateral
            |> TokenImage.icon tokenImages [ height <| px 32 ]
        ]


iconsAside : TokenImages -> Pair -> Element msg
iconsAside tokenImages pair =
    row
        [ width shrink
        , height shrink
        , spacing 4
        , alignLeft
        , centerY
        ]
        [ pair
            |> Pair.toAsset
            |> TokenImage.icon tokenImages [ height <| px 24 ]
        , pair
            |> Pair.toCollateral
            |> TokenImage.icon tokenImages [ height <| px 24 ]
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
