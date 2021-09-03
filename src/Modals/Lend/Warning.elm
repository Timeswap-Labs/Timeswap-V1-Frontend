module Modals.Lend.Warning exposing (view)

import Data.Images exposing (Images)
import Data.Maturity as Maturity
import Data.Pair as Pair
import Data.Pool exposing (Pool)
import Data.Token as Token
import Data.ZoneInfo exposing (ZoneInfo)
import Element
    exposing
        ( Element
        , centerX
        , centerY
        , el
        , fill
        , height
        , paddingXY
        , paragraph
        , px
        , shrink
        , spacing
        , text
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Utility.Color as Color
import Utility.Image as Image


view :
    { model | zoneInfo : Maybe ZoneInfo, images : Images }
    -> { modal | pool : Pool }
    -> Element msg
view { zoneInfo, images } { pool } =
    paragraph
        [ width fill
        , height shrink
        , paddingXY 40 8
        , Background.color Color.warning100
        , Border.rounded 4
        , Font.regular
        , Font.size 14
        , Font.color Color.warning400
        , Font.center
        , spacing 2
        ]
        [ Image.warning images
            [ width <| px 16
            , paddingXY 0 3
            , Element.alignTop
            , centerX
            ]
        , el
            [ width (fill |> Element.maximum 300)
            , centerX
            , centerY
            , paddingXY 0 3
            ]
            ([ " You will receive "
             , pool.pair |> Pair.toAsset |> Token.toSymbol
             , " + any defaulted "
             , pool.pair |> Pair.toCollateral |> Token.toSymbol
             , " after "
             , pool.maturity
                |> Maturity.toString zoneInfo
             ]
                |> String.concat
                |> text
            )
        ]
