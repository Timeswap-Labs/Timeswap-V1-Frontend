module Pages.PairMarket.Tooltip exposing
    ( Tooltip(..)
    , amount
    )

import Data.Maturity exposing (Maturity)
import Data.Pair exposing (Pair)
import Element
    exposing
        ( Element
        , alignTop
        , centerY
        , el
        , height
        , moveUp
        , padding
        , paddingEach
        , px
        , row
        , shrink
        , text
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Utility.Color as Color
import Utility.Tooltip as Tooltip


type Tooltip
    = AssetLiquidity Pair Maturity
    | CollateralLiquidity Pair Maturity
    | CollateralFactor Pair Maturity


amount : String -> Element msg
amount string =
    row
        [ width shrink
        , height shrink
        , paddingEach
            { top = 0
            , right = 0
            , bottom = 0
            , left = 4
            }
        ]
        [ el
            [ alignTop
            , height <| px 24
            ]
            (el [ centerY ] Tooltip.triangleLeft)
        , el
            [ width shrink
            , height shrink
            , padding 12
            , alignTop
            , moveUp 6
            , Background.color Color.dark500
            , Border.rounded 4
            , Tooltip.shadow
            , Font.size 12
            , Font.color Color.transparent300
            ]
            (text string)
        ]
