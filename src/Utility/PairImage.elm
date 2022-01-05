module Utility.PairImage exposing (view)

import Data.Images exposing (Images)
import Data.Pair as Pair exposing (Pair)
import Element
    exposing
        ( Element
        , height
        , moveLeft
        , moveRight
        , px
        , row
        , shrink
        , width
        )
import Utility.Image as Image


view : { pair : Pair, length : Int } -> Images -> Element msg
view { pair, length } images =
    row
        [ width <| px (length * 3 // 2)
        , height shrink
        ]
        [ images
            |> Image.viewToken
                [ width <| px length
                , height <| px length
                , moveRight ((length // 2) |> toFloat)
                ]
                (pair |> Pair.toCollateral)
        , images
            |> Image.viewToken
                [ width <| px length
                , height <| px length
                , moveLeft (length |> toFloat)
                ]
                (pair |> Pair.toAsset)
        ]
