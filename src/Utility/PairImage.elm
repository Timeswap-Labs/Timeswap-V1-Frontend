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


view : Images -> Pair -> Element msg
view images pair =
    row
        [ width <| px 36
        , height shrink
        ]
        [ images
            |> Image.viewToken
                [ width <| px 24
                , height <| px 24
                , moveRight 12
                ]
                (pair |> Pair.toCollateral)
        , images
            |> Image.viewToken
                [ width <| px 24
                , height <| px 24
                , moveLeft 24
                ]
                (pair |> Pair.toAsset)
        ]
