module Page.Positions.Claims.Main exposing
    ( Msg
    , Positions
    , init
    , view
    )

import Blockchain.User.Main exposing (User)
import Data.Images exposing (Images)
import Element
    exposing
        ( Element
        , centerX
        , centerY
        , el
        , height
        , padding
        , paddingXY
        , px
        , row
        , shrink
        , spacing
        , text
        , width
        )
import Element.Font as Font
import Utility.Color as Color
import Utility.Image as Image


type Positions
    = Positions


type Msg
    = Msg


init : Positions
init =
    Positions


view : { model | images : Images } -> User -> Element Msg
view { images } user =
    el
        [ width <| px 710
        , height shrink
        , padding 24
        ]
        (el
            [ width shrink
            , height <| px 100
            , centerX
            , centerY
            ]
            (row
                [ width shrink
                , height shrink
                , centerY
                , spacing 16
                ]
                [ images
                    |> Image.info
                        [ width <| px 20
                        , height <| px 20
                        , centerY
                        ]
                , el
                    [ width shrink
                    , height shrink
                    , centerY
                    , Font.size 14
                    , paddingXY 0 3
                    , Font.color Color.transparent300
                    ]
                    (text "Your Lend positions will appear here...")
                ]
            )
        )
