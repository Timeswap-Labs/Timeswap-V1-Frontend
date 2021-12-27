module Page.Positions.Liquidities.Main exposing
    ( Msg
    , view
    )

import Blockchain.User.Main exposing (User)
import Data.Backdrop exposing (Backdrop)
import Data.Device exposing (Device(..))
import Data.Images exposing (Images)
import Element
    exposing
        ( Element
        , alignTop
        , centerX
        , centerY
        , el
        , fill
        , height
        , padding
        , paddingXY
        , paragraph
        , px
        , row
        , shrink
        , spacing
        , text
        , width
        )
import Element.Border as Border
import Element.Font as Font
import Element.Region as Region
import Utility.Color as Color
import Utility.Glass as Glass
import Utility.Id as Id
import Utility.Image as Image


type Msg
    = Msg


view :
    { model
        | device : Device
        , backdrop : Backdrop
        , images : Images
    }
    -> User
    -> Element Msg
view { device, backdrop, images } user =
    el
        ([ Region.description "liquidity positions"
         , (case device of
                Desktop ->
                    758

                _ ->
                    375
           )
            |> px
            |> width
         , height shrink
         , (case device of
                Desktop ->
                    24

                _ ->
                    16
           )
            |> padding
         , Border.rounded 8
         , Border.width 1
         , Border.color Color.transparent100
         , Id.is "positions"
         ]
            ++ Glass.background backdrop
        )
        (row
            [ (case device of
                Desktop ->
                    shrink

                _ ->
                    fill
              )
                |> width
            , height shrink
            , centerX
            , centerY
            , spacing 12
            ]
            [ images
                |> Image.info
                    [ width <| px 20
                    , height <| px 20
                    , centerX
                    , alignTop
                    ]
            , paragraph
                [ width fill
                , height shrink
                , centerX
                , centerY
                , Font.size 14
                , paddingXY 0 3
                , Font.color Color.transparent300
                ]
                [ text "Your Borrow positions from your Liquidity transactions will appear in Borrow section. Your Liquidity positions will appear here..." ]
            ]
        )
