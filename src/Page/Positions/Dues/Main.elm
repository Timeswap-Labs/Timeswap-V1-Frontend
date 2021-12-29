module Page.Positions.Dues.Main exposing
    ( Msg
    , view
    )

import Blockchain.User.Main exposing (User)
import Data.Backdrop exposing (Backdrop)
import Data.Device exposing (Device(..))
import Data.Images exposing (Images)
import Data.Theme exposing (Theme)
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
        , theme : Theme
    }
    -> User
    -> Element Msg
view { device, backdrop, images, theme } user =
    el
        ([ Region.description "borrow positions"
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
            ++ Glass.background backdrop theme
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
                , Font.size 14
                , paddingXY 0 3
                , Font.color Color.transparent300
                ]
                [ text "Your Borrow positions including from Liqudity transactions will appear here..." ]
            ]
        )
