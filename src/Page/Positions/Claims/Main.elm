module Page.Positions.Claims.Main exposing
    ( Msg
    , view
    )

import Blockchain.User.Main as User exposing (User)
import Data.Backdrop exposing (Backdrop)
import Data.Device exposing (Device(..))
import Data.Images exposing (Images)
import Data.Remote as Remote exposing (Remote(..))
import Element
    exposing
        ( Element
        , alignTop
        , centerX
        , centerY
        , el
        , height
        , none
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
import Sort.Dict as Dict
import Utility.Color as Color
import Utility.Glass as Glass
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
view ({ device, backdrop } as model) user =
    el
        ([ Region.description "lend positions"
         , (case device of
                Desktop ->
                    760

                _ ->
                    377
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
         ]
            ++ Glass.background backdrop
        )
        (case user |> User.getClaims of
            Loading timeline ->
                none

            Failure error ->
                none

            Success claims ->
                if claims |> Dict.isEmpty then
                    noClaims model

                else
                    none
        )


noClaims : { model | images : Images } -> Element msg
noClaims { images } =
    row
        [ width shrink
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
            [ width shrink
            , height shrink
            , centerX
            , centerY
            , Font.size 14
            , paddingXY 0 3
            , Font.color Color.transparent300
            ]
            [ text "Your Lend positions will appear here..." ]
        ]
