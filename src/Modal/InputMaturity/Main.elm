module Modal.InputMaturity.Main exposing
    ( Modal
    , Msg
    , init
    , update
    , view
    )

import Data.Backdrop exposing (Backdrop)
import Data.Pair exposing (Pair)
import Element
    exposing
        ( Element
        , centerX
        , centerY
        , el
        , height
        , none
        , padding
        , px
        , width
        )
import Element.Border as Border
import Utility.Color as Color
import Utility.Glass as Glass


type Modal
    = Modal { pair : Pair }


init :
    Pair
    -> Modal
init pair =
    { pair = pair }
        |> Modal


type Msg
    = Exit


update : Msg -> Modal -> Maybe Modal
update msg modal =
    case ( msg, modal ) of
        ( Exit, _ ) ->
            Nothing


view : { model | backdrop : Backdrop } -> Modal -> Element Msg
view { backdrop } (Modal modal) =
    Glass.outsideModal backdrop
        Exit
        (el
            [ width <| px 375
            , height <| px 200
            , padding 24
            , centerX
            , centerY
            , Glass.background backdrop
            , Border.rounded 8
            , Border.color Color.transparent100
            , Border.width 1
            ]
            none
        )
