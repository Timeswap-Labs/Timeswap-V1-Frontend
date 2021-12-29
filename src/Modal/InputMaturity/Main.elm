module Modal.InputMaturity.Main exposing
    ( Modal
    , Msg
    , init
    , update
    , view
    )

import Data.Backdrop exposing (Backdrop)
import Data.Images exposing (Images)
import Data.Pair exposing (Pair)
import Data.Theme exposing (Theme)
import Element
    exposing
        ( Element
        , centerX
        , centerY
        , column
        , el
        , fill
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
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Modal.Outside as Outside
import Utility.Color as Color
import Utility.Glass as Glass
import Utility.IconButton as IconButton


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


view :
    { model
        | backdrop : Backdrop
        , images : Images
        , theme : Theme
    }
    -> Modal
    -> Element Msg
view ({ backdrop, theme } as model) (Modal modal) =
    Outside.view model
        { onClick = Exit
        , modal =
            el
                ([ width <| px 375
                 , height <| px 200
                 , padding 24
                 , centerX
                 , centerY
                 , Background.color Color.background
                 , Border.rounded 8
                 , Border.color Color.transparent100
                 , Border.width 1
                 ]
                    ++ Glass.background backdrop theme
                )
                (column
                    [ width fill
                    , height shrink
                    , spacing 16
                    ]
                    [ row
                        [ width fill
                        , height shrink
                        , spacing 16
                        ]
                        [ el
                            [ width shrink
                            , height shrink
                            , centerY
                            , Font.size 18
                            , paddingXY 0 3
                            , Font.color Color.light100
                            ]
                            (text "Input Maturity")
                        , IconButton.exit model Exit
                        ]
                    ]
                )
        }
