port module Modal.ChainList.Main exposing
    ( Msg
    , update
    , view
    )

import Data.Backdrop exposing (Backdrop)
import Data.Chain as Chain exposing (Chain)
import Data.Images exposing (Images)
import Element
    exposing
        ( Element
        , centerX
        , centerY
        , column
        , el
        , fill
        , height
        , paddingXY
        , px
        , row
        , shrink
        , spacing
        , text
        , width
        )
import Element.Background as Background
import Element.Font as Font
import Json.Encode exposing (Value)
import Utility.Color as Color
import Utility.Glass as Glass
import Utility.IconButton as IconButton


type Msg
    = ClickChain Chain
    | Exit


update : Msg -> ( Maybe Never, Cmd Msg )
update msg =
    case msg of
        ClickChain chain ->
            ( Nothing
            , chain
                |> Chain.encode
                |> changeChain
            )

        Exit ->
            ( Nothing
            , Cmd.none
            )


port changeChain : Value -> Cmd msg


view :
    { model
        | backdrop : Backdrop
        , images : Images
    }
    -> Element Msg
view model =
    Glass.outsideModal model
        { onClick = Exit
        , modal =
            column
                [ width <| px 335
                , height <| px 300
                , centerX
                , centerY
                , Background.color Color.light100
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
                        (text "Change Chain")
                    , IconButton.exit model Exit
                    ]
                ]
        }
