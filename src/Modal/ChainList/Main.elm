port module Modal.ChainList.Main exposing (Msg, update, view)

import Data.Backdrop exposing (Backdrop)
import Data.Chain as Chain exposing (Chain)
import Element
    exposing
        ( Element
        , alpha
        , behindContent
        , centerX
        , centerY
        , column
        , el
        , fill
        , height
        , none
        , px
        , width
        )
import Element.Background as Background
import Element.Events as Events
import Element.Input as Input
import Json.Encode exposing (Value)
import Utility.Color as Color
import Utility.Glass as Glass


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


view : { model | backdrop : Backdrop } -> Element Msg
view { backdrop } =
    Glass.outsideModal backdrop
        Exit
        (column
            [ width <| px 335
            , height <| px 300
            , centerX
            , centerY
            , Background.color Color.light100
            ]
            []
        )
