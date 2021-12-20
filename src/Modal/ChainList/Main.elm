port module Modal.ChainList.Main exposing
    ( Msg
    , update
    , view
    )

import Blockchain.Main exposing (Blockchain)
import Blockchain.User.Main as User exposing (User)
import Data.Backdrop exposing (Backdrop)
import Data.Chain as Chain exposing (Chain)
import Data.Chains as Chains exposing (Chains)
import Data.Images exposing (Images)
import Data.Support exposing (Support)
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
import Json.Encode exposing (Value)
import Modal.Outside as Outside
import Utility.Color as Color
import Utility.Glass as Glass
import Utility.IconButton as IconButton
import Utility.Image as Image


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
        , chains : Chains
    }
    -> Element Msg
view model =
    Outside.view model
        { onClick = Exit
        , modal =
            column
                [ width <| px 335
                , height shrink
                , padding 24
                , centerX
                , centerY
                , spacing 16
                , Background.color Color.background
                , Border.rounded 8
                , Border.color Color.transparent100
                , Border.width 1
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
                , column [ width fill ]
                    (model.chains
                        |> Chains.toList
                        |> List.map (\chain -> chain |> chainRow model)
                    )
                ]
        }


chainRow :
    { model
        | backdrop : Backdrop
        , images : Images
        , chains : Chains
    }
    -> Chain
    -> Element Msg
chainRow model chain =
    row
        [ width fill
        , height <| px 54
        , paddingXY 18 0
        , spacing 8
        , Background.color Color.primary100
        , Border.rounded 8
        ]
        [ model.images
            |> Image.viewChain
                [ width <| px 24
                , height <| px 24
                , centerY
                ]
                chain
        , el
            [ width shrink
            , height shrink
            , centerY
            , Font.size 16
            , paddingXY 0 4
            , Font.color Color.light100
            ]
            (chain
                |> Chain.toString
                |> text
            )
        ]
