module Page.Position.Claim.Error exposing (Error(..), decoder)

import Element
    exposing
        ( Element
        , alignRight
        , alignTop
        , centerX
        , centerY
        , column
        , el
        , fill
        , height
        , inFront
        , padding
        , paddingEach
        , paddingXY
        , paragraph
        , px
        , rotate
        , row
        , shrink
        , spacing
        , text
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Keyed as Keyed
import Element.Region as Region
import Json.Decode as Decode exposing (Decoder)
import Utility.Color as Color


type Error
    = Invalid


decoder : Decoder Error
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "invalid" ->
                        Invalid |> Decode.succeed

                    _ ->
                        Decode.fail "Not an error"
            )
