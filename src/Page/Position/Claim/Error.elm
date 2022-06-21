module Page.Position.Claim.Error exposing (Error(..), decoder)

import Json.Decode as Decode exposing (Decoder)


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
