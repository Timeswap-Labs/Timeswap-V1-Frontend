module Services.Swap.Error exposing (Error(..), decoder)

import Json.Decode as Decode exposing (Decoder)


type Error
    = NoInternet


decoder : Decoder Error
decoder =
    Decode.int
        |> Decode.andThen
            (\int ->
                if int == 0 then
                    Decode.succeed NoInternet

                else
                    Decode.fail "Wrong Message"
            )
