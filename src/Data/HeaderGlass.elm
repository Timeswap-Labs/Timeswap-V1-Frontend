module Data.HeaderGlass exposing (HeaderGlass(..), decoder)

import Json.Decode as Decode exposing (Decoder)


type HeaderGlass
    = Show
    | Hidden


decoder : Decoder HeaderGlass
decoder =
    Decode.bool
        |> Decode.map
            (\bool ->
                if bool then
                    Show

                else
                    Hidden
            )
