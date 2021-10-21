module Data.Connectivity exposing (Connectivity(..), decoder)

import Json.Decode as Decode exposing (Decoder)


type Connectivity
    = Connected
    | NoConnection


decoder : Decoder Connectivity
decoder =
    Decode.bool
        |> Decode.map
            (\bool ->
                if bool then
                    Connected

                else
                    NoConnection
            )
