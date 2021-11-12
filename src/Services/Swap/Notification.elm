module Services.Swap.Notification exposing (Notification(..), decoder)

import Json.Decode as Decode exposing (Decoder)


type Notification
    = Successful
    | Failed


decoder : Decoder Notification
decoder =
    Decode.bool
        |> Decode.map
            (\bool ->
                if bool then
                    Successful

                else
                    Failed
            )
