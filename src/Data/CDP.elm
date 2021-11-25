module Data.CDP exposing (CDP, decoder, init)

import Data.Uint as Uint exposing (Uint)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline


type alias CDP =
    { percent : Maybe Float
    , ratio : Uint
    }


init : CDP
init =
    { percent = Nothing
    , ratio = Uint.zero
    }


decoder : Decoder CDP
decoder =
    Decode.succeed CDP
        |> Pipeline.required "percent"
            (Decode.float |> Decode.nullable)
        |> Pipeline.required "ratio" Uint.decoder
