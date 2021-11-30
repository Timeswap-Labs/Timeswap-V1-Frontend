module Modal.MaturityList.Summary exposing (Summary, decoder)

import Data.CDP as CDP exposing (CDP)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline


type alias Summary =
    { rank : Int
    , apr : Float
    , cdp : CDP
    }


decoder : Decoder Summary
decoder =
    Decode.succeed Summary
        |> Pipeline.required "rank" Decode.int
        |> Pipeline.required "apr" Decode.float
        |> Pipeline.required "cdp" CDP.decoder
