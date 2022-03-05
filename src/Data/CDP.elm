module Data.CDP exposing (CDP, decoder, encode, init)

import Data.Uint as Uint exposing (Uint)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode exposing (Value)


type alias CDP =
    { percent : Maybe Float
    , ratio : Uint
    }


init : CDP
init =
    { percent = Just 0
    , ratio = Uint.zero
    }


decoder : Decoder CDP
decoder =
    Decode.succeed CDP
        |> Pipeline.required "percent"
            (Decode.float |> Decode.nullable)
        |> Pipeline.required "ratio" Uint.decoder


encode : CDP -> Value
encode cdp =
    [ ( "percent"
      , cdp.percent
            |> Maybe.map Encode.float
            |> Maybe.withDefault Encode.null
      )
    , ( "ratio", cdp.ratio |> Uint.encode )
    ]
        |> Encode.object
