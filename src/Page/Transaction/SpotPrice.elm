module Page.Transaction.SpotPrice exposing (SpotPrice, decoder, encode)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode exposing (Value)


type alias SpotPrice =
    { assetSpot : Maybe Float
    , collateralSpot : Maybe Float
    }


decoder : Decoder SpotPrice
decoder =
    Decode.succeed SpotPrice
        |> Pipeline.required "assetSpot" (Decode.float |> Decode.nullable)
        |> Pipeline.required "collateralSpot" (Decode.float |> Decode.nullable)


encode : SpotPrice -> Value
encode { assetSpot, collateralSpot } =
    [ ( "assetSpot"
      , assetSpot
            |> Maybe.map Encode.float
            |> Maybe.withDefault Encode.null
      )
    , ( "collateralSpot"
      , collateralSpot
            |> Maybe.map Encode.float
            |> Maybe.withDefault Encode.null
      )
    ]
        |> Encode.object
