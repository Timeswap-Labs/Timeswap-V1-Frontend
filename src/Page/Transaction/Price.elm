module Page.Transaction.Price exposing (Price, decoder, encode)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode exposing (Value)


type alias Price =
    { asset : Maybe Float
    , collateral : Maybe Float
    }


decoder : Decoder Price
decoder =
    Decode.succeed Price
        |> Pipeline.required "assetSpot" (Decode.float |> Decode.nullable)
        |> Pipeline.required "collateralSpot" (Decode.float |> Decode.nullable)


encode : Price -> Value
encode { asset, collateral } =
    [ ( "assetSpot"
      , asset
            |> Maybe.map Encode.float
            |> Maybe.withDefault Encode.null
      )
    , ( "collateralSpot"
      , collateral
            |> Maybe.map Encode.float
            |> Maybe.withDefault Encode.null
      )
    ]
        |> Encode.object
