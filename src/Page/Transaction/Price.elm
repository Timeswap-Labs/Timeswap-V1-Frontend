module Page.Transaction.Price exposing (Price, decoder, dummy, encode)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode exposing (Value)


type alias Price =
    { asset : Maybe Float
    , collateral : Maybe Float
    }


dummy : Price
dummy =
    { asset = Nothing
    , collateral = Nothing
    }


decoder : Decoder Price
decoder =
    Decode.succeed Price
        |> Pipeline.required "asset" (Decode.float |> Decode.nullable)
        |> Pipeline.required "collateral" (Decode.float |> Decode.nullable)


encode : Price -> Value
encode { asset, collateral } =
    [ ( "asset"
      , asset
            |> Maybe.map Encode.float
            |> Maybe.withDefault Encode.null
      )
    , ( "collateral"
      , collateral
            |> Maybe.map Encode.float
            |> Maybe.withDefault Encode.null
      )
    ]
        |> Encode.object
