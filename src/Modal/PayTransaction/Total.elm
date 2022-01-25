module Modal.PayTransaction.Total exposing (Total, decoder, encode)

import Data.Uint as Uint exposing (Uint)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode exposing (Value)


type alias Total =
    { assetIn : Uint
    , collateralOut : Uint
    }


encode : Total -> Value
encode { assetIn, collateralOut } =
    [ ( "assetIn", assetIn |> Uint.encode )
    , ( "collateralOut", collateralOut |> Uint.encode )
    ]
        |> Encode.object


decoder : Decoder Total
decoder =
    Decode.succeed Total
        |> Pipeline.required "assetIn" Uint.decoder
        |> Pipeline.required "collateralOut" Uint.decoder
