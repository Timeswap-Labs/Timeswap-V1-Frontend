module Blockchain.User.Claim exposing
    ( Claim
    , decoder
    , dummy
    , encode
    )

import Data.Uint as Uint exposing (Uint)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode exposing (Value)


type alias Claim =
    { bond : Uint
    , insurance : Uint
    }


dummy : Claim
dummy =
    { bond = Uint.dummy
    , insurance = Uint.dummy
    }


decoder : Decoder Claim
decoder =
    Decode.succeed Claim
        |> Pipeline.required "bond" Uint.decoder
        |> Pipeline.required "insurance" Uint.decoder


encode : Claim -> Value
encode { bond, insurance } =
    [ ( "bond", bond |> Uint.encode )
    , ( "insurance", insurance |> Uint.encode )
    ]
        |> Encode.object
