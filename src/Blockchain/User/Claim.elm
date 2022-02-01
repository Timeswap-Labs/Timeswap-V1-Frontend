module Blockchain.User.Claim exposing
    ( Claim
    , decoder
    , encode
    )

import Data.Uint as Uint exposing (Uint)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode exposing (Value)


type alias Claim =
    { bondPrincipal : Uint
    , bondInterest : Uint
    , insurancePrincipal : Uint
    , insuranceInterest : Uint
    }


decoder : Decoder Claim
decoder =
    Decode.succeed Claim
        |> Pipeline.required "bondPrincipal" Uint.decoder
        |> Pipeline.required "bondInterest" Uint.decoder
        |> Pipeline.required "insurancePrincipal" Uint.decoder
        |> Pipeline.required "insuranceInterest" Uint.decoder


encode : Claim -> Value
encode { bondPrincipal, bondInterest, insurancePrincipal, insuranceInterest } =
    [ ( "bondPrincipal", bondPrincipal |> Uint.encode )
    , ( "bondInterest", bondInterest |> Uint.encode )
    , ( "insurancePrincipal", insurancePrincipal |> Uint.encode )
    , ( "insuranceInterest", insuranceInterest |> Uint.encode )
    ]
        |> Encode.object
