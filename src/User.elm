module User exposing (User, decoder)

import Data.Address as Address exposing (Address)
import Data.Chain as Chain exposing (Chain)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline


type alias User =
    { chain : Chain
    , address : Address
    }


decoder : Decoder User
decoder =
    Decode.succeed User
        |> Pipeline.required "chainId" Chain.decoder
        |> Pipeline.required "user" Address.decoder
