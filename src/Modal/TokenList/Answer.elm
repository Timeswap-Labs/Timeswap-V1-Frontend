module Modal.TokenList.Answer exposing (Answer, decoder)

import Data.ERC20 as ERC20 exposing (ERC20)
import Json.Decode as Decode exposing (Decoder)


type alias Answer =
    Maybe ERC20


decoder : Decoder Answer
decoder =
    ERC20.decoder
        |> Decode.nullable
