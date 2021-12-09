module Modal.MaturityList.Answer exposing (Answer, decoder)

import Json.Decode exposing (Decoder)
import Modal.MaturityList.Pools as Pools exposing (Pools)


type alias Answer =
    Pools


decoder : Decoder Answer
decoder =
    Pools.decoder
