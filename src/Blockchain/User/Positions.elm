module Blockchain.User.Positions exposing
    ( Positions
    , decoder
    , dummy
    )

import Blockchain.User.Claims as Claims exposing (Claims)
import Blockchain.User.Dues as Dues exposing (Dues)
import Blockchain.User.Liqs as Liqs exposing (Liqs)
import Data.Address as Address exposing (Address)
import Data.Chain as Chain exposing (Chain)
import Data.Pool as Pool
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Sort.Dict as Dict


type alias Positions =
    { claims : Claims
    , dues : Dues
    , liqs : Liqs
    }


type alias Answer =
    { chain : Chain
    , owner : Address
    , positions : Positions
    }


dummy : Positions
dummy =
    { claims = Dict.empty Pool.sorter
    , dues = Dict.empty Pool.sorter
    , liqs = Dict.empty Pool.sorter
    }


decoder : Decoder Answer
decoder =
    Decode.succeed Answer
        |> Pipeline.required "chain" Chain.decoder
        |> Pipeline.required "owner" Address.decoder
        |> Pipeline.required "positions" decoderPositions


decoderPositions : Decoder Positions
decoderPositions =
    Decode.succeed Positions
        |> Pipeline.required "claims" Claims.decoder
        |> Pipeline.required "dues" Dues.decoder
        |> Pipeline.required "liqs" Liqs.decoder
