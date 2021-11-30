module Modal.MaturityList.Pools exposing (Pools, decoder)

import Data.Maturity as Maturity exposing (Maturity)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Modal.MaturityList.Summary as Summary exposing (Summary)
import Sort.Dict as Dict exposing (Dict)


type alias Pools =
    Dict Maturity Summary


decoder : Decoder Pools
decoder =
    Decode.succeed Tuple.pair
        |> Pipeline.required "maturity" Maturity.decoder
        |> Pipeline.custom Summary.decoder
        |> Decode.list
        |> Decode.map (Dict.fromList Maturity.sorter)
