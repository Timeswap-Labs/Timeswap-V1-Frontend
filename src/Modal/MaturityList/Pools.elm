module Modal.MaturityList.Pools exposing (Pools, compareMaturity, compareRank, decoder)

import Data.CDP exposing (CDP)
import Data.Maturity as Maturity exposing (Maturity)
import Data.Uint as Uint
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


compareRank : ( Maturity, Summary ) -> ( Maturity, Summary ) -> Order
compareRank ( maturity1, summary1 ) ( maturity2, summary2 ) =
    if summary1.rank > summary2.rank then
        GT

    else
        LT


compareMaturity : ( Maturity, Summary ) -> ( Maturity, Summary ) -> Order
compareMaturity ( maturity1, summary1 ) ( maturity2, summary2 ) =
    if (maturity1 |> Maturity.toUnix) > (maturity2 |> Maturity.toUnix) then
        GT

    else
        LT
