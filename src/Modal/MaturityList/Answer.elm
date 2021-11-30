module Modal.MaturityList.Answer exposing (Answer, decoder)

import Data.Chain exposing (Chain)
import Data.Chains as Chains exposing (Chains)
import Data.Pair as Pair exposing (Pair)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Modal.MaturityList.Pools as Pools exposing (Pools)


type alias Answer =
    { chainId : Chain
    , pair : Pair
    , result : Pools
    }


decoder : { model | chains : Chains } -> Decoder Answer
decoder { chains } =
    Chains.decoderChain chains
        |> Decode.field "chainId"
        |> Decode.andThen
            (\chain ->
                Decode.succeed (Answer chain)
                    |> Pipeline.required "pair" (Pair.decoder chain chains)
                    |> Pipeline.required "result" Pools.decoder
            )
