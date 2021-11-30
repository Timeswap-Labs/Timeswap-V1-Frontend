module Page.Transaction.Answer exposing (Answer, decoder)

import Data.Chain exposing (Chain)
import Data.Chains as Chains exposing (Chains)
import Data.Pool as Pool exposing (Pool)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Page.Transaction.PoolInfo as PoolInfo exposing (PoolInfo)


type alias Answer =
    { chainId : Chain
    , pool : Pool
    , result : Maybe PoolInfo
    }


decoder : { model | chains : Chains } -> Decoder Answer
decoder { chains } =
    Chains.decoderChain chains
        |> Decode.field "chainId"
        |> Decode.andThen
            (\chain ->
                Decode.succeed (Answer chain)
                    |> Pipeline.required "pool" (Pool.decoder chain chains)
                    |> Pipeline.required "result"
                        (PoolInfo.decoder |> Decode.nullable)
            )
