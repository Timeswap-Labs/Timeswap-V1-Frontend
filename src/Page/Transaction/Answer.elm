module Page.Transaction.Answer exposing (Answer, decoder)

import Data.Chain exposing (Chain)
import Data.Chains exposing (Chains)
import Data.Pool as Pool exposing (Pool)
import Json.Decode as Decode exposing (Decoder)
import Page.Transaction.PoolInfo as PoolInfo exposing (PoolInfo)


type alias Answer =
    Maybe PoolInfo


decoder :
    { model | chains : Chains }
    -> Chain
    -> Pool
    -> Decoder Answer
decoder { chains } chain pool =
    Pool.decoder chain chains
        |> Decode.andThen
            (\decodedPool ->
                if decodedPool == pool then
                    PoolInfo.decoder |> Decode.nullable

                else
                    Decode.fail "not the correct chain and pool"
            )
