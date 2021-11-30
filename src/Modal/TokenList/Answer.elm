module Modal.TokenList.Answer exposing (Answer, decoder)

import Data.Address as Address exposing (Address)
import Data.Chain exposing (Chain)
import Data.Chains as Chains exposing (Chains)
import Data.ERC20 exposing (ERC20)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline


type alias Answer =
    { chainId : Chain
    , address : Address
    , result : Maybe ERC20
    }


decoder : { model | chains : Chains } -> Decoder Answer
decoder { chains } =
    Chains.decoderChain chains
        |> Decode.field "chainId"
        |> Decode.andThen
            (\chain ->
                Decode.succeed (Answer chain)
                    |> Pipeline.required "address" Address.decoder
                    |> Pipeline.required "result"
                        (Chains.decoderERC20 chain chains
                            |> Decode.nullable
                        )
            )
