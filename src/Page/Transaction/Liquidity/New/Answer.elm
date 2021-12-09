module Page.Transaction.Liquidity.New.Answer exposing
    ( Answer
    , decoder
    )

import Data.CDP as CDP exposing (CDP)
import Data.Chain exposing (Chain)
import Data.Chains as Chains exposing (Chains)
import Data.Pool as Pool exposing (Pool)
import Data.Remote exposing (Remote(..))
import Data.Uint as Uint exposing (Uint)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Page.Transaction.Liquidity.New.Error as Error exposing (Error)


type alias Answer =
    { chainId : Chain
    , pool : Pool
    , assetIn : Uint
    , debtOut : Uint
    , collateralOut : Uint
    , result : Result Error ResultNew
    }


type alias ResultNew =
    { liquidityOut : Uint
    , apr : Float
    , cdp : CDP
    }


decoder : { model | chains : Chains } -> Decoder Answer
decoder { chains } =
    Chains.decoderChain chains
        |> Decode.field "chainId"
        |> Decode.andThen
            (\chain ->
                Decode.succeed (Answer chain)
                    |> Pipeline.required "pool" (Pool.decoder chain chains)
                    |> Pipeline.required "assetIn" Uint.decoder
                    |> Pipeline.required "debtOut" Uint.decoder
                    |> Pipeline.required "collateralOut" Uint.decoder
                    |> Pipeline.required "result"
                        ([ decoderResultNew |> Decode.map Ok
                         , Error.decoder |> Decode.map Err
                         ]
                            |> Decode.oneOf
                        )
            )


decoderResultNew : Decoder ResultNew
decoderResultNew =
    Decode.succeed ResultNew
        |> Pipeline.required "liquidityOut" Uint.decoder
        |> Pipeline.required "apr" Decode.float
        |> Pipeline.required "cdp" CDP.decoder
