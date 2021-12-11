module Page.Transaction.Liquidity.New.Answer exposing
    ( Answer
    , decoder
    )

import Data.CDP as CDP exposing (CDP)
import Data.Chain as Chain exposing (Chain)
import Data.Pool as Pool exposing (Pool)
import Data.Remote exposing (Remote(..))
import Data.Uint as Uint exposing (Uint)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Page.Transaction.Liquidity.New.Error as Error exposing (Error)


type alias Answer =
    { chainId : Chain
    , pool : Pool
    , spot : Maybe Uint
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


decoder : Decoder Answer
decoder =
    Decode.succeed Answer
        |> Pipeline.required "chainId" Chain.decoder
        |> Pipeline.required "pool" Pool.decoder
        |> Pipeline.required "spot" (Uint.decoder |> Decode.nullable)
        |> Pipeline.required "assetIn" Uint.decoder
        |> Pipeline.required "debtOut" Uint.decoder
        |> Pipeline.required "collateralOut" Uint.decoder
        |> Pipeline.required "result"
            ([ decoderResultNew |> Decode.map Ok
             , Error.decoder |> Decode.map Err
             ]
                |> Decode.oneOf
            )


decoderResultNew : Decoder ResultNew
decoderResultNew =
    Decode.succeed ResultNew
        |> Pipeline.required "liquidityOut" Uint.decoder
        |> Pipeline.required "apr" Decode.float
        |> Pipeline.required "cdp" CDP.decoder
