module Page.Transaction.Liquidity.New.Query exposing
    ( Answer
    , decoder
    , givenNew
    )

import Data.CDP as CDP exposing (CDP)
import Data.Chain as Chain exposing (Chain)
import Data.Pool as Pool exposing (Pool)
import Data.Remote exposing (Remote(..))
import Data.Uint as Uint exposing (Uint)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode exposing (Value)
import Page.Transaction.Liquidity.New.Error as Error exposing (Error)
import Page.Transaction.Price as Price exposing (Price)


type alias QueryNew =
    { chainId : Chain
    , pool : Pool
    , price : Price
    , assetIn : Uint
    , debtIn : Uint
    , collateralIn : Uint
    }


type alias Answer =
    { chainId : Chain
    , pool : Pool
    , price : Maybe Uint
    , assetIn : Uint
    , debtIn : Uint
    , collateralIn : Uint
    , result : Result Error ResultNew
    }


type alias ResultNew =
    { liquidityOut : Uint
    , apr : Float
    , cdp : CDP
    }


givenNew : QueryNew -> Value
givenNew { chainId, pool, price, assetIn, debtIn, collateralIn } =
    [ ( "chainId", chainId |> Chain.encode )
    , ( "pool", pool |> Pool.encode )
    , ( "price", price |> Price.encode )
    , ( "assetIn", assetIn |> Uint.encode )
    , ( "debtIn", debtIn |> Uint.encode )
    , ( "collateralIn", collateralIn |> Uint.encode )
    ]
        |> Encode.object


decoder : Decoder Answer
decoder =
    Decode.succeed Answer
        |> Pipeline.required "chainId" Chain.decoder
        |> Pipeline.required "pool" Pool.decoder
        |> Pipeline.required "price" (Uint.decoder |> Decode.nullable)
        |> Pipeline.required "assetIn" Uint.decoder
        |> Pipeline.required "debtIn" Uint.decoder
        |> Pipeline.required "collateralIn" Uint.decoder
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
