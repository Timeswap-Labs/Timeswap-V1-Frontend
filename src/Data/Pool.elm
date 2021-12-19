module Data.Pool exposing
    ( Pool
    , decoder
    , encode
    , sorter
    , toQueryParameters
    , toString
    )

import Data.Maturity as Maturity exposing (Maturity)
import Data.Pair as Pair exposing (Pair)
import Data.Token as Token
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode exposing (Value)
import Sort exposing (Sorter)
import Url.Builder exposing (QueryParameter)


type alias Pool =
    { pair : Pair
    , maturity : Maturity
    }


decoder : Decoder Pool
decoder =
    Decode.succeed Pool
        |> Pipeline.custom Pair.decoder
        |> Pipeline.required "maturity" Maturity.decoder


encode : Pool -> Value
encode { pair, maturity } =
    [ ( "asset", pair |> Pair.toAsset |> Token.encode )
    , ( "collateral", pair |> Pair.toCollateral |> Token.encode )
    , ( "maturity", maturity |> Maturity.encode )
    ]
        |> Encode.object


toQueryParameters : Pool -> List QueryParameter
toQueryParameters { pair, maturity } =
    [ pair |> Pair.toQueryParameters
    , maturity
        |> Maturity.toQueryParameter
        |> List.singleton
    ]
        |> List.concat


sorter : Sorter Pool
sorter =
    Pair.sorter
        |> Sort.by .pair
        |> Sort.tiebreaker
            (Maturity.sorter |> Sort.by .maturity)


toString : Pool -> String
toString pool =
    [ pool.pair
        |> Pair.toAsset
        |> Token.toSymbol
        |> String.left 5
    , pool.pair
        |> Pair.toCollateral
        |> Token.toSymbol
        |> String.left 5
    , pool.maturity
        |> Maturity.toUnix
        |> String.fromInt
    ]
        |> String.join "-"
