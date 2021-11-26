module Data.Pair exposing
    ( Pair
    , decoder
    , init
    , opposite
    , sorter
    , toAsset
    , toCollateral
    , toQueryParameters
    , toString
    )

import Data.Chain exposing (Chain)
import Data.Chains as Chains exposing (Chains)
import Data.Token as Token exposing (Token)
import Data.TokenParam as TokenParam
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Sort exposing (Sorter)
import Url.Builder exposing (QueryParameter)


type Pair
    = Pair
        { asset : Token
        , collateral : Token
        }


init : Token -> Token -> Maybe Pair
init asset collateral =
    if asset == collateral then
        Nothing

    else
        { asset = asset
        , collateral = collateral
        }
            |> Pair
            |> Just


decoder : Chain -> Chains -> Decoder Pair
decoder chain chains =
    Decode.succeed
        (\asset collateral ->
            if asset == collateral then
                Decode.fail "Not a pair"

            else
                { asset = asset
                , collateral = collateral
                }
                    |> Pair
                    |> Decode.succeed
        )
        |> Pipeline.required "asset"
            (Chains.decoderToken chain chains)
        |> Pipeline.required "collateral"
            (Chains.decoderToken chain chains)
        |> Decode.andThen identity


toQueryParameters : Pair -> List QueryParameter
toQueryParameters pair =
    [ pair
        |> toAsset
        |> Token.toQueryParameter TokenParam.Asset
    , pair
        |> toCollateral
        |> Token.toQueryParameter TokenParam.Collateral
    ]


toString : Pair -> String
toString (Pair { asset, collateral }) =
    [ asset |> Token.toString
    , collateral |> Token.toString
    ]
        |> String.join " "


toAsset : Pair -> Token
toAsset (Pair { asset }) =
    asset


toCollateral : Pair -> Token
toCollateral (Pair { collateral }) =
    collateral


opposite : Pair -> Pair
opposite (Pair { asset, collateral }) =
    { asset = collateral
    , collateral = asset
    }
        |> Pair


sorter : Sorter Pair
sorter =
    Token.sorter
        |> Sort.by toAsset
        |> Sort.tiebreaker
            (Token.sorter
                |> Sort.by toCollateral
            )
