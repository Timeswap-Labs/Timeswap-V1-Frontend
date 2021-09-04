module Data.Whitelist exposing
    ( Whitelist
    , init
    )

import Data.Pools as Pools exposing (Pools)
import Data.Token as Token
import Data.Tokens as Tokens exposing (Tokens)
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Decode.Pipeline as Pipeline
import Sort.Set as Set


type alias Whitelist =
    { tokens : Tokens
    , pools : Pools
    }


init : Value -> Whitelist
init value =
    value
        |> Decode.decodeValue decoder
        |> (\result ->
                case result of
                    Ok whitelist ->
                        whitelist

                    Err _ ->
                        { tokens = Set.empty Token.sorter
                        , pools = Pools.empty
                        }
           )


decoder : Decoder Whitelist
decoder =
    Tokens.decoder
        |> Decode.field "erc20s"
        |> Decode.andThen
            (\tokens ->
                Decode.succeed Whitelist
                    |> Pipeline.hardcoded tokens
                    |> Pipeline.required "pairs" (Pools.decoder tokens)
            )
