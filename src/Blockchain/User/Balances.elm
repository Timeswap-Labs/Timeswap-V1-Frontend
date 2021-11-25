module Blockchain.User.Balances exposing (Balances, hasEnough)

import Data.Chain exposing (Chain)
import Data.Chains as Chains exposing (Chains)
import Data.Token as Token exposing (Token)
import Data.Uint as Uint exposing (Uint)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Sort.Dict as Dict exposing (Dict)


type alias Balances =
    Dict Token Uint


decoder : Chain -> Chains -> Decoder Balances
decoder chain chains =
    Decode.succeed Tuple.pair
        |> Pipeline.required "token" (Chains.decoderToken chain chains)
        |> Pipeline.required "balance" Uint.decoder
        |> Decode.list
        |> Decode.map (Dict.fromList Token.sorter)


hasEnough : Token -> Uint -> Balances -> Bool
hasEnough token amount balances =
    balances
        |> Dict.get token
        |> Maybe.map (Uint.compare amount)
        |> Maybe.map
            (\order ->
                case order of
                    LT ->
                        True

                    _ ->
                        False
            )
        |> Maybe.withDefault False
