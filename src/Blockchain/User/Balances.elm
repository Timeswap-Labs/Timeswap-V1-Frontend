module Blockchain.User.Balances exposing (Balances, decoder, hasEnough)

import Data.Token as Token exposing (Token)
import Data.Uint as Uint exposing (Uint)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Sort.Dict as Dict exposing (Dict)


type alias Balances =
    Dict Token Uint


decoder : Decoder Balances
decoder =
    Decode.succeed Tuple.pair
        |> Pipeline.required "token" Token.decoder
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
