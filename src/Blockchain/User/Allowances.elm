module Blockchain.User.Allowances exposing (Allowances, hasEnough)

import Data.Chain exposing (Chain)
import Data.Chains as Chains exposing (Chains)
import Data.ERC20 as ERC20 exposing (ERC20)
import Data.Uint as Uint exposing (Uint)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Sort.Dict as Dict exposing (Dict)


type alias Allowances =
    Dict ERC20 Uint


decoder : Chain -> Chains -> Decoder Allowances
decoder chain chains =
    Decode.succeed Tuple.pair
        |> Pipeline.required "erc20" (Chains.decoderERC20 chain chains)
        |> Pipeline.required "allowance" Uint.decoder
        |> Decode.list
        |> Decode.map (Dict.fromList ERC20.sorter)


hasEnough : ERC20 -> Uint -> Allowances -> Bool
hasEnough erc20 amount allowances =
    allowances
        |> Dict.get erc20
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
