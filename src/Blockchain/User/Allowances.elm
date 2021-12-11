module Blockchain.User.Allowances exposing
    ( Allowances
    , decoder
    , hasEnough
    , init
    )

import Data.Chain exposing (Chain)
import Data.Chains as Chains exposing (Chains)
import Data.ERC20 as ERC20 exposing (ERC20)
import Data.Remote as Remote exposing (Remote(..))
import Data.Uint as Uint exposing (Uint)
import Data.Web exposing (Web)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Sort.Dict as Dict exposing (Dict)


type alias Allowances =
    Dict ERC20 (Web Uint)


init :
    Chains
    -> Chain
    -> Allowances
init chains chain =
    chains
        |> Chains.toListERC20 chain
        |> List.map (\erc20 -> ( erc20, Loading ))
        |> Dict.fromList ERC20.sorter


decoder : Decoder Allowances
decoder =
    Decode.succeed Tuple.pair
        |> Pipeline.required "erc20" ERC20.decoder
        |> Pipeline.required "allowance"
            (Uint.decoder |> Decode.map Success)
        |> Decode.list
        |> Decode.map (Dict.fromList ERC20.sorter)


hasEnough : ERC20 -> Uint -> Allowances -> Bool
hasEnough erc20 amount allowances =
    allowances
        |> Dict.get erc20
        |> (Maybe.map << Remote.map) (Uint.compare amount)
        |> (Maybe.map << Remote.map)
            (\order ->
                case order of
                    LT ->
                        True

                    _ ->
                        False
            )
        |> (Maybe.map << Remote.withDefault) False
        |> Maybe.withDefault False
