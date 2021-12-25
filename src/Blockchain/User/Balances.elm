module Blockchain.User.Balances exposing
    ( Balances
    , decoder
    , hasEnough
    , init
    , subscriptions
    , update
    )

import Data.Chain exposing (Chain)
import Data.Chains as Chains exposing (Chains)
import Data.Remote as Remote exposing (Remote(..))
import Data.Token as Token exposing (Token)
import Data.Uint as Uint exposing (Uint)
import Data.Web exposing (Web)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Sort.Dict as Dict exposing (Dict)
import Time exposing (Posix)


type alias Balances =
    Dict Token (Web Uint)


init :
    Chains
    -> Chain
    -> Balances
init chains chain =
    chains
        |> Chains.toTokenList chain
        |> List.map (\token -> ( token, Remote.loading ))
        |> Dict.fromList Token.sorter


decoder : Decoder Balances
decoder =
    Decode.succeed Tuple.pair
        |> Pipeline.required "token" Token.decoder
        |> Pipeline.required "balance"
            (Uint.decoder |> Decode.map Success)
        |> Decode.list
        |> Decode.map (Dict.fromList Token.sorter)


hasEnough : Token -> Uint -> Balances -> Bool
hasEnough token amount balances =
    balances
        |> Dict.get token
        |> (Maybe.map << Remote.map) (Uint.hasEnough amount)
        |> (Maybe.map << Remote.withDefault) False
        |> Maybe.withDefault False


update : Posix -> Balances -> Balances
update posix balances =
    balances
        |> Dict.toList
        |> (List.map << Tuple.mapSecond) (Remote.update posix)
        |> Dict.fromList Token.sorter


subscriptions : (Posix -> msg) -> Balances -> Sub msg
subscriptions tick balances =
    balances
        |> Dict.values
        |> List.map (Remote.subscriptions tick)
        |> Sub.batch
