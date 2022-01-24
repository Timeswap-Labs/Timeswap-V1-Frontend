module Blockchain.User.Balances exposing
    ( Balances
    , decoder
    , encode
    , encodeSingle
    , hasEnough
    , init
    , subscriptions
    , update
    )

import Data.Address as Address exposing (Address)
import Data.Chain as Chain exposing (Chain)
import Data.Chains as Chains exposing (Chains)
import Data.Remote as Remote exposing (Remote(..))
import Data.Token as Token exposing (Token)
import Data.Uint as Uint exposing (Uint)
import Data.Web exposing (Web)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode exposing (Value)
import Sort.Dict as Dict exposing (Dict)
import Time exposing (Posix)


type alias Balances =
    Dict Token (Web Uint)


type alias Answer =
    { chain : Chain
    , address : Address
    , balances : Balances
    }


init :
    Chains
    -> Chain
    -> Balances
init chains chain =
    chains
        |> Chains.toTokenList chain
        |> List.map (\token -> ( token, Remote.loading ))
        |> Dict.fromList Token.sorter


encode : Chains -> Chain -> Address -> Value
encode chains chain address =
    [ ( "chain", chain |> Chain.encode )
    , ( "address", address |> Address.encode )
    , ( "tokens"
      , chains
            |> Chains.toTokenList chain
            |> Encode.list Token.encode
      )
    ]
        |> Encode.object


encodeSingle : Chain -> Address -> Token -> Value
encodeSingle chain address token =
    [ ( "chain", chain |> Chain.encode )
    , ( "address", address |> Address.encode )
    , ( "tokens", [ token ] |> Encode.list Token.encode )
    ]
        |> Encode.object


decoder : Decoder Answer
decoder =
    Decode.succeed
        (\chain address tokens balances ->
            { chain = chain
            , address = address
            , balances =
                List.map2
                    (\token balance ->
                        ( token, balance |> Success )
                    )
                    tokens
                    balances
                    |> Dict.fromList Token.sorter
            }
        )
        |> Pipeline.required "chain" Chain.decoder
        |> Pipeline.required "address" Address.decoder
        |> Pipeline.required "tokens" (Token.decoder |> Decode.list)
        |> Pipeline.required "balances" (Uint.decoder |> Decode.list)


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
