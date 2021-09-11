module Data.Balances exposing
    ( BalanceInfo
    , Balances
    , get
    , hasEnough
    , init
    , isEmpty
    , toList
    , update
    )

import Data.Remote exposing (Remote(..))
import Data.Token as Token exposing (Token)
import Data.Tokens as Tokens exposing (Tokens)
import Data.Uint as Uint exposing (Uint)
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Decode.Pipeline as Pipeline
import Sort.Dict as Dict exposing (Dict)


type Balances
    = Balances (Dict Token Uint)


type alias BalanceInfo =
    { token : Token
    , balance : String
    }


decoder : Tokens -> Decoder Balances
decoder tokens =
    Decode.succeed Tuple.pair
        |> Pipeline.required "token" (Tokens.decoderToken tokens)
        |> Pipeline.required "balance" Uint.decoder
        |> Decode.list
        |> Decode.map (Dict.fromList Token.sorter)
        |> Decode.map Balances


init : Tokens -> Value -> Remote () Balances
init tokens value =
    value
        |> Decode.decodeValue (decoder tokens)
        |> (\result ->
                case result of
                    Ok balances ->
                        Success balances

                    _ ->
                        Loading
           )


update : Tokens -> Value -> Balances -> Balances
update tokens value (Balances balances) =
    value
        |> Decode.decodeValue (decoder tokens)
        |> (\result ->
                case result of
                    Ok (Balances dict) ->
                        balances
                            |> Dict.insertAll dict
                            |> Balances

                    _ ->
                        Balances balances
           )


isEmpty : Balances -> Bool
isEmpty (Balances dict) =
    dict |> Dict.isEmpty


toList : Balances -> List BalanceInfo
toList (Balances dict) =
    dict
        |> Dict.toList
        |> List.map
            (\( token, balance ) ->
                { token = token
                , balance =
                    balance
                        |> Uint.toAmount token
                }
            )


get : Token -> Balances -> String
get token (Balances dict) =
    dict
        |> Dict.get token
        |> Maybe.map (Uint.toAmount token)
        |> Maybe.withDefault "0"


hasEnough : Token -> String -> Balances -> Bool
hasEnough token string (Balances dict) =
    Maybe.map2 Uint.compare
        (dict |> Dict.get token)
        (string |> Uint.fromAmount token)
        |> Maybe.map
            (\order ->
                case order of
                    LT ->
                        False

                    _ ->
                        True
            )
        |> Maybe.withDefault False
