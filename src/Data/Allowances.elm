module Data.Allowances exposing
    ( Allowances
    , example
    , hasEnough
    , init
    , update
    )

import Data.ERC20 as ERC20 exposing (ERC20)
import Data.Remote exposing (Remote(..))
import Data.Token as Token exposing (Token)
import Data.Tokens as Tokens exposing (Tokens)
import Data.Uint as Uint exposing (Uint)
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Decode.Pipeline as Pipeline
import Sort.Dict as Dict exposing (Dict)


type Allowances
    = Allowances (Dict ERC20 Uint)


decoder : Tokens -> Decoder Allowances
decoder tokens =
    Decode.succeed (Dict.singleton ERC20.sorter)
        |> Pipeline.required "erc20" (Tokens.decoderERC20 tokens)
        |> Pipeline.required "allowance" Uint.decoder
        |> Decode.map Allowances


init : Tokens -> Value -> Remote Allowances
init tokens value =
    value
        |> Decode.decodeValue (decoder tokens)
        |> (\result ->
                case result of
                    Ok allowances ->
                        Success allowances

                    _ ->
                        Loading
           )


update : Tokens -> Value -> Allowances -> Allowances
update tokens value (Allowances allowances) =
    value
        |> Decode.decodeValue (decoder tokens)
        |> (\result ->
                case result of
                    Ok (Allowances dict) ->
                        allowances
                            |> Dict.insertAll dict
                            |> Allowances

                    _ ->
                        Allowances allowances
           )


hasEnough : Token -> String -> Allowances -> Bool
hasEnough token string (Allowances dict) =
    case token of
        Token.ETH ->
            True

        Token.ERC20 erc20 ->
            Maybe.map2 Uint.compare
                (dict |> Dict.get erc20)
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


example : Allowances
example =
    List.map2 Tuple.pair
        [ ERC20.daiRinkeby
        , ERC20.maticRinkeby
        , ERC20.wethRinkeby
        ]
        Uint.example
        |> Dict.fromList ERC20.sorter
        |> Allowances
