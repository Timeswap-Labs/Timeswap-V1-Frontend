module Data.Tokens exposing
    ( Tokens
    , decoder
    , decoderERC20
    , decoderToken
    , fromAssetFragment
    , fromCollateralFragment
    , getToken
    )

import Data.Address as Address exposing (Address)
import Data.ERC20 as ERC20 exposing (ERC20)
import Data.Token as Token exposing (Token)
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Decode.Pipeline as Pipeline
import Sort.Set as Set exposing (Set)


type alias Tokens =
    Set Token


getToken : Address -> Tokens -> Maybe Token
getToken address tokens =
    tokens
        |> Set.foldl
            (\token accumulator ->
                case token of
                    Token.ERC20 erc20 ->
                        if (erc20 |> ERC20.toAddress) == address then
                            Just token

                        else
                            accumulator

                    Token.ETH ->
                        accumulator
            )
            Nothing


decoder : Decoder Tokens
decoder =
    let
        recursive : Int -> Value -> ( Int, List Token ) -> Decoder (List Token)
        recursive length value ( id, list ) =
            if id < length then
                value
                    |> Decode.decodeValue (Decode.index id (Token.decoder id))
                    |> (\result ->
                            case result of
                                Ok erc20 ->
                                    ( id + 1, erc20 :: list )
                                        |> recursive length value

                                Err error ->
                                    error
                                        |> Decode.errorToString
                                        |> Decode.fail
                       )

            else
                Decode.succeed list
    in
    Decode.succeed (\length value -> recursive length value ( 0, [] ))
        |> Pipeline.custom
            (Decode.succeed ()
                |> Decode.list
                |> Decode.map List.length
            )
        |> Pipeline.custom Decode.value
        |> Decode.andThen identity
        |> Decode.map (Set.fromList Token.sorter)
        |> Decode.map (Set.insert Token.ETH)


decoderToken : Tokens -> Decoder Token
decoderToken tokens =
    Decode.oneOf
        [ Token.decoderETH
        , decoderERC20 tokens |> Decode.map Token.ERC20
        ]


decoderERC20 : Tokens -> Decoder ERC20
decoderERC20 tokens =
    Address.decoder
        |> Decode.andThen
            (\address ->
                tokens
                    |> Set.foldl
                        (\token accumulator ->
                            case token of
                                Token.ETH ->
                                    accumulator

                                Token.ERC20 erc20 ->
                                    if (erc20 |> ERC20.toAddress) == address then
                                        Decode.succeed erc20

                                    else
                                        accumulator
                        )
                        (Decode.fail "not in whitelist")
            )


fromAssetFragment : Tokens -> String -> Maybe Token
fromAssetFragment tokens string =
    string
        |> String.split "="
        |> (\list ->
                case list of
                    "asset" :: "ETH" :: _ ->
                        Just Token.ETH

                    "asset" :: address :: _ ->
                        tokens
                            |> Set.foldl
                                (\token accumulator ->
                                    case token of
                                        Token.ETH ->
                                            accumulator

                                        Token.ERC20 erc20 ->
                                            accumulator
                                                |> Set.insert erc20
                                )
                                (Set.empty ERC20.sorter)
                            |> (\set ->
                                    address
                                        |> ERC20.fromString set
                                        |> Maybe.map Token.ERC20
                               )

                    _ ->
                        Nothing
           )


fromCollateralFragment : Tokens -> String -> Maybe Token
fromCollateralFragment tokens string =
    string
        |> String.split "="
        |> (\list ->
                case list of
                    "collateral" :: "ETH" :: _ ->
                        Just Token.ETH

                    "collateral" :: address :: _ ->
                        tokens
                            |> Set.foldl
                                (\token accumulator ->
                                    case token of
                                        Token.ETH ->
                                            accumulator

                                        Token.ERC20 erc20 ->
                                            accumulator
                                                |> Set.insert erc20
                                )
                                (Set.empty ERC20.sorter)
                            |> (\set ->
                                    address
                                        |> ERC20.fromString set
                                        |> Maybe.map Token.ERC20
                               )

                    _ ->
                        Nothing
           )
