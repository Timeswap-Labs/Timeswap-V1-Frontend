module Data.Token exposing
    ( Token(..)
    , decoder
    , decoderETH
    , encode
    , sorter
    , toAssetFragment
    , toCollateralFragment
    , toDecimals
    , toKey
    , toName
    , toSymbol
    )

import Data.ERC20 as ERC20 exposing (ERC20)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Sort exposing (Sorter)


type Token
    = ETH
    | ERC20 ERC20


decoder : Int -> Decoder Token
decoder id =
    ERC20.decoder id
        |> Decode.map ERC20


decoderETH : Decoder Token
decoderETH =
    Decode.string
        |> Decode.andThen
            (\string ->
                if string == "ETH" then
                    Decode.succeed ETH

                else
                    Decode.fail "Not an ETH"
            )


encode : Token -> Value
encode token =
    case token of
        ETH ->
            Encode.string "ETH"

        ERC20 erc20 ->
            ERC20.encode erc20


toKey : Token -> String
toKey token =
    case token of
        ETH ->
            "ETH"

        ERC20 erc20 ->
            erc20 |> ERC20.toKey


toName : Token -> String
toName token =
    case token of
        ETH ->
            "Ethereum"

        ERC20 erc20 ->
            erc20 |> ERC20.toName


toSymbol : Token -> String
toSymbol token =
    case token of
        ETH ->
            "ETH"

        ERC20 erc20 ->
            erc20 |> ERC20.toSymbol


toDecimals : Token -> Int
toDecimals token =
    case token of
        ETH ->
            18

        ERC20 erc20 ->
            erc20 |> ERC20.toDecimals


toAssetFragment : Token -> String
toAssetFragment token =
    case token of
        ETH ->
            "asset=ETH"

        ERC20 erc20 ->
            erc20
                |> ERC20.toString
                |> (++) "asset="


toCollateralFragment : Token -> String
toCollateralFragment token =
    case token of
        ETH ->
            "collateral=ETH"

        ERC20 erc20 ->
            erc20
                |> ERC20.toString
                |> (++) "collateral="


compare : Token -> Token -> Order
compare token1 token2 =
    case ( token1, token2 ) of
        ( ETH, ETH ) ->
            EQ

        ( ETH, ERC20 _ ) ->
            LT

        ( ERC20 _, ETH ) ->
            GT

        ( ERC20 erc20a, ERC20 erc20b ) ->
            ERC20.compare erc20a erc20b


sorter : Sorter Token
sorter =
    Sort.custom compare
