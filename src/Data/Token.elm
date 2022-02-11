module Data.Token exposing
    ( Token(..)
    , containsString
    , decoder
    , encode
    , sorter
    , toApiTokenId
    , toDecimals
    , toERC20
    , toName
    , toQueryParameter
    , toString
    , toSymbol
    )

import Data.ERC20 as ERC20 exposing (ERC20)
import Data.Native as Native exposing (Native)
import Data.TokenParam as TokenParam exposing (TokenParam)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode exposing (Value)
import Sort exposing (Sorter)
import Url.Builder exposing (QueryParameter)


type Token
    = Native Native
    | ERC20 ERC20


decoder : Decoder Token
decoder =
    [ ERC20.decoder |> Decode.map ERC20
    , Native.decoder |> Decode.map Native
    ]
        |> Decode.oneOf


encode : Token -> Value
encode token =
    case token of
        Native native ->
            native |> Native.encode

        ERC20 erc20 ->
            erc20 |> ERC20.encode


toString : Token -> String
toString token =
    case token of
        Native native ->
            native |> Native.toSymbol

        ERC20 erc20 ->
            erc20 |> ERC20.toString


toName : Token -> String
toName token =
    case token of
        Native native ->
            native |> Native.toName

        ERC20 erc20 ->
            erc20 |> ERC20.toName


toSymbol : Token -> String
toSymbol token =
    case token of
        Native native ->
            native |> Native.toSymbol

        ERC20 erc20 ->
            erc20 |> ERC20.toSymbol


toDecimals : Token -> Int
toDecimals token =
    case token of
        Native native ->
            native |> Native.toDecimals

        ERC20 erc20 ->
            erc20 |> ERC20.toDecimals


toQueryParameter : TokenParam -> Token -> QueryParameter
toQueryParameter tokenParam token =
    (case token of
        Native native ->
            native
                |> Native.toSymbol

        ERC20 erc20 ->
            erc20
                |> ERC20.toString
    )
        |> (tokenParam |> TokenParam.toQueryParameter)


toERC20 : Token -> Maybe ERC20
toERC20 token =
    case token of
        ERC20 erc20 ->
            Just erc20

        Native _ ->
            Nothing


compare : Token -> Token -> Order
compare token1 token2 =
    case ( token1, token2 ) of
        ( Native _, Native _ ) ->
            EQ

        ( Native _, ERC20 _ ) ->
            LT

        ( ERC20 _, Native _ ) ->
            GT

        ( ERC20 erc20a, ERC20 erc20b ) ->
            ERC20.compare erc20a erc20b


sorter : Sorter Token
sorter =
    Sort.custom compare


containsString : Token -> String -> Bool
containsString token input =
    case token of
        ERC20 erc20 ->
            (erc20
                |> ERC20.toName
                |> String.toLower
                |> String.contains (input |> String.toLower)
            )
                || (erc20
                        |> ERC20.toSymbol
                        |> String.toLower
                        |> String.contains (input |> String.toLower)
                   )

        Native native ->
            (native
                |> Native.toName
                |> String.toLower
                |> String.contains (input |> String.toLower)
            )
                || (native
                        |> Native.toSymbol
                        |> String.toLower
                        |> String.contains (input |> String.toLower)
                   )


toApiTokenId : Token -> String
toApiTokenId token =
    if (token |> toSymbol) == "MATIC" then
        "matic-network"

    else if (token |> toSymbol) == "AVAX" then
        "avalanche"

    else if (token |> toSymbol) == "DAI" then
        "dai"

    else if (token |> toSymbol) == "ETH" then
        "ethereum"

    else
        ""
