module Data.Token exposing
    ( Token(..)
    , encode
    , sorter
    , toDecimals
    , toERC20
    , toFragmentAsset
    , toFragmentCollateral
    , toName
    , toString
    , toSymbol
    )

import Data.ERC20 as ERC20 exposing (ERC20)
import Data.Native as Native exposing (Native)
import Json.Encode exposing (Value)
import Sort exposing (Sorter)


type Token
    = Native Native
    | ERC20 ERC20


encode : Token -> Value
encode token =
    case token of
        Native native ->
            native |> Native.encode

        ERC20 erc20 ->
            erc20 |> ERC20.encodeAddress


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


toFragmentAsset : Token -> String
toFragmentAsset token =
    (case token of
        Native native ->
            native
                |> Native.toSymbol

        ERC20 erc20 ->
            erc20
                |> ERC20.toString
    )
        |> (++) "asset="


toFragmentCollateral : Token -> String
toFragmentCollateral token =
    (case token of
        Native native ->
            native
                |> Native.toSymbol

        ERC20 erc20 ->
            erc20
                |> ERC20.toString
    )
        |> (++) "collateral="


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
