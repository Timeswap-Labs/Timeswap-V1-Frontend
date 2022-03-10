module Data.Wallet exposing
    ( Flag
    , Wallet(..)
    , decoder
    , encode
    , init
    , sorter
    , toString
    , toUrlString
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Sort exposing (Sorter)
import Url.Builder as Builder


type Wallet
    = Metamask
    | WalletConnect


type alias Flag =
    String


init : Flag -> Maybe Wallet
init string =
    case string of
        "metamask" ->
            Just Metamask

        "walletConnect" ->
            Just WalletConnect

        _ ->
            Nothing


toString : Wallet -> String
toString wallet =
    case wallet of
        Metamask ->
            "Metamask"

        WalletConnect ->
            "WalletConnect"


toUrlString : Wallet -> String
toUrlString wallet =
    case wallet of
        Metamask ->
            Builder.crossOrigin "https://metamask.io" [] []

        WalletConnect ->
            Builder.crossOrigin "https://walletconnect.com/" [] []


decoder : Decoder Wallet
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "metamask" ->
                        Decode.succeed Metamask

                    "walletConnect" ->
                        Decode.succeed WalletConnect

                    _ ->
                        Decode.fail "Not a supported wallet"
            )


encode : Wallet -> Value
encode wallet =
    (case wallet of
        Metamask ->
            "metamask"

        WalletConnect ->
            "walletConnect"
    )
        |> Encode.string


sorter : Sorter Wallet
sorter =
    Sort.increasing
        |> Sort.by
            (\wallet ->
                case wallet of
                    Metamask ->
                        1

                    WalletConnect ->
                        2
            )
