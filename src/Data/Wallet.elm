module Data.Wallet exposing
    ( Flag
    , Wallet(..)
    , decoder
    , encode
    , init
    , sorter
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Sort exposing (Sorter)


type Wallet
    = Metamask


type alias Flag =
    String


init : Flag -> Maybe Wallet
init string =
    case string of
        "metamask" ->
            Just Metamask

        _ ->
            Nothing


decoder : Decoder Wallet
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "metamask" ->
                        Decode.succeed Metamask

                    _ ->
                        Decode.fail "Not a supported wallet"
            )


encode : Wallet -> Value
encode wallet =
    (case wallet of
        Metamask ->
            "metamask"
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
            )
