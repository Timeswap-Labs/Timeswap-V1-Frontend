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


type alias Flag =
    String


init : Flag -> Maybe Wallet
init string =
    case string of
        "metamask" ->
            Just Metamask

        _ ->
            Nothing


toString : Wallet -> String
toString wallet =
    case wallet of
        Metamask ->
            "Metamask"


toUrlString : Wallet -> String
toUrlString wallet =
    case wallet of
        Metamask ->
            Builder.crossOrigin "https://metamask.io" [] []


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
