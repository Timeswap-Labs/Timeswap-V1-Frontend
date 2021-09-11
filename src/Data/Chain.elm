module Data.Chain exposing
    ( Chain(..)
    , Error
    , decoder
    )

import Json.Decode as Decode exposing (Decoder)


type Chain
    = Mainnet
    | Rinkeby


type Error
    = UnsupportedNetwork


decoder : Decoder (Result Error Chain)
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "0x4" ->
                        Ok Rinkeby
                            |> Decode.succeed

                    _ ->
                        Err UnsupportedNetwork
                            |> Decode.succeed
            )
