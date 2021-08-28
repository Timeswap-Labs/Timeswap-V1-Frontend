module Data.Chain exposing (Chain(..), decoder)

import Json.Decode as Decode exposing (Decoder)


type Chain
    = Mainnet
    | Rinkeby


decoder : Decoder Chain
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "0x1" ->
                        Decode.succeed Mainnet

                    "0x4" ->
                        Decode.succeed Rinkeby

                    _ ->
                        Decode.fail "Not a whitelisted chain"
            )
