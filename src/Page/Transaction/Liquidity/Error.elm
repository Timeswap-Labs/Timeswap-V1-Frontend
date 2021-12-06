module Page.Transaction.Liquidity.Error exposing (CreateError, TransactionError, decoder, decoderCreate)

import Json.Decode as Decode exposing (Decoder)


type Error other
    = Invalid


type OnlyTransaction
    = TransactionError


type OnlyCreate
    = CreateError


type alias TransactionError =
    Error OnlyTransaction


type alias CreateError =
    Error OnlyCreate


decoder : Decoder TransactionError
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "invalid" ->
                        Invalid |> Decode.succeed

                    _ ->
                        Decode.fail "Not an error"
            )


decoderCreate : Decoder CreateError
decoderCreate =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "invalid" ->
                        Invalid |> Decode.succeed

                    _ ->
                        Decode.fail "Not an error"
            )
