module Page.Transaction.Liquidity.Add.Error exposing (Error(..), decoder, toString)

import Json.Decode as Decode exposing (Decoder)


type Error
    = Invalid


decoder : Decoder Error
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


toString : Error -> String
toString error =
    case error of
        Invalid ->
            "Invalid Transaction"
