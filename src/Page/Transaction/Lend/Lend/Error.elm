module Page.Transaction.Lend.Lend.Error exposing (Error(..), decoder, toString)

import Json.Decode as Decode exposing (Decoder)


type Error
    = BondUnderflow
    | BondOverflow
    | InsuranceUnderflow
    | InsuranceOverflow
    | PrincipalOverflow
    | Invalid


decoder : Decoder Error
decoder =
    Decode.int
        |> Decode.andThen
            (\errCode ->
                case errCode of
                    0 ->
                        Invalid |> Decode.succeed

                    1 ->
                        BondUnderflow |> Decode.succeed

                    2 ->
                        BondOverflow |> Decode.succeed

                    3 ->
                        InsuranceUnderflow |> Decode.succeed

                    4 ->
                        InsuranceOverflow |> Decode.succeed

                    5 ->
                        PrincipalOverflow |> Decode.succeed

                    _ ->
                        Decode.fail "Not an error"
            )


toString : Error -> String
toString error =
    case error of
        BondUnderflow ->
            "Bond to receive too low"

        BondOverflow ->
            "Bond to receive too high"

        InsuranceUnderflow ->
            "Insurance to receive too low"

        InsuranceOverflow ->
            "Insurance to receive too high"

        PrincipalOverflow ->
            "Lend amount too high"

        Invalid ->
            "Invalid Transaction"
