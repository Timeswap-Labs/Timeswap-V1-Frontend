module Page.Transaction.Borrow.Borrow.Error exposing (Error, decoder, toString)

import Json.Decode as Decode exposing (Decoder)


type Error
    = PrincipalOverflow
    | DebtUnderflow
    | DebtOverflow
    | CollateralUnderflow
    | CollateralOverflow
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
                        DebtUnderflow |> Decode.succeed

                    2 ->
                        DebtOverflow |> Decode.succeed

                    3 ->
                        CollateralUnderflow |> Decode.succeed

                    4 ->
                        CollateralOverflow |> Decode.succeed

                    5 ->
                        PrincipalOverflow |> Decode.succeed

                    _ ->
                        Decode.fail "Not an error"
            )


toString : Error -> String
toString error =
    case error of
        DebtUnderflow ->
            "Debt to receive too low"

        DebtOverflow ->
            "Debt to receive too high"

        CollateralUnderflow ->
            "Collateral to lock too low"

        CollateralOverflow ->
            "Collateral to lock too high"

        PrincipalOverflow ->
            "Borrow amount too high"

        Invalid ->
            "Invalid Transaction"
