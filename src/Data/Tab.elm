module Data.Tab exposing (Tab(..), toString)


type Tab
    = Lend
    | Borrow
    | Liquidity


toString : Tab -> String
toString tab =
    case tab of
        Lend ->
            "Lend"

        Borrow ->
            "Borrow"

        Liquidity ->
            "Liquidity"
