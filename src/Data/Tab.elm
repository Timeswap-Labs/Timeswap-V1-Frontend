module Data.Tab exposing (Tab(..), toString)


type Tab
    = Lend
    | Borrow
    | Liquidity
    | Info


toString : Tab -> String
toString tab =
    case tab of
        Lend ->
            "Lend"

        Borrow ->
            "Borrow"

        Liquidity ->
            "Liquidity"

        Info ->
            "Info"
