module Modal.MaturityList.Sorting exposing (Sorting(..), toString)


type Sorting
    = Liquidity
    | Maturity


toString : Sorting -> String
toString sorting =
    case sorting of
        Liquidity ->
            "Liquidity"

        Maturity ->
            "Maturity"
