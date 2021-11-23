module Data.Pool exposing (Pool, sorter, toFragment, toString)

import Data.Maturity as Maturity exposing (Maturity)
import Data.Pair as Pair exposing (Pair)
import Sort exposing (Sorter)


type alias Pool =
    { pair : Pair
    , maturity : Maturity
    }


toFragment : Pool -> String
toFragment { pair, maturity } =
    [ pair |> Pair.toFragment
    , maturity |> Maturity.toFragment
    ]
        |> String.join "&"


toString : Pool -> String
toString { pair, maturity } =
    [ pair |> Pair.toString
    , maturity |> Maturity.toUnix
    ]
        |> String.join " "


sorter : Sorter Pool
sorter =
    Pair.sorter
        |> Sort.by .pair
        |> Sort.tiebreaker
            (Maturity.sorter |> Sort.by .maturity)
