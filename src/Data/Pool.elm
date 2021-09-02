module Data.Pool exposing (Pool, sorter, toFragment, toKey)

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


toKey : Pool -> String
toKey { pair, maturity } =
    [ pair |> Pair.toKey
    , maturity |> Maturity.toKey
    ]
        |> String.join " "


sorter : Sorter Pool
sorter =
    Pair.sorter
        |> Sort.by .pair
        |> Sort.tiebreaker
            (Maturity.sorter |> Sort.by .maturity)
