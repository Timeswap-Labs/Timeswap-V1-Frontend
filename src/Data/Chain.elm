module Data.Chain exposing
    ( Chain(..)
    , encode
    , sorter
    , toChainId
    )

import Json.Encode as Encode exposing (Value)
import Sort exposing (Sorter)


type Chain
    = Chain
        { chainId : Int
        , name : String
        }


encode : Chain -> Value
encode (Chain { chainId }) =
    chainId
        |> Encode.int


sorter : Sorter Chain
sorter =
    Sort.increasing
        |> Sort.by (\(Chain { chainId }) -> chainId)


toChainId : Chain -> Int
toChainId (Chain { chainId }) =
    chainId
