module Data.Chain exposing
    ( Chain(..)
    , encode
    , sorter
    , toChainId
    , toEtherscan
    , toQueryParameter
    , toString
    )

import Json.Encode as Encode exposing (Value)
import Sort exposing (Sorter)
import Url.Builder as Builder exposing (QueryParameter)


type Chain
    = Chain
        { chainId : Int
        , name : String
        , etherscan : String
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


toString : Chain -> String
toString (Chain { name }) =
    name


toEtherscan : Chain -> String
toEtherscan (Chain { etherscan }) =
    etherscan


toQueryParameter : Chain -> QueryParameter
toQueryParameter chain =
    chain
        |> toChainId
        |> Builder.int "chainId"
